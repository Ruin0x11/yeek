use full_moon::ast;
use full_moon::ast::punctuated::{Punctuated, Pair};
use full_moon::ast::owned::Owned;
use full_moon::tokenizer::{Position, Token, TokenReference, TokenType};
use full_moon::visitors::VisitorMut;
use full_moon::node::Node;

use anyhow::{anyhow, Result};
use walkdir::WalkDir;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use id_arena::Id;
use indicatif::ProgressBar;
use rayon::prelude::*;
use crate::ast_util::scopes::{Variable, ScopeManager};
use crate::detect;
use crate::util;

#[derive(Debug, Clone, Copy)]
pub enum FnKind {
    Function,
    Method
}

pub fn is_module_fn_decl(name: &ast::FunctionName, module_name: &str, fn_name: &str) -> Option<FnKind> {
    let method_name = name.method_name();
    let is_method = method_name.is_some();

    if let Some(method_name) = method_name {
        if method_name.token().to_string() != fn_name {
            return None
        }
    }

    let names = name.names();

    if is_method {
        if names.len() != 1 {
            return None
        }

        let mut iter = names.iter();
        let mod_name = iter.next().unwrap();

        if mod_name.token().to_string() == module_name {
            return Some(FnKind::Method)
        }
    } else {
        if names.len() != 2 {
            return None
        }

        let mut iter = names.iter();
        let mod_name = iter.next().unwrap();
        let name = iter.next().unwrap();

        if mod_name.token().to_string() == module_name && name.token().to_string() == fn_name {
            return Some(FnKind::Function)
        }
    }

    None
}

fn find_fn_def_in_module_file<'a, 'b>(ast: &'a ast::Ast<'b>, module_name: &str, fn_name: &str) -> Option<(&'a ast::FunctionDeclaration<'b>, FnKind)> {
    for stmt in ast.nodes().iter_stmts() {
        if let ast::Stmt::FunctionDeclaration(decl) = stmt {
            if let Some(kind) = is_module_fn_decl(decl.name(), module_name, fn_name) {
                return Some((decl, kind))
            }
        }
    }

    None
}

struct RenameFnDefVisitor {
    module_name: String,
    fn_name: String,
    new_name: String,
}

pub fn make_new_fn_name<'ast>(module_name: String, fn_name: String, fn_kind: FnKind) -> ast::FunctionName<'ast> {
    let mut names = Punctuated::new();

    let sym = match fn_kind {
        FnKind::Function => ".",
        FnKind::Method => ":",
    };

    let sym_tok = Cow::Owned(TokenReference::symbol(sym).unwrap());

    let module_tok = Token::new(TokenType::Identifier { identifier: Cow::from(module_name) });
    names.push(Pair::Punctuated(Cow::Owned(TokenReference::new(Vec::new(), module_tok, Vec::new())), sym_tok));
    let fn_tok = Token::new(TokenType::Identifier { identifier: Cow::from(fn_name) });
    names.push(Pair::End(Cow::Owned(TokenReference::new(Vec::new(), fn_tok, Vec::new()))));

    ast::FunctionName::new(names)
}

impl VisitorMut<'_> for RenameFnDefVisitor {
    fn visit_function_declaration<'ast>(&mut self, decl: ast::FunctionDeclaration<'ast>) -> ast::FunctionDeclaration<'ast> {
        if let Some(fn_kind) = is_module_fn_decl(&decl.name(), &self.module_name, &self.fn_name) {
            let name = make_new_fn_name(self.module_name.clone(), self.new_name.clone(), fn_kind);
            decl.with_name(name)
        } else {
            decl
        }
    }
}

pub type Range = (Position, Position);

#[derive(Debug)]
pub struct Warning {
    pub filepath: PathBuf,
    pub range: Option<Range>,
    pub message: String,
}

impl Warning {
    pub fn new(filepath: PathBuf, node: &dyn Node, message: String) -> Self {
        Warning {
            filepath: filepath,
            range: node.range(),
            message: message
        }
    }

    pub fn new_no_pos(filepath: PathBuf, message: String) -> Self {
        Warning {
            filepath: filepath,
            range: None,
            message: message
        }
    }
}

impl fmt::Display for Warning {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let range = match self.range {
            None => "".into(),
            Some((start, _end)) => format!(":{}:{}", start.line(), start.character())
        };
        format!("{}{}: {}", self.filepath.to_str().unwrap(), range, self.message).fmt(formatter)
    }
}

pub type TokenCow<'a> = Cow<'a, TokenReference<'a>>;

pub fn make_new_token<'a>(old_tok: &TokenCow<'a>, new_name: String) -> TokenCow<'a> {
    let name_tok = Token::new(TokenType::Identifier { identifier: Cow::from(new_name) });
    Cow::Owned(TokenReference::new(old_tok.leading_trivia().cloned().collect(),
                                   name_tok,
                                   old_tok.trailing_trivia().cloned().collect()))
}

fn make_new_funcall<'a>(old_tok: &TokenCow<'a>, new_name: String, funcall: ast::FunctionCall<'a>, dot: TokenCow<'a>) -> ast::FunctionCall<'a> {
    let new_name = make_new_token(old_tok, new_name);
    let new_dot = ast::Index::Dot { dot: dot, name: new_name };
    let new_suffix = ast::Suffix::Index(new_dot);
    let mut new_suffixes = funcall.iter_suffixes().cloned().collect::<Vec<ast::Suffix>>();
    new_suffixes[0] = new_suffix;
    funcall.with_suffixes(new_suffixes)
}

fn make_new_var_expr<'a>(old_tok: &TokenCow<'a>, new_name: String, var_expr: ast::VarExpression<'a>, dot: TokenCow<'a>) -> ast::Var<'a> {
    let new_name = make_new_token(old_tok, new_name);
    let new_dot = ast::Index::Dot { dot: dot, name: new_name };
    let new_suffix = ast::Suffix::Index(new_dot);
    let mut new_suffixes = var_expr.iter_suffixes().cloned().collect::<Vec<ast::Suffix>>();
    new_suffixes[0] = new_suffix;
    let new_var_expr = var_expr.with_suffixes(new_suffixes);
    ast::Var::Expression(new_var_expr)
}

pub fn extract_require_path<'ast>(expr: &ast::Expression<'ast>) -> Option<String> {
    if let ast::Expression::Value { value: ref val, .. } = expr {
        if let ast::Value::FunctionCall(funcall) = &**val {
            if let ast::Prefix::Name(name) = funcall.prefix() {
                if name.token().to_string() == util::REQUIRE_FN_NAME {
                    let mut suffixes = funcall.iter_suffixes();

                    let first_suffix = suffixes.next();
                    if let Some(ast::Suffix::Call(ast::Call::AnonymousCall(args))) = first_suffix {
                        if let ast::FunctionArgs::Parentheses { arguments, .. } = args {
                            let mut iter = arguments.iter();

                            let first_arg = iter.next();
                            if let Some(ast::Expression::Value { value: val, .. }) = first_arg {
                                if let ast::Value::String(s) = &**val {
                                    let raw = s.token().to_string();
                                    return Some(util::normalize_require_path(util::strip_quotes(raw)));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

struct RenameFnCallVisitor {
    filepath: PathBuf,
    own_require_path: String,
    require_path: String,
    module_name: String,
    fn_name: String,
    new_name: String,
    scopes: ScopeManager,
    referenced_variables: HashSet<Id<Variable>>,
    renamed_count: u32,
    warnings: Vec<Warning>
}

impl RenameFnCallVisitor {
    fn warn(&mut self, node: &dyn Node, message: String) {
        self.warnings.push(Warning::new(self.filepath.clone(), node, message));
    }
}

impl VisitorMut<'_> for RenameFnCallVisitor {
    fn visit_local_assignment<'ast>(&mut self, assign: ast::LocalAssignment<'ast>) -> ast::LocalAssignment<'ast> {
        for (name, expr) in assign.name_list().iter().zip(assign.expr_list().iter()) {
            if name.token().to_string() == self.module_name {
                let req_path_opt = extract_require_path(&expr);
                let mut proceed = req_path_opt.map(|r| r == self.require_path).unwrap_or(false);

                if !proceed {
                    proceed = self.own_require_path == self.require_path
                        && detect::detect_module_declaration_in_expr(&self.module_name, expr).is_some()
                }

                if proceed {
                    if let Some(pos) = name.start_position() {
                        let byte_pos = pos.bytes();
                        if let Some(reference) = self.scopes.reference_at_byte(byte_pos) {
                            if let Some(resolved_id) = reference.resolved {
                                self.referenced_variables.insert(resolved_id);
                            }
                            else {
                                self.warn(&assign, format!("could not find variable for module"));
                            }
                        }
                        else {
                            self.warn(&assign, format!("could not find reference for module"));
                        }
                    }
                }
            }
        }
        assign
    }

    fn visit_var<'ast>(&mut self, var: ast::Var<'ast>) -> ast::Var<'ast> {
        if let ast::Var::Expression(ref var_expr) = var {
            if let ast::Prefix::Name(module_name) = var_expr.prefix() {
                if module_name.token().to_string() == self.module_name {
                    let mut suffixes = var_expr.iter_suffixes();
                    let first_suffix = suffixes.next();
                    if let Some(ast::Suffix::Index(ast::Index::Dot { dot, name: fn_name })) = first_suffix {
                        if fn_name.token().to_string() == self.fn_name {
                            if let Some(pos) = module_name.start_position() {
                                let byte_pos = pos.bytes();
                                if let Some(reference) = self.scopes.reference_at_byte(byte_pos) {
                                    if let Some(resolved_id) = reference.resolved {
                                        if self.referenced_variables.contains(&resolved_id) {
                                            self.renamed_count += 1;
                                            return make_new_var_expr(fn_name, self.new_name.clone(), var_expr.clone(), dot.clone());
                                        }
                                        else {
                                            self.warn(&var, format!("could not find associated require for funcall stmt"));
                                        }
                                    }
                                    else {
                                        self.warn(&var, format!("could not find variable for funcall stmt"));
                                    }
                                }
                                else {
                                    self.warn(&var, format!("could not find reference for funcall stmt"));
                                }
                            }
                        }
                    }
                }
            }
        }
        var
    }

    fn visit_function_call<'ast>(&mut self, funcall: ast::FunctionCall<'ast>) -> ast::FunctionCall<'ast> {
        if let ast::Prefix::Name(module_name) = funcall.prefix() {
            if module_name.token().to_string() == self.module_name {
                let mut suffixes = funcall.iter_suffixes();
                let first_suffix = suffixes.next();
                if let Some(ast::Suffix::Index(ast::Index::Dot { dot, name: fn_name })) = first_suffix {
                    if fn_name.token().to_string() == self.fn_name {
                        if let Some(pos) = module_name.start_position() {
                            let byte_pos = pos.bytes();
                            if let Some(reference) = self.scopes.reference_at_byte(byte_pos) {
                                if let Some(resolved_id) = reference.resolved {
                                    if self.referenced_variables.contains(&resolved_id) {
                                        self.renamed_count += 1;
                                        return make_new_funcall(fn_name, self.new_name.clone(), funcall.clone(), dot.clone());
                                    }
                                    else {
                                        self.warn(&funcall, format!("could not find associated require for funcall stmt"));
                                    }
                                }
                                else {
                                    self.warn(&funcall, format!("could not find variable for funcall stmt"));
                                }
                            }
                            else {
                                self.warn(&funcall, format!("could not find reference for funcall stmt"));
                            }
                        }
                    }
                }
            }
        }
        funcall
    }
}

#[derive(Debug)]
pub struct RenameResult<'a> {
    pub filepath: PathBuf,
    pub new_ast: Option<ast::Ast<'a>>,
    pub renamed_count: u32,
    pub warnings: Vec<Warning>
}

pub fn rename_fn_calls_in_file<'a>(root: &Path, path: &Path, ast: ast::Ast<'_>, require_path: &str, module_name: &str, fn_name: &str, new_name: &str) -> Result<RenameResult<'static>> {
    let mut visitor = RenameFnCallVisitor {
        filepath: PathBuf::from(path),
        own_require_path: util::path_to_require_path(path, root).unwrap(),
        require_path: require_path.to_string(),
        module_name: module_name.to_string(),
        fn_name: fn_name.to_string(),
        new_name: new_name.to_string(),
        scopes: ScopeManager::new(&ast),
        referenced_variables: HashSet::new(),
        renamed_count: 0,
        warnings: Vec::new()
    };

    let new_ast = visitor.visit_ast(ast);

    Ok(RenameResult { filepath: PathBuf::from(path), new_ast: Some(new_ast.owned()), renamed_count: visitor.renamed_count, warnings: visitor.warnings })
}

pub fn rename_fn_def<'a>(path: &Path, ast: ast::Ast<'a>, fn_name: &str, new_name: &str) -> Result<ast::Ast<'a>> {
    let module_name = util::path_to_module_name(path);

    if find_fn_def_in_module_file(&ast, &module_name, &fn_name).is_none() {
        return Err(anyhow!("Could not find definition in file"))
    }

    let mut rename_fn_visitor = RenameFnDefVisitor {
        module_name: module_name.clone(),
        fn_name: fn_name.to_string(),
        new_name: new_name.to_string()
    };

    let new_ast = rename_fn_visitor.visit_ast(ast);
    Ok(new_ast)
}

pub fn rename_function(root: &Path, path: &Path, fn_name: &str, new_name: &str) -> Result<Vec<RenameResult<'static>>> {
    let it = WalkDir::new(root).follow_links(true).into_iter().filter_entry(util::is_lua_file).filter_map(|e| e.ok());
    let entries = it.collect::<Vec<_>>();

    let pb = ProgressBar::new(entries.len() as u64);

    let module_name = util::path_to_module_name(path);
    let require_path = util::path_to_require_path(path, root).unwrap();

    let process = |entry: &walkdir::DirEntry| -> Result<Option<RenameResult<'static>>> {
        let result = if entry.file_type().is_file() {
            let source = fs::read_to_string(entry.path())?;
            let mut ast: ast::Ast<'_> = full_moon::parse(&source).map_err(|e| anyhow!(format!("{}", e)))?;

            if entry.path() == path {
                ast = rename_fn_def(&path, ast, &fn_name, &new_name)?;
            }

            match rename_fn_calls_in_file(&root, entry.path(), ast, &require_path, &module_name, &fn_name, &new_name) {
                Err(err) => {
                    Ok(Some(RenameResult {
                        filepath: entry.path().into(),
                        new_ast: None,
                        renamed_count: 0,
                        warnings: vec![Warning::new_no_pos(entry.path().into(), format!("Could not parse: {}", err))]
                    }))
                },
                Ok(r) => Ok(Some(r))
            }
        } else {
            Ok(None)
        };
        pb.inc(1);
        result
    };

    let results = entries.par_iter().map(process)
                                    .filter_map(|i| i.ok())
                                    .fold(|| Vec::new(), |mut acc, i| {
                                        if let Some(item) = i {
                                            acc.push(item);
                                        }
                                        acc
                                    }).reduce(|| Vec::new(), |mut acc, mut i| {
                                        acc.append(&mut i); acc
                                    });

    pb.finish_with_message("");

    Ok(results)
}

#[cfg(test)]
mod tests {
    use crate::refactor::*;
    use full_moon;
    use std::path::PathBuf;

    fn assert_rename_def<'a>(path: &str,
                             fn_name: &str,
                             new_name: &str,
                             before: &str,
                             after: &str) {
        let ast = full_moon::parse(before).unwrap();
        let new_ast = rename_fn_def(&PathBuf::from(path), ast, &fn_name, &new_name).unwrap();
        assert_eq!(after, full_moon::print(&new_ast));
    }

    fn assert_rename_calls<'a>(path: &str,
                               require_path: &str,
                               module_name: &str,
                               fn_name: &str,
                               new_name: &str,
                               before: &str,
                               after: &str) {
        let ast = full_moon::parse(before).unwrap();
        let result = rename_fn_calls_in_file(&PathBuf::from(""), &PathBuf::from(path), ast, &require_path, &module_name, &fn_name, &new_name).unwrap();
        assert_eq!(after, full_moon::print(&result.new_ast.unwrap()));
    }

    #[test]
    fn rename_in_function() {
        assert_rename_calls("api/Yeek.lua", "api.Rand", "Rand", "rnd", "asdfg",
                            r#"
local Rand = require("api.Rand")

local Yeek = {}

function Yeek.test()
   local a = Rand.rnd(5)
   local b = Rand.between(10, 20)
   local c = Rand.rnd(30)
   return a + b + c
end

return Yeek
"#,
                            r#"
local Rand = require("api.Rand")

local Yeek = {}

function Yeek.test()
   local a = Rand.asdfg(5)
   local b = Rand.between(10, 20)
   local c = Rand.asdfg(30)
   return a + b + c
end

return Yeek
"#
        );
    }

    #[test]
    fn rename_in_method() {
        assert_rename_calls("api/Yeek.lua", "api.Rand", "Rand", "rnd", "asdfg",
                            r#"
local Rand = require("api.Rand")

local Yeek = class.class("Yeek")

function Yeek:test()
   return Rand.rnd(5)
end

function Yeek.static_test()
   return Rand.rnd(5)
end

return Yeek
"#,
                            r#"
local Rand = require("api.Rand")

local Yeek = class.class("Yeek")

function Yeek:test()
   return Rand.asdfg(5)
end

function Yeek.static_test()
   return Rand.asdfg(5)
end

return Yeek
"#
        );
    }

    #[test]
    fn rename_function_def() {
        assert_rename_def("api/Rand.lua", "rnd", "asdfg",
                          r#"
local Rand = {}

function Rand.rnd(n)
    return rng:rnd(n)
end

function Rand.rnd_float()
   return rng:rnd_float()
end

function Rand.one_in(n)
   return Rand.rnd(n) == 0
end

return Rand
"#,
                          r#"
local Rand = {}

function Rand.asdfg(n)
    return rng:rnd(n)
end

function Rand.rnd_float()
   return rng:rnd_float()
end

function Rand.one_in(n)
   return Rand.rnd(n) == 0
end

return Rand
"#
        );
    }

    #[test]
    fn rename_bare_assign() {
        assert_rename_calls("api/Test.lua", "api.Rand", "Rand", "rnd", "asdfg",
                            r#"
local Rand = require("api.Rand")

local Test = {}

function Test.test()
   local env = { Rand.rnd }
   env["rnd"] = Rand.rnd
   return env
end

rnd.rnd = Rand.rnd

return Test
"#,
                            r#"
local Rand = require("api.Rand")

local Test = {}

function Test.test()
   local env = { Rand.asdfg }
   env["rnd"] = Rand.asdfg
   return env
end

rnd.rnd = Rand.asdfg

return Test
"#,

        );
    }

    #[test]
    fn rename_bare_assign_alias() {
        assert_rename_calls("api/Rand.lua", "api.Rand", "Rand", "rnd", "asdfg",
                            r#"
local Rand = {}

Rand.rnd = rnd.rnd

return Rand
"#,
                            r#"
local Rand = {}

Rand.asdfg = rnd.rnd

return Rand
"#,

        );
    }

    #[test]
    fn rename_preserves_trivia() {
        assert_rename_calls("api/Yeek.lua", "api.Rand", "Rand", "rnd", "asdfg",
                            r#"
local Rand = require("api.Rand")

local Yeek = {}

function Yeek.test()
   -- Yee-eek!
   return Rand.rnd(5) + Rand.rnd(5) -- Yee-eek!
end

return Yeek
"#,
                            r#"
local Rand = require("api.Rand")

local Yeek = {}

function Yeek.test()
   -- Yee-eek!
   return Rand.asdfg(5) + Rand.asdfg(5) -- Yee-eek!
end

return Yeek
"#
        );
    }
}
