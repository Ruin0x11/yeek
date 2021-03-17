use full_moon::ast;
use full_moon::ast::punctuated::{Punctuated, Pair};
use full_moon::ast::owned::Owned;
use full_moon::visitors::VisitorMut;
use full_moon::node::Node;

use anyhow::{anyhow, Result};
use walkdir::WalkDir;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use id_arena::Id;
use indicatif::ProgressBar;
use itertools::{
    Itertools,
    EitherOrBoth::*,
};
use rayon::prelude::*;
use crate::ast_util::scopes::{Variable, ScopeManager};
use crate::detect;
use crate::move_module;
use crate::refactor::{self, RenameResult, TokenCow, Warning};
use crate::util;

#[derive(Debug)]
struct RenameModuleVisitor {
    filepath: PathBuf,
    old_module_name: String,
    new_module_name: String,
    own_require_path: String,
    old_require_path: String,
    new_require_path: String,
    renamed: HashSet<usize>,
    scopes: ScopeManager,
    referenced_variables: HashSet<Id<Variable>>,
    warnings: Vec<Warning>
}

impl RenameModuleVisitor {
    fn warn(&mut self, node: &dyn Node, message: String) {
        self.warnings.push(Warning::new(self.filepath.clone(), node, message));
    }
}

fn make_new_prefix_name<'a>(old_tok: &TokenCow<'a>, new_name: String) -> ast::Prefix<'a> {
    let new_name = refactor::make_new_token(old_tok, new_name);
    ast::Prefix::Name(new_name)
}

fn make_new_var_name<'a>(old_tok: &TokenCow<'a>, new_name: String) -> ast::Var<'a> {
    let new_name = refactor::make_new_token(old_tok, new_name);
    ast::Var::Name(new_name)
}

type NameList<'a> = Punctuated<'a, TokenCow<'a>>;

impl VisitorMut<'_> for RenameModuleVisitor {
    fn visit_local_assignment<'ast>(&mut self, assign: ast::LocalAssignment<'ast>) -> ast::LocalAssignment<'ast> {
        let mut new_names = NameList::new();

        for pair in assign.name_list().pairs().zip_longest(assign.expr_list().pairs()) {
            let (name_pair, expr_pair) = match pair {
                Both(n, e) => (Some(n), Some(e)),
                Left(n) => (Some(n), None),
                Right(e) => (None, Some(e)),
            };

            if let Some(np) = name_pair {
                let name = np.value();
                let mut found = false;
                if name.token().to_string() == self.old_module_name {
                    let mut proceed = false;

                    if let Some(ep) = expr_pair {
                        let expr = ep.value();
                        let req_path_opt = refactor::extract_require_path(&expr);
                        proceed = req_path_opt.map(|r| r == self.old_require_path).unwrap_or(false);

                        if !proceed {
                            proceed = self.own_require_path == self.old_require_path
                                && detect::detect_module_declaration_in_expr(&self.old_module_name, expr).is_some()
                        }
                    }

                    if proceed {
                        if let Some(pos) = name.start_position() {
                            let byte_pos = pos.bytes();
                            if !self.renamed.contains(&byte_pos) {
                                if let Some(reference) = self.scopes.reference_at_byte(byte_pos) {
                                    if let Some(resolved_id) = reference.resolved {
                                        self.referenced_variables.insert(resolved_id);
                                        self.renamed.insert(byte_pos);

                                        let new_name = refactor::make_new_token(name, self.new_module_name.clone());
                                        let new_name_pair = match np.punctuation() {
                                            Some(punc) => Pair::Punctuated(new_name, Cow::Owned(punc.clone())),
                                            None => Pair::End(new_name),
                                        };
                                        new_names.push(new_name_pair);
                                        found = true;
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
                    } else {
                        self.warn(&assign, format!("Found local assignment matching old module name"));
                    } 
                } else if name.token().to_string() == self.new_module_name {
                    self.warn(&assign, format!("Found local assignment matching new module name"));
                }
                if !found {
                    new_names.push(np.clone());
                }
            }
        }

        assign.with_name_list(new_names)
    }

    fn visit_prefix<'ast>(&mut self, prefix: ast::Prefix<'ast>) -> ast::Prefix<'ast> {
        if let ast::Prefix::Name(ref module_name) = prefix {
            let string = module_name.token().to_string();
            if string == self.old_module_name {
                if let Some(pos) = module_name.start_position() {
                    let bytes = pos.bytes();
                    if let Some(reference) = self.scopes.reference_at_byte(bytes) {
                        if let Some(resolved_id) = reference.resolved {
                            if self.referenced_variables.contains(&resolved_id) {
                                self.renamed.insert(bytes);
                                return make_new_prefix_name(module_name, self.new_module_name.clone());
                            }
                        }
                    }
                }
                self.warn(&prefix, format!("Found identifier matching old module name"));
            } else if string == self.new_module_name {
                self.warn(&prefix, format!("Found identifier matching new module name"));
            }
        }
        prefix
    }

    fn visit_var<'ast>(&mut self, var: ast::Var<'ast>) -> ast::Var<'ast> {
        if let ast::Var::Name(ref module_name) = var {
            if module_name.token().to_string() == self.old_module_name {
                if let Some(pos) = module_name.start_position() {
                    let byte_pos = pos.bytes();
                    if let Some(reference) = self.scopes.reference_at_byte(byte_pos) {
                        if let Some(resolved_id) = reference.resolved {
                            if self.referenced_variables.contains(&resolved_id) {
                                self.renamed.insert(byte_pos);
                                return make_new_var_name(module_name, self.new_module_name.clone())
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
        var
    }

    fn visit_value<'ast>(&mut self, val: ast::Value<'ast>) -> ast::Value<'ast> {
        if let ast::Value::String(ref s) = val {
            let byte_pos = s.start_position().unwrap().bytes();
            if !self.renamed.contains(&byte_pos) {
                let raw = s.token().to_string();
                let string = util::strip_quotes(raw);
                if string == self.old_module_name {
                    self.warn(&s, "Found string constant matching old module name".to_string());
                } else if string == self.new_module_name {
                    self.warn(&s, "Found string constant matching new module name".to_string());
                } else if string == self.old_require_path {
                    self.warn(&s, "Found string matching old require path outside require".to_string());
                } else if string == self.new_require_path {
                    self.warn(&s, "Found string matching new require path outside require".to_string());
                }
            }
        }
        val
    }

    fn visit_function_name<'ast>(&mut self, name: ast::FunctionName<'ast>) -> ast::FunctionName<'ast> {
        if let Some(mod_name) = name.names().iter().next() {
            let string = mod_name.token().to_string();
            if string == self.old_module_name {
                if let Some(pos) = mod_name.start_position() {
                    let byte_pos = pos.bytes();
                    if let Some(reference) = self.scopes.reference_at_byte(byte_pos) {
                        if let Some(resolved_id) = reference.resolved {
                            if self.referenced_variables.contains(&resolved_id) {
                                let mut new_names = Punctuated::new();

                                let pair = name.names().pairs().next().unwrap();
                                let module_tok = refactor::make_new_token(pair.value(), self.new_module_name.clone());

                                match pair.punctuation() {
                                    Some(p) => new_names.push(Pair::Punctuated(module_tok, Cow::Owned(p.clone()))),
                                    None => new_names.push(Pair::End(module_tok))
                                }

                                for pair in name.names().pairs().skip(1) {
                                    new_names.push(pair.clone());
                                }

                                return name.clone().with_names(new_names)
                            }
                        }
                    }
                }
                self.warn(&name, "Found function name matching old module name".to_string());
            } else if string == self.new_module_name {
                self.warn(&name, "Found function name matching new module name".to_string());
            }
        }

        name
    }

    fn visit_function_call<'ast>(&mut self, funcall: ast::FunctionCall<'ast>) -> ast::FunctionCall<'ast> {
        if let ast::Prefix::Name(module_name) = funcall.prefix() {
            if module_name.token().to_string() == util::REQUIRE_FN_NAME {
                let mut suffixes = funcall.iter_suffixes();

                let first_suffix = suffixes.next();
                if let Some(ast::Suffix::Call(ast::Call::AnonymousCall(args))) = first_suffix {
                    if let ast::FunctionArgs::Parentheses { arguments, parentheses } = args {
                        let mut iter = arguments.iter();

                        let first_arg = iter.next();
                        if let Some(ast::Expression::Value { value: val, .. }) = first_arg {
                            if let ast::Value::String(s) = &**val {
                                let raw = s.token().to_string();
                                let require_path = util::normalize_require_path(util::strip_quotes(raw));
                                if require_path == self.old_require_path {
                                    let byte_pos = s.start_position().unwrap().bytes();
                                    self.renamed.insert(byte_pos);
                                    let args = move_module::make_new_require_args(&s, self.new_require_path.clone(), &arguments);
                                    let new_args = ast::FunctionArgs::Parentheses { arguments: args, parentheses: parentheses.clone() };
                                    let new_call = ast::Call::AnonymousCall(new_args);
                                    let mut new_suffixes = funcall.iter_suffixes().cloned().collect::<Vec<ast::Suffix>>();
                                    new_suffixes[0] = ast::Suffix::Call(new_call);
                                    return funcall.clone().with_suffixes(new_suffixes)
                                }
                            }
                        }
                    }
                }
            }
        }
        funcall
    }
}

pub fn update_require_paths_and_module_identifiers<'a>(root: &Path, path: &Path, ast: ast::Ast<'_>, old_module_name: &str, new_module_name: &str, old_require_path: &str, new_require_path: &str) -> Result<RenameResult<'static>> {
    let mut visitor = RenameModuleVisitor {
        filepath: PathBuf::from(path),
        old_module_name: old_module_name.to_string(),
        new_module_name: new_module_name.to_string(),
        own_require_path: util::path_to_require_path(path, root).unwrap(),
        old_require_path: old_require_path.to_string(),
        new_require_path: new_require_path.to_string(),
        renamed: HashSet::new(),
        scopes: ScopeManager::new(&ast),
        referenced_variables: HashSet::new(),
        warnings: Vec::new()
    };

    let new_ast = visitor.visit_ast(ast);

    Ok(RenameResult { filepath: PathBuf::from(path), new_ast: Some(new_ast.owned()), renamed_count: visitor.renamed.len() as u32, warnings: visitor.warnings })
}

pub fn rename_module(root: &Path, path: &Path, new_path: &Path) -> Result<Vec<RenameResult<'static>>> {
    let it = WalkDir::new(root).follow_links(true).into_iter().filter_entry(util::is_lua_file).filter_map(|e| e.ok());
    let entries = it.collect::<Vec<_>>();

    let pb = ProgressBar::new(entries.len() as u64);

    let module_name = util::path_to_module_name(path);
    let require_path = util::path_to_require_path(path, root).unwrap();
    let new_module_name = util::path_to_module_name(new_path);
    let new_require_path = util::path_to_require_path(new_path, root).unwrap();

    let process = |entry: &walkdir::DirEntry| -> Result<Option<RenameResult<'static>>> {
        let result = if entry.file_type().is_file() {
            let source = fs::read_to_string(entry.path())?;
            let ast: ast::Ast<'_> = full_moon::parse(&source).map_err(|e| anyhow!(format!("{}", e)))?;

            match update_require_paths_and_module_identifiers(&root, entry.path(), ast, &module_name, &new_module_name, &require_path, &new_require_path) {
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
    use crate::rename_module::*;
    use full_moon;
    use std::path::PathBuf;
    use pretty_assertions::assert_eq;

    fn assert_update_require<'a>(path: &str,
                                 old_path: &str,
                                 new_module_name: &str,
                                 before: &str,
                                 after: &str) {
        let old_path = PathBuf::from(old_path);
        let root = PathBuf::from("src/");
        let old_module_name = util::path_to_module_name(&old_path);
        let old_require_path = util::path_to_require_path(&old_path, &root).unwrap();
        let new_path = old_path.with_file_name(new_module_name).with_extension("lua");
        let new_require_path = util::path_to_require_path(&new_path, &root).unwrap();

        let ast = full_moon::parse(before).unwrap();
        let result = update_require_paths_and_module_identifiers(&root, &PathBuf::from(path), ast, &old_module_name, &new_module_name, &old_require_path, &new_require_path).unwrap();

        assert_eq!(after, full_moon::print(&result.new_ast.unwrap()));
    }

    fn assert_warns<'a>(path: &str,
                        old_path: &str,
                        new_module_name: &str,
                        before: &str,
                        warning: &str) {
        let old_path = PathBuf::from(old_path);
        let root = PathBuf::from("src/");
        let old_module_name = util::path_to_module_name(&old_path);
        let old_require_path = util::path_to_require_path(&old_path, &root).unwrap();
        let new_path = old_path.with_file_name(new_module_name).with_extension("lua");
        let new_require_path = util::path_to_require_path(&new_path, &root).unwrap();

        let ast = full_moon::parse(before).unwrap();
        let result = update_require_paths_and_module_identifiers(&root, &PathBuf::from(path), ast, &old_module_name, &new_module_name, &old_require_path, &new_require_path).unwrap();

        assert_eq!(Some(warning.to_string()), result.warnings.iter().next().map(|w| w.message.clone()));
    }

    #[test]
    fn updates_require_path() {
        assert_update_require("src/api/test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = require("api.Rand")

Rand.rnd(2)
Rand.rnd.rnd(2)
"#,
                              r#"
local Dood = require("api.Dood")

Dood.rnd(2)
Dood.rnd.rnd(2)
"#
        );
    }

    #[test]
    fn updates_require_path_without_ident() {
        assert_update_require("src/api/Test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Test = require("api.Rand")

Test.rnd(2)
Test.rnd.rnd(2)
"#,
                              r#"
local Test = require("api.Dood")

Test.rnd(2)
Test.rnd.rnd(2)
"#
        );
    }

    #[test]
    fn updates_require_path_single_quote() {
        assert_update_require("src/api/test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = require('api.Rand')
"#,
                              r#"
local Dood = require("api.Dood")
"#
        );
    }

    #[test]
    fn updates_require_path_trailing() {
        assert_update_require("src/api/test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = require("api.Rand", dood, asd)
"#,
                              r#"
local Dood = require("api.Dood", dood, asd)
"#
        );
    }

    #[test]
    fn updates_module_name_in_prefix() {
        assert_update_require("src/api/Test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = require("api.Rand")

local Test = {}

function Test.dood()
   return Rand.rnd(5) + Test.Rand.rnd(5)
end

return Test
"#,
                              r#"
local Dood = require("api.Dood")

local Test = {}

function Test.dood()
   return Dood.rnd(5) + Test.Rand.rnd(5)
end

return Test
"#
        );
    }

    #[test]
    fn updates_module_name_in_def() {
        assert_update_require("src/api/Rand.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = {}

function Rand.rnd(n)
   return rng:rnd(n)
end

function Rand.one_in(n)
   return Rand.rnd(n) == 0
end

Rand.NUM = 10

return Rand
"#,
                              r#"
local Dood = {}

function Dood.rnd(n)
   return rng:rnd(n)
end

function Dood.one_in(n)
   return Dood.rnd(n) == 0
end

Dood.NUM = 10

return Dood
"#
        );
    }

    #[test]
    fn updates_module_name_in_class_def() {
        assert_update_require("src/api/Test.lua", "src/api/Test.lua", "Dood",
                              r#"
local Test = class.class("Test")

function Test:test(n)
   return Test:test(n)
end

function Test.static_test(n)
   return Test.static_test(n)
end

return Test
"#,
                              r#"
local Dood = class.class("Test")

function Dood:test(n)
   return Dood:test(n)
end

function Dood.static_test(n)
   return Dood.static_test(n)
end

return Dood
"#
        );
    }

    #[test]
    fn updates_function_module() {
        assert_update_require("src/api/Test.lua", "src/api/Test.lua", "Dood",
                              r#"
local Test = {}

Test()

return Test
"#,
                              r#"
local Dood = {}

Dood()

return Dood
"#,
        );
    }

    #[test]
    fn does_not_update_module_name_in_multi_assign() {
        assert_update_require("src/api/test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand

Rand = Rand or require("api.Rand")
"#,
                              r#"
local Rand

Rand = Rand or require("api.Dood")
"#
        );
    }

    #[test]
    fn does_not_update_module_name_in_non_def() {
        assert_update_require("src/api/Rand.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = {}
"#,
                              r#"
local Dood = {}
"#
        );
        assert_update_require("src/api/Test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand = {}

return Rand
"#,
                              r#"
local Rand = {}

return Rand
"#
        );
    }

    #[test]
    fn does_not_update_identifier_in_unrelated_ident() {
        assert_update_require("src/test.lua", "src/api/Rand.lua", "Dood",
                              r#"
local Rand
local Dood
"#,
                              r#"
local Rand
local Dood
"#
        );
    }

    #[test]
    fn warns_about_literal_require_path() {
        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local path = "api.Rand"
"#,
                     "Found string matching old require path outside require"
        );

        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local path = "api.Dood"
"#,
                     "Found string matching new require path outside require"
        );
    }

    #[test]
    fn warns_about_literal_identifier() {
        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local Rand
"#,
                     "Found local assignment matching old module name"
        );
        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local Dood
"#,
                     "Found local assignment matching new module name"
        );
    }

    #[test]
    fn warns_about_literal_string() {
        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local api = "Rand"
"#,
                     "Found string constant matching old module name"
        );
        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local api = "Dood"
"#,
                     "Found string constant matching new module name"
        );
    }

    #[test]
    fn warns_about_function_decl() {
        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local Rand = {}

function Rand.rnd(n)
   return rng:rnd(n)
end

return Rand
"#,
                     "Found local assignment matching old module name"
        );

        assert_warns("src/api/test.lua", "src/api/Rand.lua", "Dood",
                     r#"
local Dood = {}

function Dood.rnd(n)
   return rng:rnd(n)
end

return Dood
"#,
                     "Found local assignment matching new module name"
        );
    }
}
