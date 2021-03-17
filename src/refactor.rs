use full_moon::ast;
use full_moon::tokenizer::{Token, TokenType, TokenReference};
use full_moon::visitors::VisitorMut;
use full_moon::ast::punctuated::{Punctuated, Pair};

use anyhow::{anyhow, Result};
use walkdir::WalkDir;
use std::borrow::Cow;
use std::fs;
use std::path::Path;
use crate::ast_util::scopes::ScopeManager;

#[derive(Debug, Clone, Copy)]
enum FnKind {
    Function,
    Method
}

// NOTE: needs to be kept in sync with how path -> require path resolution is
// handled in the Lua code
fn path_to_require_path(path: &str) -> Option<String> {
    path.to_string()
        .strip_suffix(".lua")
        .map(|s| s.replace("/", "."))
        .map(|s| s.replace("\\", "."))
        .map(|s| s.to_string())
}

fn is_module_fn_decl(decl: &ast::FunctionDeclaration, module_name: &str, fn_name: &str) -> Option<FnKind> {
    let name = decl.name();

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
            return Some(FnKind::Function)
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
            if let Some(kind) = is_module_fn_decl(decl, module_name, fn_name) {
                return Some((decl, kind))
            }
        }
    }

    None
}

fn path_to_module_name(path: &Path) -> String {
    path.file_stem().unwrap().to_str().unwrap().into()
}

struct RenameFnDefVisitor {
    module_name: String,
    fn_name: String,
    new_name: String,
}

fn gen_fn_name<'ast>(module_name: String, fn_name: String, fn_kind: FnKind) -> ast::FunctionName<'ast> {
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
        if let Some(fn_kind) = is_module_fn_decl(&decl, &self.module_name, &self.fn_name) {
            let name = gen_fn_name(self.module_name.clone(), self.new_name.clone(), fn_kind);
            decl.with_name(name)
        } else {
            decl
        }
    }
}

struct RenameFnCallVisitor {
    require_path: String,
    module_name: String,
    fn_name: String,
    new_name: String,
    scopes: ScopeManager
}

impl VisitorMut<'_> for RenameFnCallVisitor {
    fn visit_expression<'ast>(&mut self, expr: ast::Expression<'ast>) -> ast::Expression<'ast> {
        if let ast::Expression::Value { value: ref val, ref binop } = expr {
            if let ast::Value::FunctionCall(funcall) = &**val {
                if let ast::Prefix::Name(name) = funcall.prefix() {
                    if name.token().to_string() == self.module_name {
                        let mut suffixes = funcall.iter_suffixes();

                        let first_suffix = suffixes.next();
                        if let Some(ast::Suffix::Index(ast::Index::Dot { dot, name })) = first_suffix {
                            if name.token().to_string() == self.fn_name {
                                let name_tok = Token::new(TokenType::Identifier { identifier: Cow::from(self.new_name.clone()) });
                                let new_name = Cow::Owned(TokenReference::new(Vec::new(), name_tok, Vec::new()));
                                let new_dot = ast::Index::Dot { dot: dot.clone(), name: new_name };
                                let new_suffix = ast::Suffix::Index(new_dot);
                                let mut new_suffixes = funcall.iter_suffixes().cloned().collect::<Vec<ast::Suffix>>();
                                new_suffixes[0] = new_suffix;
                                let new_funcall = funcall.clone().with_suffixes(new_suffixes);
                                let new_value = ast::Value::FunctionCall(new_funcall);
                                return ast::Expression::Value { value: Box::new(new_value), binop: binop.clone() }
                            }
                        }
                    }
                }
            }
        }

        expr
    }
}

pub fn rename_in_file(path: &Path, require_path: &str, module_name: &str, fn_name: &str, new_name: &str) -> Result<()> {
    let source = fs::read_to_string(path)?;

    let mut ast: ast::Ast<'_> = full_moon::parse(&source).unwrap();

    let scope_manager = ScopeManager::new(&ast);

    // for (_, var) in &scope_manager.variables {
    //     println!("{:?}", var);
    // }

    let mut visitor = RenameFnCallVisitor {
        require_path: require_path.to_string(),
        module_name: module_name.to_string(),
        fn_name: fn_name.to_string(),
        new_name: new_name.to_string(),
        scopes: scope_manager
    };

    let new_ast = visitor.visit_ast(ast);

    println!("{}", full_moon::print(&new_ast));

    Ok(())
}

fn is_lua_file(entry: &walkdir::DirEntry) -> bool {
    if entry.file_type().is_dir() {
        return true
    }

    entry.file_name()
         .to_str()
         .map(|s| s.ends_with(".lua"))
         .unwrap_or(false)
}

pub fn rename_function(root: &Path, path: &Path, fn_name: &str, new_name: &str) -> Result<()> {
    let source = fs::read_to_string(path)?;

    let mut ast: ast::Ast<'_> = full_moon::parse(&source).unwrap();

    let module_name = path_to_module_name(path);

    if find_fn_def_in_module_file(&ast, &module_name, &fn_name).is_none() {
        return Err(anyhow!("Could not find definition in file"))
    }

    let mut rename_fn_visitor = RenameFnDefVisitor {
        module_name: module_name.clone(),
        fn_name: fn_name.to_string(),
        new_name: new_name.to_string()
    };

    let new_ast = rename_fn_visitor.visit_ast(ast);

    // println!("{}", full_moon::print(&new_ast));

    let require_path = path_to_require_path(path.to_str().unwrap()).unwrap();

    let walker = WalkDir::new(root).into_iter();
    for entry in walker.filter_entry(is_lua_file).filter_map(|e| e.ok()) {
        if entry.file_type().is_file() {
            rename_in_file(entry.path(), &require_path, &module_name, &fn_name, &new_name)?;
        }
    }

    Ok(())
}
