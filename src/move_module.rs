use full_moon::ast;
use full_moon::ast::punctuated::{Punctuated, Pair};
use full_moon::ast::owned::Owned;
use full_moon::tokenizer::TokenReference;
use full_moon::visitors::VisitorMut;
use full_moon::node::Node;

use anyhow::{anyhow, Result};
use walkdir::WalkDir;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use indicatif::ProgressBar;
use rayon::prelude::*;
use crate::util;
use crate::refactor::{self, RenameResult, TokenCow, Warning};

struct UpdateRequireCallVisitor {
    filepath: PathBuf,
    old_require_path: String,
    new_require_path: String,
    renamed: HashSet<usize>,
    warnings: Vec<Warning>
}

impl UpdateRequireCallVisitor {
    fn warn(&mut self, node: &dyn Node, message: String) {
        self.warnings.push(Warning::new(self.filepath.clone(), node, message));
    }
}

type ParenthesesArgs<'a> = Punctuated<'a, ast::Expression<'a>>;

pub fn make_new_require_args<'a>(old_tok: &TokenCow<'a>, new_require_path: String, args: &ParenthesesArgs<'a>) -> ParenthesesArgs<'a> {
    let quoted = format!("\"{}\"", new_require_path);
    let new_require_path = ast::Value::String(refactor::make_new_token(old_tok, quoted));
    let mut new_args = ParenthesesArgs::new();

    if args.len() == 1 {
        new_args.push(Pair::End(ast::Expression::Value { value: Box::new(new_require_path), binop: None }));
    } else if args.len() >= 2 {
        let sym_tok = Cow::Owned(TokenReference::symbol(", ").unwrap());
        new_args.push(Pair::Punctuated(ast::Expression::Value { value: Box::new(new_require_path), binop: None }, sym_tok));
        for arg in args.pairs().skip(1) {
            new_args.push(arg.clone());
        }
    }
    new_args
}

impl VisitorMut<'_> for UpdateRequireCallVisitor {
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
                                    let args = make_new_require_args(&s, self.new_require_path.clone(), &arguments);
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

    fn visit_value<'ast>(&mut self, val: ast::Value<'ast>) -> ast::Value<'ast> {
        if let ast::Value::String(ref s) = val {
            let raw = s.token().to_string();
            let require_path = util::normalize_require_path(util::strip_quotes(raw));
            if require_path == self.old_require_path {
                let byte_pos = s.start_position().unwrap().bytes();
                if !self.renamed.contains(&byte_pos) {
                    self.warn(&s, "Found string matching literal outside require".to_string());
                }
            }
        }
        val
    }
}

pub fn update_require_calls_in_file<'a>(path: &Path, ast: ast::Ast<'_>, old_require_path: &str, new_require_path: &str) -> Result<RenameResult<'static>> {
    let mut visitor = UpdateRequireCallVisitor {
        filepath: PathBuf::from(path),
        old_require_path: old_require_path.to_string(),
        new_require_path: new_require_path.to_string(),
        renamed: HashSet::new(),
        warnings: Vec::new()
    };

    let new_ast = visitor.visit_ast(ast);

    Ok(RenameResult { filepath: PathBuf::from(path), new_ast: Some(new_ast.owned()), renamed_count: visitor.renamed.len() as u32, warnings: visitor.warnings })
}

pub fn move_module_file(root: &Path, path_from: &Path, path_to: &Path) -> Result<Vec<RenameResult<'static>>> {
    let it = WalkDir::new(root).follow_links(true).into_iter().filter_entry(util::is_lua_file).filter_map(|e| e.ok());
    let entries = it.collect::<Vec<_>>();

    let pb = ProgressBar::new(entries.len() as u64);

    let old_require_path = util::path_to_require_path(path_from, root).unwrap();
    let new_require_path = util::path_to_require_path(path_to, root).unwrap();

    let process = |entry: &walkdir::DirEntry| -> Result<Option<RenameResult<'static>>> {
        let result = if entry.file_type().is_file() {
            let source = fs::read_to_string(entry.path())?;
            let ast: ast::Ast<'_> = full_moon::parse(&source).map_err(|e| anyhow!(format!("{}", e)))?;

            match update_require_calls_in_file(entry.path(), ast, &old_require_path, &new_require_path) {
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
    use crate::move_module::*;
    use full_moon;
    use std::path::PathBuf;

    fn assert_update_require<'a>(path: &str,
                                 old_require_path: &str,
                                 new_require_path: &str,
                                 before: &str,
                                 after: &str) {
        let ast = full_moon::parse(before).unwrap();
        let result = update_require_calls_in_file(&PathBuf::from(path), ast, &old_require_path, &new_require_path).unwrap();
        assert_eq!(after, full_moon::print(&result.new_ast.unwrap()));
    }

    fn assert_warns<'a>(path: &str,
                        old_require_path: &str,
                        new_require_path: &str,
                        before: &str,
                        warning: &str) {
        let ast = full_moon::parse(before).unwrap();
        let result = update_require_calls_in_file(&PathBuf::from(path), ast, &old_require_path, &new_require_path).unwrap();

        assert_eq!(Some(warning.to_string()), result.warnings.iter().next().map(|w| w.message.clone()));
    }

    #[test]
    fn updates_require_path() {
        assert_update_require("api/test.lua", "api.Rand", "mod.tools.api.Rand",
                            r#"
local Rand = require("api.Rand")
"#,
                            r#"
local Rand = require("mod.tools.api.Rand")
"#
        );
    }

    #[test]
    fn updates_require_path_single_quote() {
        assert_update_require("api/test.lua", "api.Rand", "mod.tools.api.Rand",
                            r#"
local Rand = require('api.Rand')
"#,
                            r#"
local Rand = require("mod.tools.api.Rand")
"#
        );
    }

    #[test]
    fn updates_require_path_trailing() {
        assert_update_require("api/test.lua", "api.Rand", "mod.tools.api.Rand",
                            r#"
local Rand = require("api.Rand", dood, asd)
"#,
                            r#"
local Rand = require("mod.tools.api.Rand", dood, asd)
"#
        );
    }

    #[test]
    fn warns_about_literal_require_path() {
        assert_warns("api/test.lua", "api.Rand", "mod.tools.api.Rand",
                     r#"
local path = "api.Rand"
local Rand = require(path)
"#,
                     "Found string matching literal outside require"
        );
    }
}
