extern crate anyhow;
extern crate clap;
extern crate full_moon;
extern crate walkdir;

use std::fs;
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use anyhow::{anyhow, Result};
use clap::{Arg, App, SubCommand, ArgMatches, crate_version, crate_authors};

use full_moon::{ast, node::Node};
use full_moon::tokenizer::{self, Token, TokenReference};

mod ast_util;
mod context;
mod detect;
mod refactor;

fn get_app<'a, 'b>() -> App<'a, 'b> {
    App::new("yeek")
        .version(crate_version!())
        .author(crate_authors!())
        .about("Yee-eek!")
        .subcommand(SubCommand::with_name("detect")
                    .about("Detects the type of module in a Lua source file.")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Lua file")
                         .index(1)))
        .subcommand(SubCommand::with_name("rename")
                    .about("Renames a function.")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Lua file")
                         .index(1))
                    .arg(Arg::with_name("FN-NAME")
                         .required(true)
                         .help("Name of function or method")
                         .index(2))
                    .arg(Arg::with_name("NEW-NAME")
                         .required(true)
                         .help("New function name")
                         .index(3)))
        .subcommand(SubCommand::with_name("dump")
                    .about("Prints the full_moon AST for a Lua source file.")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Lua file")
                         .index(1)))
}

fn unpack_token_reference<'a>(token: Cow<TokenReference<'a>>) -> Vec<Token<'a>> {
    token
        .leading_trivia()
        .chain(std::iter::once(token.token()))
        .chain(token.trailing_trivia())
        .cloned()
        .collect()
}

fn cmd_detect(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());

    let source = fs::read_to_string(input_file)?;

    let tokens = tokenizer::tokens(&source)?;

    let ast = ast::Ast::from_tokens(tokens)
        .unwrap_or_else(|error| panic!("couldn't make ast for {:?} - {:?}", input_file, error));

    let old_positions: Vec<_> = ast.tokens().flat_map(unpack_token_reference).collect();
    let ast = ast.update_positions();
    assert_eq!(
        old_positions,
        ast.tokens()
           .flat_map(unpack_token_reference)
           .collect::<Vec<_>>(),
    );

    // println!("{}", full_moon::print(&ast));

    let ctxt = context::Context::new(&input_file);

    let module = detect::detect_module(&ast, ctxt.clone());
    println!("{:?}: {:?}", input_file, module);

    Ok(())
}

fn get_root(input_file: &Path) -> Option<PathBuf> {
    let mut new = input_file.clone();

    for parent in input_file.ancestors() {
        if parent.join(".git").is_dir() {
            return Some(parent.join("src/"))
        }
    }

    None
}

fn cmd_rename(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let fn_name = sub_matches.value_of("FN-NAME").unwrap();
    let new_name = sub_matches.value_of("NEW-NAME").unwrap();

    let root = get_root(input_file).ok_or(anyhow!("Could not find root"))?;

    refactor::rename_function(&root, &input_file, &fn_name, &new_name)?;

    Ok(())
}

fn cmd_dump(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());

    let source = fs::read_to_string(input_file)?;

    let tokens = tokenizer::tokens(&source)?;

    let ast = ast::Ast::from_tokens(tokens)
        .unwrap_or_else(|error| panic!("couldn't make ast for {:?} - {:?}", input_file, error));

    println!("{:#?}", ast);

    Ok(())
}

fn main() -> Result<()> {
    let matches = get_app().get_matches();

    match matches.subcommand() {
        ("detect", Some(sub_matches)) => cmd_detect(&sub_matches)?,
        ("rename", Some(sub_matches)) => cmd_rename(&sub_matches)?,
        ("dump", Some(sub_matches)) => cmd_dump(&sub_matches)?,
        _ => get_app().print_long_help()?
    }

    Ok(())
}
