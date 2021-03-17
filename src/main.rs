extern crate anyhow;
extern crate clap;
extern crate full_moon;

use std::fs;
use std::borrow::Cow;
use std::path::{Path};
use anyhow::Result;
use clap::{Arg, App, SubCommand, ArgMatches, crate_version, crate_authors};

use full_moon::{ast, node::Node};
use full_moon::tokenizer::{self, Token, TokenReference};

mod context;
mod detect;

fn get_app<'a, 'b>() -> App<'a, 'b> {
    App::new("yeek")
        .version(crate_version!())
        .author(crate_authors!())
        .about("Yee-eek!")
        .subcommand(SubCommand::with_name("detect")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Lua file")
                         .index(1))
        )
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

    let kind = detect::detect_module_kind(&ast, ctxt.clone());
    println!("{}\t{:?}", &ctxt.module_name, kind);

    Ok(())
}

fn main() -> Result<()> {
    let matches = get_app().get_matches();

    match matches.subcommand() {
        ("detect", Some(sub_matches)) => cmd_detect(&sub_matches)?,
        _ => get_app().print_long_help()?
    }

    Ok(())
}
