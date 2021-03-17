extern crate anyhow;
extern crate clap;
extern crate full_moon;
extern crate walkdir;
extern crate indicatif;

#[cfg(test)]
#[macro_use] extern crate pretty_assertions;

use std::fs;
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use anyhow::{anyhow, Result};
use clap::{Arg, App, SubCommand, ArgMatches, crate_version, crate_authors};
use indicatif::ProgressBar;
use rayon::prelude::*;

use full_moon::node::Node;
use full_moon::tokenizer::{Token, TokenReference};

mod ast_util;
mod context;
mod detect;
mod move_module;
mod refactor;
mod rename_module;
mod util;

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
        .subcommand(SubCommand::with_name("move")
                    .about("Moves a module to a different directory.")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Lua file")
                         .index(1))
                    .arg(Arg::with_name("NEW-DIR")
                         .required(true)
                         .help("New directory")
                         .index(2)))
        .subcommand(SubCommand::with_name("rename-module")
                    .about("Renames an API, class, or interface module.")
                    .arg(Arg::with_name("FILE")
                         .required(true)
                         .help("Lua file")
                         .index(1))
                    .arg(Arg::with_name("NEW-NAME")
                         .required(true)
                         .help("New name, must be alphanumeric-only")
                         .index(2)))
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

    if !input_file.is_file() {
        return Err(anyhow!("Input file {:?} does not exist", input_file))
    }

    let source = fs::read_to_string(input_file)?;

    let ast = full_moon::parse(&source).map_err(|e| anyhow!(format!("{}", e)))?;

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
    for parent in input_file.ancestors() {
        if parent.join(".git").is_dir() {
            return Some(parent.join("src/"))
        }
    }

    None
}

fn write_code(results: &Vec<refactor::RenameResult<'_>>) -> Result<()> {
    let pb = ProgressBar::new(results.len() as u64);

    let process = |result: &refactor::RenameResult<'_>| -> Result<()> {
        if let Some(new_ast) = &result.new_ast {
            fs::write(&result.filepath, full_moon::print(&new_ast))?;
        }

        pb.inc(1);

        Ok(())
    };

    results.par_iter().try_for_each(process)?;
    pb.finish_with_message("");

    Ok(())
}

fn print_results(results: &Vec<refactor::RenameResult<'_>>) {
    let mut updated = 0;
    let mut warnings = 0;
    for result in results.iter() {
        updated += result.renamed_count;
        warnings += result.warnings.len();

        for warning in &result.warnings {
            println!("{}", warning);
        }
    }

    println!("Made {} changes across {} files with {} warnings.", updated, results.len(), warnings);
}

fn cmd_rename(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let fn_name = sub_matches.value_of("FN-NAME").unwrap();
    let new_name = sub_matches.value_of("NEW-NAME").unwrap();

    if !input_file.is_file() {
        return Err(anyhow!("Input file {:?} does not exist", input_file))
    }

    let root = get_root(input_file).ok_or(anyhow!("Could not find root"))?;

    println!("Renaming function calls...");

    let results = refactor::rename_function(&root, &input_file, &fn_name, &new_name)?;

    println!("Writing code...");

    write_code(&results)?;

    print_results(&results);

    Ok(())
}

fn cmd_move(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let new_dir = Path::new(sub_matches.value_of("NEW-DIR").unwrap());

    if !input_file.is_file() {
        return Err(anyhow!("Input file {:?} does not exist", input_file))
    }

    if !new_dir.is_dir() {
        return Err(anyhow!("Output directory {:?} does not exist", new_dir))
    }

    let root = get_root(input_file).ok_or(anyhow!("Could not find root"))?;

    if !new_dir.starts_with(&root) {
        return Err(anyhow!("New directory {:?} is not contained in OpenNefia root {:?}", new_dir, root))
    }

    let new_path = new_dir.join(input_file.file_name().unwrap());

    if new_path.is_file() {
        return Err(anyhow!("New path {:?} already exists", new_path))
    }

    println!("Updating require paths...");

    let results = move_module::move_module_file(&root, &input_file, &new_path)?;

    println!("Writing code...");

    write_code(&results)?;

    println!("Performing move...");

    fs::copy(input_file, new_path)?;
    fs::remove_file(input_file)?;

    print_results(&results);

    Ok(())
}

fn cmd_rename_module(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());
    let new_name = sub_matches.value_of("NEW-NAME").unwrap();

    if !input_file.is_file() {
        return Err(anyhow!("Input file {:?} does not exist", input_file))
    }

    if !new_name.chars().all(char::is_alphanumeric) {
        return Err(anyhow!("Name must consist only of alphanumeric characters."))
    }

    let new_path = input_file.with_file_name(new_name).with_extension("lua");

    if new_path.is_file() {
        return Err(anyhow!("New path {:?} already exists", new_path))
    }

    let root = get_root(input_file).ok_or(anyhow!("Could not find root"))?;

    assert!(new_path.starts_with(&root));

    println!("Updating require paths and identifiers...");

    let results = rename_module::rename_module(&root, &input_file, &new_path)?;

    println!("Writing code...");

    write_code(&results)?;

    println!("Performing move...");

    fs::copy(input_file, new_path)?;
    fs::remove_file(input_file)?;

    print_results(&results);

    Ok(())
}

fn cmd_dump(sub_matches: &ArgMatches) -> Result<()> {
    let input_file = Path::new(sub_matches.value_of("FILE").unwrap());

    let source = fs::read_to_string(input_file)?;

    println!("{:#?}", full_moon::parse(&source).map_err(|e| anyhow!(format!("{}", e)))?);

    Ok(())
}

fn main() -> Result<()> {
    let matches = get_app().get_matches();

    match matches.subcommand() {
        ("detect", Some(sub_matches)) => cmd_detect(&sub_matches)?,
        ("rename", Some(sub_matches)) => cmd_rename(&sub_matches)?,
        ("move", Some(sub_matches)) => cmd_move(&sub_matches)?,
        ("rename-module", Some(sub_matches)) => cmd_rename_module(&sub_matches)?,
        ("dump", Some(sub_matches)) => cmd_dump(&sub_matches)?,
        _ => get_app().print_long_help()?
    }

    Ok(())
}
