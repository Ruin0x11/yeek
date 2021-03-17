use std::path::Path;
use walkdir;

pub static REQUIRE_FN_NAME: &str = "require";

// NOTE: needs to be kept in sync with how path -> require path resolution is
// handled in the Lua code
pub fn path_to_require_path(path: &Path, root: &Path) -> Option<String> {
    let path = path.strip_prefix(root).unwrap();

    path.to_str().unwrap().to_string()
        .strip_suffix(".lua")
        .map(|s| s.replace("/", "."))
        .map(|s| s.replace("\\", "."))
        .map(|s| s.strip_suffix(".init").unwrap_or(&s).to_string())
        .map(|s| s.trim_start_matches(".").to_string())
        .map(|s| s.to_string())
}

pub fn path_to_module_name(path: &Path) -> String {
    path.file_stem().unwrap().to_str().unwrap().into()
}

pub fn strip_quotes(mut string: String) -> String {
    string.pop();
    string.chars().skip(1).collect()
}

pub fn normalize_require_path(string: String) -> String {
    string.strip_suffix(".init").unwrap_or(&string).to_string()
}

pub fn is_useful_path(path: &Path) -> bool {
    return !path.ends_with("src/thirdparty")
}

pub fn is_lua_file(entry: &walkdir::DirEntry) -> bool {
    if !is_useful_path(entry.path()) {
        return false
    }

    if entry.file_type().is_dir() {
        return entry.file_name() != "locale"
    }

    entry.file_name()
         .to_str()
         .map(|s| s.ends_with(".lua"))
         .unwrap_or(false)
}

#[cfg(test)]
mod test {
    use crate::util;
    use std::path::PathBuf;

    #[test]
    fn test_path_to_module_name() {
        assert_eq!(&util::path_to_module_name(&PathBuf::from("src/api/Rand.lua")), "Rand");
    }
}
