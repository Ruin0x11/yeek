use std::path::Path;

#[derive(Debug, Clone)]
pub struct Context {
    pub module_name: String
}


impl Context {
    pub fn new(file_path: &Path) -> Self {
        Context {
            module_name: file_path.file_stem().unwrap().to_str().unwrap().into()
        }
    }
}
