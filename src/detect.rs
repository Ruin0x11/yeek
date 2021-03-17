use full_moon::ast::{self, Ast};
use full_moon::visitors::Visitor;
use crate::context::Context;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceName(String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ApiDef;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDef {
    pub serial_id: String,
    pub implements: Vec<InterfaceName>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceDef {
    pub serial_id: String,
    // TODO
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleKind {
    Api(ApiDef),
    Class(ClassDef),
    Interface(InterfaceDef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    name: String,
    kind: ModuleKind
}

// class.class("MyClass", { IMyInterface })
static CLASS_MODULE_NAME: &str = "class";
static CLASS_DECL_FN_NAME: &str = "class";
static INTERFACE_DECL_FN_NAME: &str = "interface";

fn detect_class_implements(val: &ast::Value) -> Vec<InterfaceName> {
    let mut result = Vec::new();

    match val {
        ast::Value::Var(ast::Var::Name(name)) => {
            result.push(InterfaceName(name.token().to_string()));
        },
        ast::Value::TableConstructor(tbl) => {
            for field in tbl.fields() {
                match field {
                    ast::Field::NoKey(ast::Expression::Value { value: val, .. }) => {
                        if let ast::Value::Var(ast::Var::Name(name)) = &**val {
                            result.push(InterfaceName(name.token().to_string()));
                        }
                    }
                    _ => ()
                }
            }
        }
        _ => ()
    }

    result
}

fn detect_class_info(args: &ast::FunctionArgs) -> Option<ClassDef> {
    if let ast::FunctionArgs::Parentheses { arguments, .. } = args {
        let mut iter = arguments.iter();

        let first_arg = iter.next();
        if let Some(ast::Expression::Value { value: val, .. }) = first_arg {
            if let ast::Value::String(s) = &**val {
                let serial_id = s.token().to_string();

                let second_arg = iter.next();
                let implements = if let Some(ast::Expression::Value { value: val, .. }) = second_arg {
                    detect_class_implements(&val)
                } else {
                    Vec::new()
                };

                let def = ClassDef {
                    serial_id: serial_id,
                    implements: implements
                };

                return Some(def)
            }
        }
    }
    None
}

fn detect_interface_info(args: &ast::FunctionArgs) -> Option<InterfaceDef> {
    if let ast::FunctionArgs::Parentheses { arguments, .. } = args {
        let mut iter = arguments.iter();

        let first_arg = iter.next();
        if let Some(ast::Expression::Value { value: val, .. }) = first_arg {
            if let ast::Value::String(s) = &**val {
                let serial_id = s.token().to_string();

                // TODO requirements, parents

                let def = InterfaceDef {
                    serial_id: serial_id,
                };

                return Some(def)
            }
        }
    }
    None
}

fn get_prefixed_call<'a, 'b>(expr: &'a ast::Expression<'b>, module_name: &str, fn_name: &str) -> Option<&'a ast::FunctionArgs<'b>> {
    if let ast::Expression::Value { value: val, .. } = expr {
        if let ast::Value::FunctionCall(funcall) = &**val {
            if let ast::Prefix::Name(name) = funcall.prefix() {
                if name.token().to_string() == module_name {
                    let mut suffixes = funcall.iter_suffixes();

                    let first_suffix = suffixes.next();
                    if let Some(ast::Suffix::Index(ast::Index::Dot { name, .. })) = first_suffix {
                        if name.token().to_string() == fn_name {
                            let second_suffix = suffixes.next();
                            if let Some(ast::Suffix::Call(ast::Call::AnonymousCall(args))) = second_suffix {
                                return Some(args)
                            }
                        }
                    }
                }
            }

            return None
        }
    }
    None
}

fn detect_class_module_declaration(expr: &ast::Expression) -> Option<ModuleKind> {
    return get_prefixed_call(expr, CLASS_MODULE_NAME, CLASS_DECL_FN_NAME)
        .and_then(|args| detect_class_info(args))
        .map(ModuleKind::Class)
}

fn detect_interface_module_declaration(expr: &ast::Expression) -> Option<ModuleKind> {
    return get_prefixed_call(expr, CLASS_MODULE_NAME, INTERFACE_DECL_FN_NAME)
        .and_then(|args| detect_interface_info(args))
        .map(ModuleKind::Interface)
}

fn is_empty_constructor_expression(expr: &ast::Expression) -> bool {
    if let ast::Expression::Value { value: val, .. } = expr {
        if let ast::Value::TableConstructor(tbl) = &**val {
            return tbl.fields().len() == 0
        }
    }
    false
}

fn detect_module_declaration_in_expr(ctxt: &Context, first_expr: &ast::Expression<'_>) -> Option<Module> {
    if is_empty_constructor_expression(&first_expr) {
        return Some(Module { name: ctxt.module_name.clone(), kind: ModuleKind::Api(ApiDef) })
    }

    if let Some(class_module) = detect_class_module_declaration(&first_expr) {
        return Some(Module { name: ctxt.module_name.clone(), kind: class_module })
    }

    if let Some(iface_module) = detect_interface_module_declaration(&first_expr) {
        return Some(Module { name: ctxt.module_name.clone(), kind: iface_module })
    }

    None
}

fn detect_module_declaration(ctxt: &Context, assign: &ast::LocalAssignment<'_>) -> Option<Module> {
    let names = assign.name_list();
    let exprs = assign.expr_list();

    if exprs.len() == 0 {
        return None
    }

    let first_name = names.iter().next().unwrap();
    let first_expr = exprs.iter().next().unwrap();

    if first_name.token().to_string() != ctxt.module_name {
        return None
    }

    detect_module_declaration_in_expr(ctxt, &first_expr)
}

fn find_module_declaration<'a, 'b>(ctxt: &Context, stmt: &'a ast::Stmt<'b>) -> Option<Module> {
    match stmt {
        ast::Stmt::LocalAssignment(assign) => {
            if let Some(module_kind) = detect_module_declaration(ctxt, &assign) {
                return Some(module_kind)
            }
        },
        _ => ()
    }
    None
}

fn find_module_declarations<'a, 'b>(ctxt: &Context, block: &'a ast::Block<'b>) -> Vec<Module> {
    let mut result = Vec::new();
    for stmt in block.iter_stmts() {
        if let Some(module) = find_module_declaration(ctxt, stmt) {
            result.push(module);
        }
    }
    result
}

fn returns_assigned_local<'a, 'b>(module_name: &str, ret: &'a ast::Return<'b>) -> bool {
    let returns = ret.returns();

    if let Some(ast::Expression::Value { value: val, .. }) = returns.iter().next() {
        if let ast::Value::Var(var) = &**val {
            if let ast::Var::Name(name) = var {
                return name.token().to_string() == module_name
            }
        }
    }
    false
}

#[derive(Debug)]
struct ModuleDetectVisitor {
    is_top_level: bool,
    module: Option<Module>,
    ctxt: Context
}

impl ModuleDetectVisitor {
    pub fn new(ctxt: Context) -> Self {
        ModuleDetectVisitor {
            is_top_level: true,
            module: None,
            ctxt: ctxt
        }
    }

    fn visit_top_level_block(&mut self, block: &ast::Block) {
        let modules = find_module_declarations(&self.ctxt, block);
        let last_stmt_opt = block.last_stmt();
        let mut found = None;

        for module in modules.iter() {
            if let Some(ast::LastStmt::Return(ret)) = last_stmt_opt {
                if returns_assigned_local(&module.name, ret) {
                    found = Some(module.clone());
                    break;
                }
            }
        }

        if found.is_none() {
            if let Some(ast::LastStmt::Return(ret)) = last_stmt_opt {
                if let Some(expr) = ret.returns().iter().next() {
                    if let Some(module) = detect_module_declaration_in_expr(&self.ctxt, expr) {
                        found = Some(module.clone());
                    }
                }
            }
        }

        self.module = found;
    }
}

impl Visitor<'_> for ModuleDetectVisitor {
    fn visit_block(&mut self, block: &ast::Block) {
        if self.is_top_level {
            self.is_top_level = false;
            self.visit_top_level_block(block);
            return;
        }
    }
}

pub fn detect_module<'a>(ast: &Ast<'a>, ctxt: Context) -> Option<Module> {
    let mut visitor = ModuleDetectVisitor::new(ctxt);

    visitor.visit_ast(ast);

    visitor.module
}


#[cfg(test)]
mod tests {
    use crate::detect::*;
    use full_moon;
    use std::path::PathBuf;

    fn parse_detect<'a>(filename: &'a str, code: &'a str) -> Option<Module> {
        let ast = full_moon::parse(code).unwrap();
        let ctxt = Context::new(&PathBuf::from(filename));
        detect_module(&ast, ctxt)
    }

    #[test]
    fn detects_api_module() {
        assert_eq!(parse_detect(
            "Api.lua",
            r#"
local Api = {}

return Api
"#),
            Some(Module {
                name: "Api".into(),
                kind: ModuleKind::Api(ApiDef)
            })
        );
    }

    #[test]
    fn detects_reassigned_api_module() {
        assert_eq!(parse_detect(
            "Api.lua",
            r#"
local Api = {}

Api = nil

return Api
"#),
            Some(Module {
                name: "Api".into(),
                kind: ModuleKind::Api(ApiDef)
            })
        );
    }

    #[test]
    fn does_not_detect_misnamed_api_module() {
        assert_eq!(parse_detect(
            "Dood.lua",
            r#"
local Api = {}

return Api
"#),
            None
        );
    }

    #[test]
    fn does_not_detect_differing_return() {
        assert_eq!(parse_detect(
            "Api.lua",
            r#"
local Api = {}
local Dood = nil

return Dood
"#),
            None
        );
    }

    #[test]
    fn detects_proper_returned_module() {
        assert_eq!(parse_detect(
            "Api.lua",
            r#"
local Dood = {}
local Api = {}

return Api
"#),
            Some(Module {
                name: "Api".into(),
                kind: ModuleKind::Api(ApiDef)
            })
        );
    }

    #[test]
    fn does_not_detect_at_below_top_level() {
        assert_eq!(parse_detect(
            "Api.lua",
            r#"
local function dood()
   local Api = {}

   return Api
end

return dood()
"#),
            None
        );
    }

    #[test]
    fn detects_class_module() {
        assert_eq!(parse_detect(
            "Class.lua",
            r#"
local Class = class.class("MyClass")

return Class
"#),
            Some(Module {
                name: "Class".into(),
                kind: ModuleKind::Class(
                    ClassDef { serial_id: "\"MyClass\"".into(), implements: Vec::new() }
                )
            })
        );
    }

    #[test]
    fn detects_class_module_implements_single() {
        assert_eq!(parse_detect(
            "Class.lua",
            r#"
local Class = class.class("MyClass", ILocation)

return Class
"#),
            Some(Module {
                name: "Class".into(),
                kind: ModuleKind::Class(
                    ClassDef { serial_id: "\"MyClass\"".into(), implements: vec![
                        InterfaceName("ILocation".into())
                    ] }
                )
            })
        );
    }

    #[test]
    fn detects_class_module_implements_multiple() {
        assert_eq!(parse_detect(
            "Class.lua",
            r#"
local Class = class.class("MyClass", { IOwned, ILocation })

return Class
"#),
            Some(Module {
                name: "Class".into(),
                kind: ModuleKind::Class(
                    ClassDef { serial_id: "\"MyClass\"".into(), implements: vec![
                        InterfaceName("IOwned".into()),
                        InterfaceName("ILocation".into())
                    ] }
                )
            })
        );
    }

    #[test]
    fn detects_primary_class_module() {
        assert_eq!(parse_detect(
            "Class.lua",
            r#"
local Other = class.class("MyOther")

local Class = class.class("MyClass")

return Class
"#),
            Some(Module {
                name: "Class".into(),
                kind: ModuleKind::Class(
                    ClassDef { serial_id: "\"MyClass\"".into(), implements: Vec::new() }
                )
            })
        );
    }

    #[test]
    fn detects_class_module_joined_return() {
        assert_eq!(parse_detect(
            "Class.lua",
            r#"
return class.class("MyClass")
"#),
            Some(Module {
                name: "Class".into(),
                kind: ModuleKind::Class(
                    ClassDef { serial_id: "\"MyClass\"".into(), implements: Vec::new() }
                )
            })
        );
    }

    #[test]
    fn detects_interface_module() {
        assert_eq!(parse_detect(
            "IInterface.lua",
            r#"
local IInterface = class.interface("IMyInterface")

return IInterface
"#),
            Some(Module {
                name: "IInterface".into(),
                kind: ModuleKind::Interface(
                    InterfaceDef { serial_id: "\"IMyInterface\"".into() }
                )
            })
        );
    }

    #[test]
    fn detects_interface_module_joined_return() {
        assert_eq!(parse_detect(
            "IInterface.lua",
            r#"
return class.interface("IMyInterface")
"#),
            Some(Module {
                name: "IInterface".into(),
                kind: ModuleKind::Interface(
                    InterfaceDef { serial_id: "\"IMyInterface\"".into() }
                )
            })
        );
    }
}
