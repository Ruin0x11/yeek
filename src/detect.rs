use full_moon::ast::{self, Ast};
use full_moon::visitors::Visitor;
use crate::context::Context;

#[derive(Debug, Clone)]
pub struct InterfaceName(String);

#[derive(Debug, Clone)]
pub struct ApiDef;

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub serial_id: String,
    pub implements: Vec<InterfaceName>
}

#[derive(Debug, Clone)]
pub struct InterfaceDef {
    pub serial_id: String,
    // TODO
}

#[derive(Debug, Clone)]
pub enum ModuleKind {
    Api(ApiDef),
    Class(ClassDef),
    Interface(InterfaceDef),
}

#[derive(Debug, Clone)]
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

fn detect_class_module_declaration(expr: &ast::Expression) -> Option<ModuleKind> {
    if let ast::Expression::Value { value: val, .. } = expr {
        if let ast::Value::FunctionCall(funcall) = &**val {
            if let ast::Prefix::Name(name) = funcall.prefix() {
                if name.token().to_string() == CLASS_MODULE_NAME {
                    let mut suffixes = funcall.iter_suffixes();

                    let first_suffix = suffixes.next();
                    if let Some(ast::Suffix::Index(ast::Index::Dot { name, .. })) = first_suffix {
                        if name.token().to_string() == CLASS_DECL_FN_NAME {
                            let second_suffix = suffixes.next();
                            if let Some(ast::Suffix::Call(ast::Call::AnonymousCall(args))) = second_suffix {
                                return detect_class_info(args)
                                    .map(ModuleKind::Class)
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

fn is_empty_constructor_expression(expr: &ast::Expression) -> bool {
    if let ast::Expression::Value { value: val, .. } = expr {
        if let ast::Value::TableConstructor(tbl) = &**val {
            return tbl.fields().len() == 0
        }
    }
    false
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

    if is_empty_constructor_expression(&first_expr) {
        return Some(Module { name: ctxt.module_name.clone(), kind: ModuleKind::Api(ApiDef) })
    }

    if let Some(class_module) = detect_class_module_declaration(&first_expr) {
        return Some(Module { name: ctxt.module_name.clone(), kind: class_module })
    }

    None
}

fn find_module_declarations<'a, 'b>(ctxt: &Context, block: &'a ast::Block<'b>) -> Vec<Module> {
    let mut result = Vec::new();
    for stmt in block.iter_stmts() {
        match stmt {
            ast::Stmt::LocalAssignment(assign) => {
                if let Some(module_kind) = detect_module_declaration(ctxt, &assign) {
                    result.push(module_kind)
                }
            },
            _ => ()
        }
    }
    result
}

fn returns_assigned_local<'a, 'b>(module_name: &str, ret: &'a ast::Return<'b>) -> bool {
    let returns = ret.returns();

    if returns.len() == 0 {
        return false
    }

    let first_return = returns.iter().next().unwrap();

    if let ast::Expression::Value { value: val, .. } = first_return {
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

        // TODO detect joined creation and return (return class.class(...))
        for module in modules.iter() {
            if let Some(ast::LastStmt::Return(ret)) = last_stmt_opt {
                if returns_assigned_local(&module.name, ret) {
                    self.module = Some(module.clone());
                    break;
                }
            }
        }
    }
}

impl Visitor<'_> for ModuleDetectVisitor {
    fn visit_function_call(&mut self, call: &ast::FunctionCall) {
        // println!("Yee-eek! {:?}", call)
    }

    fn visit_local_assignment(&mut self, node: &ast::LocalAssignment) {
        // println!("Yee-eek! {:?}", node)
    }

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
