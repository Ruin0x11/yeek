use full_moon::ast::{self, Ast};
use full_moon::visitors::Visitor;
use crate::context::Context;

#[derive(Debug, Clone)]
pub enum ModuleKind {
    Api(String),
    Class(String),
    Unknown
}

impl ModuleKind {
    pub fn name<'a>(&'a self) -> Option<&'a str> {
        match self {
            ModuleKind::Api(ref name) => Some(name),
            ModuleKind::Class(ref name) => Some(name),
            _ => None
        }
    }
}

#[derive(Debug)]
struct ModuleDetectVisitor {
    is_top_level: bool,
    kind: ModuleKind,
    ctxt: Context
}

fn is_empty_constructor_expression(expr: &ast::Expression) -> bool {
    if let ast::Expression::Value { value: val, .. } = expr {
        if let ast::Value::TableConstructor(tbl)= &**val {
            return tbl.fields().len() == 0
        }
    }
    false
}

fn looks_like_module_declaration(ctxt: &Context, assign: &ast::LocalAssignment<'_>) -> bool {
    let names = assign.name_list();
    let exprs = assign.expr_list();

    if exprs.len() == 0 {
        return false
    }

    let first_name = names.iter().next().unwrap();
    let first_expr = exprs.iter().next().unwrap();

    if first_name.token().to_string() != ctxt.module_name {
        return false
    }

    is_empty_constructor_expression(&first_expr)
}

fn find_module_declaration<'a, 'b>(ctxt: &Context, block: &'a ast::Block<'b>) -> Option<ModuleKind> {
    for stmt in block.iter_stmts() {
            // println!("{:?}", stmt);
        match stmt {
            ast::Stmt::LocalAssignment(assign) => {
                if looks_like_module_declaration(ctxt, &assign) {
                    return Some(ModuleKind::Api(ctxt.module_name.clone()))
                }
            },
            _ => ()
        }
    }
    None
}

fn returns_assigned_local<'a, 'b>(module_name: &str, ret: &'a ast::Return<'b>) -> bool {
    let returns = ret.returns();

    if returns.len() == 0 {
        return false
    }

    let first_return = returns.iter().next().unwrap();

    if let ast::Expression::Value { value: val, .. } = first_return {
        if let ast::Value::Var(var)= &**val {
            if let ast::Var::Name(name) = var {
                return name.token().to_string() == module_name
            }
        }
    }
    false
}

impl ModuleDetectVisitor {
    pub fn new(ctxt: Context) -> Self {
        ModuleDetectVisitor {
            is_top_level: true,
            kind: ModuleKind::Unknown,
            ctxt: ctxt
        }
    }

    fn visit_top_level_block(&mut self, block: &ast::Block) {
        let module_kind_opt = find_module_declaration(&self.ctxt, block);
        let last_stmt_opt = block.last_stmt();

        if let Some(module_kind) = module_kind_opt {
            if let Some(name) = module_kind.name() {
                if let Some(ast::LastStmt::Return(ret)) = last_stmt_opt {
                    if returns_assigned_local(name, ret) {
                        self.kind = module_kind
                    }
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

pub fn detect_module_kind<'a>(ast: &Ast<'a>, ctxt: Context) -> ModuleKind {
    let mut visitor = ModuleDetectVisitor::new(ctxt);

    visitor.visit_ast(ast);

    visitor.kind
}
