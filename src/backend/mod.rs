mod instr;

use std::collections::HashMap;
use crate::{
    ast,
    ast::Expr,
};
use crate::lexer::Ident;

fn gen(ast: ast::File) -> Vec<u8> {
    let main_fn = ast.items.iter().find_map(|item| match item {
        ast::Item::Fn(f) if f.name == "main" => Some(f),
        _ => None,
    }).expect("no main function found");

    let main_code = gen_fn(main_fn);

    let mut out = vec![0; 0x150];
    out.extend(main_code);

    out
}

fn gen_fn(f: &ast::Fn) -> Vec<u8> {
    let mut lookup = HashMap::new();
    let mut out = vec![];

    for expr in &f.body {
        gen_expr(expr, &mut lookup);
    }

    out
}

/// Generates instructions that evaluate the given expression and store the result in A.
fn gen_expr(expr: &Expr, lookup: &mut HashMap<Ident, usize>) -> Vec<Instr> {
    match expr {
        Expr::Lit(lit) => {
            match lit {
                ast::LitExpr::Int { value, .. } => {
                    asm! {
                        ld a, {value};
                    }
                }
            }
        },
        Expr::Var(var) => {
            let index = lookup.get(&var.name).expect(&format!("variable {:?} not found", var.name));
            asm! {
                ld hl, sp+{index};
                ld a, (hl);
            };
        },
        Expr::Let(let_expr) => {
            let index = lookup.len();
            // TODO What if the variable already exists?
            lookup.insert(let_expr.name.clone(), index);

            gen_expr(&let_expr.value, lookup);
            asm! {
                ld hl, sp+{index}
                ld (hl), a
            };
        },
        Expr::While(w) => {

            asm! {
                condition:
            };

            gen_expr(&w.cond, lookup);

            asm! {
                and a;
                jp z, end;
            }

            for expr in w.body {
                gen_expr(&expr, lookup);
            }

            asm! {
                jp condition;
                end:
            }
        },
        Expr::BinOp(binop) => {
            gen_expr(&binop.lhs, lookup);
            let tmp_index = lookup.len();
            asm! {
                ld hl,sp+{tmp_index}
                ld (hl), a
            }
            gen_expr(&binop.rhs, lookup);

            // at this point, rhs is in 'a' and lhs is in '(hl)'

            match binop.op {
                ast::BinOp::Plus => asm! { add a, (hl); },
                ast::BinOp::Minus => asm! { sub a, (hl); },
                ast::BinOp::Lt => asm! {
                    inc a;
                    sub (hl);
                    rl a;
                    and 1;
                    xor 1;
                },
                ast::BinOp::Gt => asm! {
                    sub (hl);
                    rl a;
                    and 1;
                },
                ast::BinOp::Lte => asm! {
                    sub (hl);
                    rl a;
                    and 1;
                    xor 1;
                },
                ast::BinOp::Gte => asm! {
                    inc a;
                    sub (hl);
                    rl a;
                    and 1;
                },
                ast::BinOp::EqEq => asm! {
                    sub (hl);
                    and a;
                },
                // let (let x = 5) = 3;
                // 5 = 42;
                ast::BinOp::Eq => {
                    let Expr::Var(ast::VarExpr { name } ) = *binop.lhs else {
                        panic!("lhs of '=' must be a variable");
                    };
                    let index = lookup.get(&name).expect(&format!("variable {:?} not found", name));
                    asm! {
                        ld hl,sp+{index};
                        ld (hl), a;
                    }
                },
                ast::BinOp::PlusEq => {
                    let Expr::Var(ast::VarExpr { name } ) = *binop.lhs else {
                        panic!("lhs of '+=' must be a variable");
                    };
                    let index = lookup.get(&name).expect(&format!("variable {:?} not found", name));
                    asm! {
                        add (hl);
                        ld hl,sp+{index};
                        ld (hl), a;
                    }
                },
                ast::BinOp::MinusEq => {
                    let Expr::Var(ast::VarExpr { name } ) = *binop.lhs else {
                        panic!("lhs of '-=' must be a variable");
                    };
                    let index = lookup.get(&name).expect(&format!("variable {:?} not found", name));
                    asm! {
                        sub (hl);
                        cpl;
                        inc a;
                        ld hl,sp+{index};
                        ld (hl), a;
                    }
                },
            }
        },
        _ => todo!(),
    }

    vec![]
}

struct Code {
    statements: Vec<Statement>,
}

enum Statement {
    Instr(Instr),
    Label(String),
}
