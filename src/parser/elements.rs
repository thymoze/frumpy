use std::{
    collections::{HashMap, HashSet},
    f32::consts::E,
    hash::Hash,
};

use crate::parser::{
    Connections, Direction, ErrorKind, Grid, Position, RawConnection,
    boxes::{RawBox, RawBoxKind},
};

#[derive(Debug)]
enum ElementConnection {
    Parameter,
    Return,
    Input((usize, Direction)),
    Output(Vec<(usize, Direction)>),
}

enum Element {
    /// ```
    ///  ╔═══╗
    ///  ║ 1 ╟─
    ///  ╚═══╝
    /// ```
    Literal {
        content: String,
        outputs: Vec<(usize, Direction)>,
    },
    /// ```
    ///  ╔═══╗
    ///  ║   ╟─
    ///  ╚═══╝
    /// ```
    Stdin { outputs: Vec<(usize, Direction)> },
    /// ```
    ///  ╓───╖
    /// ─╢ + ╟─
    ///  ╙───╜
    /// ```
    FunctionDeclaration {
        name: String,
        connections: Connections<ElementConnection>,
    },
    /// ```
    ///  ┌───╖
    /// ─┤ + ╟─
    ///  ╘═╤═╝
    /// ```
    FunctionCall {
        name: String,
        connections: Connections<ElementConnection>,
    },
    /// ```
    ///   │
    /// ──┼──
    ///   │
    /// ```
    Cross {
        connections: Connections<ElementConnection>,
    },
    /// ```
    ///  ─┬─
    /// ```
    Nand {
        connections: Connections<ElementConnection>,
    },
    /// ```
    ///  ╒═╧═╗   
    /// ─┤   ╟─
    ///  ╘═╤═╝   
    /// ```   
    LambdaExpr {
        connections: Connections<ElementConnection>,
    },
    /// ```
    ///  ┌─┴─╖   
    /// ─┤   ╟─
    ///  └─┬─╜   
    /// ```
    LambdaInvocation {
        connections: Connections<ElementConnection>,
    },
}

struct FunctionDeclaration(String, Vec<Ast>);

enum Ast {
    Literal(String),
    Stdin,
    FunctionCall(String, Vec<Ast>),
    Nand(Vec<Ast>, Vec<Ast>),
    Cross(Vec<Ast>, Vec<Ast>),
}

struct Parser {
    grid: Grid,
    boxes: Vec<RawBox>,
}

impl Parser {
    fn new(grid: Grid, boxes: Vec<RawBox>) -> Self {
        Self { grid, boxes }
    }

    fn parse(mut self) -> Result<Vec<Element>, ErrorKind> {
        let mut elements: HashMap<usize, Element> = HashMap::new();

        let mut functions: HashMap<usize, (Vec<usize>, HashMap<Direction, (usize, Direction)>)> =
            HashMap::new();

        for (i, bx) in self
            .boxes
            .iter()
            .enumerate()
            .filter(|(_, b)| matches!(b.shape, RawBoxKind::Box2Opposing))
        {
            println!("Tracing function declaration {i}: {bx:?}");
            functions.insert(i, self.trace_function(i)?);
        }

        // dbg!(&functions);

        let mut declarations: HashMap<String, FunctionDeclaration> = HashMap::new();
        for (decl, (items, returns)) in functions {
            for (return_dir, (i, conn_dir)) in returns {
                let b = &self.boxes[i];
                match b.shape {
                    RawBoxKind::Tee => {
                        if b.connections.at(conn_dir.rotate_n(2)).is_none() {
                            // No connection opposite the output => this is a nand
                            let (
                                Some(RawConnection::Connected(input1, in1_dir)),
                                Some(RawConnection::Connected(input2, in2_dir)),
                            ) = (
                                b.connections.at(conn_dir.rotate_n(-1)),
                                b.connections.at(conn_dir.rotate_n(1)),
                            )
                            else {
                                panic!();
                            };
                        } else {
                            // This is a splitter
                            let input = b
                                .connections
                                .at(conn_dir.rotate_n(1))
                                .or(b.connections.at(conn_dir.rotate_n(-1)))
                                .expect(
                                    "Tee connection must have at least one adjoining connection",
                                );
                        }
                    }
                }
            }
        }

        todo!()
    }

    fn trace_function(
        &self,
        declaration: usize,
    ) -> Result<(Vec<usize>, HashMap<Direction, (usize, Direction)>), ErrorKind> {
        let func = &self.boxes[declaration];

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        let mut returns = HashMap::new();

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            for (conn_dir, conn) in self.boxes[current].connections.iter() {
                match conn {
                    RawConnection::Connected(next, _) => {
                        if !visited.contains(next) {
                            stack.push(*next);
                        }
                    }
                    RawConnection::Unconnected(dir) => {
                        if func.connections.iter().any(|(d, _)| d.rotate_n(2) == *dir)
                            || returns.insert(*dir, (current, conn_dir)).is_some()
                        {
                            return Err(ErrorKind::ConflictingFunctionReturn(
                                func.content.clone(),
                                *dir,
                            ));
                        }
                    }
                    _ => (),
                }
            }
        }

        if returns.is_empty() {
            return Err(ErrorKind::MissingFunctionReturn(
                func.content.clone(),
                func.top_left,
            ));
        }

        Ok((visited.into_iter().collect(), returns))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::boxes::BoxParser;

    use super::*;

    const FNS: &str = r#"
                                                  ┌──────────────────────┐
╓─────╖        ╓─────╖            ╓────╖          │  ╓─────╖             │
║ not ║     ┌──╢ and ╟──┐      ┌──╢ or ╟──┐       ├──╢ xor ╟─────┐       │
╙──┬──╜     │  ╙─────╜  │     ┌┴┐ ╙────╜ ┌┴┐      │  ╙─────╜ ┌┐  │    ┌┐ │
   │        └─────┬─────┘     └┬┘        └┬┘      └───┬──────┤├──┴──┬─┤├─┘
  ┌┴┐            ┌┴┐           └────┬─────┘           │      └┘     │ └┘
  └┬┘            └┬┘                │                 └────┬────────┘
   ┴              │                                        │
"#;

    #[test]
    fn test_parser() {
        let boxparser = BoxParser::new(FNS);
        let boxes = boxparser.parse().unwrap();
        let parser = Parser::new(boxparser.grid, boxes);
        let result = parser.parse().unwrap();

        assert!(false);
    }
}
