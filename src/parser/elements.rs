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

#[derive(Debug)]
pub(crate) enum Element {
    /// ```
    ///  ╔═══╗
    ///  ║ 1 ╟─
    ///  ╚═══╝
    /// ```
    Literal {
        content: String,
    },
    /// ```
    ///  ╔═══╗
    ///  ║   ╟─
    ///  ╚═══╝
    /// ```
    Stdin {},
    /// ```
    ///  ╓───╖
    /// ─╢ + ╟─
    ///  ╙───╜
    /// ```
    FunctionDeclaration {
        name: String,
    },
    /// ```
    ///  ┌───╖
    /// ─┤ + ╟─
    ///  ╘═╤═╝
    /// ```
    FunctionCall {
        name: String,
        connections: Connections<usize>,
    },
    /// ```
    ///   │
    /// ──┼──
    ///   │
    /// ```
    Cross {
        connections: Connections<usize>,
    },
    /// ```
    ///  ─┬─
    /// ```
    Nand {
        connections: Connections<usize>,
    },
    Splitter {
        connections: Connections<usize>,
    },
    /// ```
    ///  ╒═╧═╗
    /// ─┤   ╟─
    ///  ╘═╤═╝
    /// ```   
    LambdaExpr {
        connections: Connections<usize>,
    },
    /// ```
    ///  ┌─┴─╖   
    /// ─┤   ╟─
    ///  └─┬─╜   
    /// ```
    LambdaInvocation {
        connections: Connections<usize>,
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
}

#[cfg(test)]
mod tests {
    use crate::parser::boxes::BoxParser;

    use super::*;

    const FNS: &str = r#"
         
╓─────╖  
║ not ║  
╙──┬──╜  
   │     
  ┌┴┐    
  └┬┘    
   ┴─    
      
"#;

    //                                       ┌──────────────────────┐
    //    ╓─────╖            ╓────╖          │  ╓─────╖             │
    // ┌──╢ and ╟──┐      ┌──╢ or ╟──┐       ├──╢ xor ╟─────┐       │
    // │  ╙─────╜  │     ┌┴┐ ╙────╜ ┌┴┐      │  ╙─────╜ ┌┐  │    ┌┐ │
    // └─────┬─────┘     └┬┘        └┬┘      └───┬──────┤├──┴──┬─┤├─┘
    //      ┌┴┐           └────┬─────┘           │      └┘     │ └┘
    //      └┬┘                │                 └────┬────────┘
    //       │                                        │

    #[test]
    fn test_parser() {
        let mut boxparser = BoxParser::new(FNS);
        let boxes = boxparser.parse().unwrap();

        assert!(false);
    }
}
