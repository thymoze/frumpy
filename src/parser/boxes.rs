use std::{
    array,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    io,
    iter::repeat,
};

use slab::Slab;

use crate::parser::{Connections, Direction, ErrorKind, Grid, Position, RawConnection};

use super::ConnectionKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RawBoxKind {
    Box1,
    Box2Opposing,
    Box2Adjacent,
    Box3,
    Box4,
    Tee,
    Cross,
}

#[derive(Debug, PartialEq)]
pub(crate) struct RawBox {
    pub(crate) shape: RawBoxKind,
    pub(crate) top_left: Position,
    pub(crate) bottom_right: Position,
    pub(crate) content: String,
    pub(crate) connections: Connections<Position>,
}

fn is_in_box(pos: Position, top_left: Position, bottom_right: Position) -> bool {
    pos.0 >= top_left.0 && pos.0 <= bottom_right.0 && pos.1 >= top_left.1 && pos.1 <= bottom_right.1
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IO {
    Input,
    Output,
}

impl IO {
    fn rev(&self) -> Self {
        match self {
            IO::Input => IO::Output,
            IO::Output => IO::Input,
        }
    }
}

struct Return {
    from: usize,
    from_dir: Direction,
    return_dir: Direction,
}
impl Return {
    fn new(from: usize, from_dir: Direction, return_dir: Direction) -> Self {
        Self {
            from,
            from_dir,
            return_dir,
        }
    }
}

#[derive(Debug)]
struct Function {
    declaration: usize,
    connections: Connections<(usize, IO)>,
}

#[derive(Debug)]
pub(crate) struct BoxParser<'src> {
    // TODO: source could be removed
    source: &'src str,
    pub(crate) grid: Grid,
    boxes: Slab<RawBox>,
    connectors: HashMap<(Position, Direction), usize>,
    connections: RefCell<HashMap<(usize, Direction), IO>>,
    functions: HashMap<String, Function>,
}

impl<'src> BoxParser<'src> {
    pub(crate) fn new(source: &'src str) -> Self {
        Self {
            source,
            grid: Grid::new(source),
            boxes: Slab::new(),
            connectors: HashMap::new(),
            connections: RefCell::new(HashMap::new()),
            functions: HashMap::new(),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<(), Vec<ErrorKind>> {
        self.parse_tokens()?;

        self.trace_connections()?;

        // self.identify_connections(&mut boxes)?;

        Ok(())
    }

    fn trace_function(&mut self, declaration: usize) -> Result<(), ErrorKind> {
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

    fn insert_connection(&self, id: usize, dir: Direction, pos: Position, io: IO) {
        let mut connections = self.connections.borrow_mut();
        if !connections.contains_key(&(id, dir)) {
            connections.insert((id, dir), io);

            let (end_pos, end_dir) = self.trace_line(pos, dir);
            if let Some(end_pos) = end_pos {
                let end_id = *self
                    .connectors
                    .get(&(end_pos, end_dir))
                    .expect("end position must have a connector");
                connections.insert((end_id, end_dir), io.rev());
            }
        }
    }

    fn trace_connections(&mut self) -> Result<(), Vec<ErrorKind>> {
        // from (usize, Dir) -> to (usize, Dir)
        // let mut connections = HashMap::new();
        // let mut io = HashMap::new();
        // let mut returns = Vec::new();

        for (id, bx) in self.boxes.iter() {
            match bx.shape {
                RawBoxKind::Box4 | RawBoxKind::Box2Opposing => {
                    // All connections are outputs
                    for (dir, &pos) in bx.connections.iter() {
                        self.insert_connection(id, dir, pos, IO::Output);
                    }
                }
                RawBoxKind::Tee => {
                    for (dir, &pos) in bx.connections.iter() {
                        if let Some(io) = self.connections.borrow().get(&(id, dir)).cloned() {
                            let opposite_dir = dir.rotate_n(2);
                            if bx.connections.at(opposite_dir).is_some() {
                                self.insert_connection(id, opposite_dir, pos, io);

                                if bx.connections.at(dir.rotate_n(1)).is_some() {
                                    self.insert_connection(id, dir.rotate_n(1), pos, io.rev());
                                } else if bx.connections.at(dir.rotate_n(-1)).is_some() {
                                    self.insert_connection(id, dir.rotate_n(-1), pos, io.rev());
                                } else {
                                    panic!()
                                }
                            } else {
                                self.insert_connection(id, dir.rotate_n(1), pos, io.rev());
                                self.insert_connection(id, dir.rotate_n(-1), pos, io.rev());
                            }
                            break;
                        }
                    }
                }
                RawBoxKind::Cross => {
                    for (dir, &pos) in bx.connections.iter() {
                        if let Some(io) = self.connections.borrow().get(&(id, dir)).cloned() {
                            self.insert_connection(id, dir.rotate_n(2), pos, io);
                        }
                    }
                }
                RawBoxKind::Box2Adjacent => {
                    let declaration = self
                        .boxes
                        .iter()
                        .find(|&(decl_id, decl_bx)| id != decl_id && bx.content == decl_bx.content);
                }
                RawBoxKind::Box1 => {}
                RawBoxKind::Box3 => {}
            }

            // let RawConnection::Unknown(from) = conn;
            // let (end_pos, end_dir) = self.trace_line(from, dir);

            // if let Some(end_pos) = end_pos {
            //     let end_index = boxes
            //         .iter()
            //         .position(|b| is_in_box(end_pos, b.top_left, b.bottom_right))
            //         .expect("all boxes have been found");
            // } else {
            //     returns.push(Return::new(i, dir, end_dir))
            // }
        }

        Ok(())
    }

    fn insert_connected(
        &self,
        boxes: &Vec<RawBox>,
        io: &mut HashMap<(usize, Direction), IO>,
        returns: &mut Vec<Return>,
        from: Position,
        from_id: usize,
        from_dir: Direction,
        from_io: IO,
    ) -> Result<(), ErrorKind> {
        io.insert((from_id, from_dir), from_io);
        let (end_pos, end_dir) = self.trace_line(from, from_dir);

        if let Some(end_pos) = end_pos {
            let end_index = boxes
                .iter()
                .position(|b| is_in_box(end_pos, b.top_left, b.bottom_right))
                .expect("all boxes have been found");
            match from_io {
                IO::Input => io.insert((end_index, end_dir), IO::Output),
                IO::Output => io.insert((end_index, end_dir), IO::Input),
            };
        } else {
            match from_io {
                IO::Output => returns.push(Return::new(from_id, from_dir, end_dir)),
                IO::Input => return Err(ErrorKind::UnconnectedInput(from, from_dir)),
            }
        }

        Ok(())
    }

    // fn trace_connections_old(&self, boxes: &mut Vec<RawBox>) -> Result<(), Vec<ErrorKind>> {
    //     for i in 0..boxes.len() {
    //         let traced_connections = boxes[i]
    //             .connections
    //             .iter()
    //             .filter_map(|(dir, &conn)| {
    //                 if let RawConnection::Unknown(from) = conn {
    //                     let end = self.trace_line(from, dir);
    //                     Some((dir, end))
    //                 } else {
    //                     None
    //                 }
    //             })
    //             .collect::<Vec<_>>();

    //         for (dir, end) in traced_connections {
    //             if let Some(end_pos) = end.0 {
    //                 let index = boxes
    //                     .iter()
    //                     .position(|b| is_in_box(end_pos, b.top_left, b.bottom_right))
    //                     .expect("all boxes have been found");
    //                 boxes[i].connections.update(
    //                     dir,
    //                     RawConnection::Connected {
    //                         from_dir: dir,
    //                         to: index,
    //                         to_dir: end.1,
    //                         kind: ConnectionKind::Unknown,
    //                     },
    //                 );
    //                 boxes[index].connections.update(
    //                     end.1,
    //                     RawConnection::Connected {
    //                         from_dir: end.1,
    //                         to: i,
    //                         to_dir: dir,
    //                         kind: ConnectionKind::Unknown,
    //                     },
    //                 );
    //             } else {
    //                 boxes[i]
    //                     .connections
    //                     .update(dir, RawConnection::Unconnected(end.1));
    //             }
    //         }
    //     }

    //     if let Some(((_, RawConnection::Unknown(position)))) = boxes.iter().find_map(|b| {
    //         b.connections
    //             .iter()
    //             .find(|(_, conn)| matches!(conn, RawConnection::Unknown(_)))
    //     }) {
    //         return Err(vec![ErrorKind::UnresolvedConnection(*position)]);
    //     }

    //     Ok(())
    // }

    fn parse_tokens(&mut self) -> Result<(), Vec<ErrorKind>> {
        let mut errors = Vec::new();

        for (pos, c) in self.grid.iter() {
            match c {
                '╓' | '╒' | '┌' | '╔' => match self.trace_box(pos) {
                    Ok(Some(bx)) => {
                        let entry = self.boxes.vacant_entry();
                        for (dir, &pos) in bx.connections.iter() {
                            self.connectors.insert((pos, dir), entry.key());
                        }
                        entry.insert(bx);
                    }
                    Ok(None) => {}
                    Err(e) => errors.push(e),
                },

                tee @ ('┴' | '├' | '┬' | '┤') => {
                    let connections = match tee {
                        '┬' => Connections::with(None, Some(pos), Some(pos), Some(pos)),
                        '┤' => Connections::with(Some(pos), None, Some(pos), Some(pos)),
                        '┴' => Connections::with(Some(pos), Some(pos), None, Some(pos)),
                        '├' => Connections::with(Some(pos), Some(pos), Some(pos), None),
                        _ => unreachable!(),
                    };
                    let entry = self.boxes.vacant_entry();
                    for (dir, &pos) in connections.iter() {
                        self.connectors.insert((pos, dir), entry.key());
                    }
                    entry.insert(RawBox {
                        shape: RawBoxKind::Tee,
                        top_left: pos,
                        bottom_right: pos,
                        content: String::new(),
                        connections,
                    });
                }
                '┼' => {
                    let connections = Connections::with(Some(pos), Some(pos), Some(pos), Some(pos));
                    let entry = self.boxes.vacant_entry();
                    for (dir, &pos) in connections.iter() {
                        self.connectors.insert((pos, dir), entry.key());
                    }
                    entry.insert(RawBox {
                        shape: RawBoxKind::Cross,
                        top_left: pos,
                        bottom_right: pos,
                        content: String::new(),
                        connections,
                    });
                }
                '─' | '│' | '┌' | '┐' | '└' | '┘' | ' ' => continue,
                _ => {
                    if !self
                        .boxes
                        .iter()
                        .any(|(_, bx)| is_in_box(pos, bx.top_left, bx.bottom_right))
                    {
                        errors.push(ErrorKind::UnexpectedCharacter(pos, c));
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let mut to_remove = HashSet::new();
        for (i, b) in self.boxes.iter() {
            if b.connections.iter().next().is_none() {
                // This box is a comment
                to_remove.insert(i);
            } else if self
                .boxes
                .iter()
                .any(|(_, bx)| b != bx && is_in_box(b.top_left, bx.top_left, bx.bottom_right))
            {
                // This box is inside another box
                to_remove.insert(i);
            }
        }
        for &id in &to_remove {
            self.boxes.remove(id);
        }
        self.connectors.retain(|_, id| !to_remove.contains(id));

        Ok(())
    }

    fn trace_box(&self, top_left: Position) -> Result<Option<RawBox>, ErrorKind> {
        let mut pos = top_left;
        let mut bottom_right = None;
        let mut dir = Direction::East;

        let mut doubles = match self.grid.get(pos).expect("start position must exist") {
            '╓' => vec![Direction::West],
            '╒' => vec![Direction::North],
            '╔' => vec![Direction::North, Direction::West],
            _ => vec![],
        };
        let mut connections = Connections::new();

        while self.is_connected(pos, dir) {
            pos = self.step(pos, dir);
            if pos == top_left {
                break; // Completed the box
            }

            match self.grid.get(pos).expect("connected position must exist") {
                // Connections
                '┴' | '╧' if dir == Direction::East => {
                    if connections.north().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::North, pos);
                }
                '├' | '╟' if dir == Direction::South => {
                    if connections.east().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::East, pos);
                }
                '┬' | '╤' if dir == Direction::West => {
                    if connections.south().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::South, pos);
                }
                '┤' | '╢' if dir == Direction::North => {
                    if connections.west().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::West, pos);
                }

                // Sanity check edges
                c @ ('─' | '│') => {
                    if doubles.contains(&dir.rotate_n(-1)) {
                        return Err(ErrorKind::UnexpectedLineType(pos, c));
                    }
                }
                c @ ('═' | '║') => {
                    if !doubles.contains(&dir.rotate_n(-1)) {
                        return Err(ErrorKind::UnexpectedLineType(pos, c));
                    }
                }

                // Box tracing
                '┐' | '╕' | '└' | '╘' | '╙' | '╚' => dir = dir.rotate_n(1),
                '╖' | '╗' => {
                    doubles.push(dir);
                    dir = dir.rotate_n(1);
                }
                '╜' | '┘' => {
                    bottom_right = Some(pos);
                    dir = dir.rotate_n(1)
                }
                '╛' | '╝' => {
                    bottom_right = Some(pos);
                    doubles.push(dir);
                    dir = dir.rotate_n(1);
                }
                _ => continue,
            }
        }

        if pos == top_left {
            Ok(Some(RawBox {
                top_left,
                bottom_right: bottom_right.expect("Closed box must have bottom right corner"),
                content: self.extract_content_from_box(top_left, bottom_right.unwrap()),

                shape: match doubles.len() {
                    4 => RawBoxKind::Box4,
                    3 => RawBoxKind::Box3,
                    2 => {
                        if doubles[0].rotate_n(2) == doubles[1] {
                            RawBoxKind::Box2Opposing
                        } else {
                            RawBoxKind::Box2Adjacent
                        }
                    }
                    1 => RawBoxKind::Box1,
                    0 => return Ok(None), // This is not a box, but two tees
                    _ => return Err(ErrorKind::MalformedBox(top_left)),
                },
                connections,
            }))
        } else {
            Ok(None)
        }
    }

    fn extract_content_from_box(&self, top_left: Position, bottom_right: Position) -> String {
        self.source
            .lines()
            .skip(top_left.0 + 1)
            .take(bottom_right.0 - top_left.0 - 1)
            .map(|line| {
                line.chars()
                    .skip(top_left.1 + 1)
                    .take(bottom_right.1 - top_left.1 - 1)
                    .collect::<String>()
            })
            .fold(String::new(), |mut acc, chars| {
                let trimmed = chars.trim();
                if !trimmed.is_empty() {
                    if acc.is_empty() {
                        return trimmed.to_owned();
                    }

                    acc.reserve(trimmed.len() + 1);
                    acc.push('\n');
                    acc.push_str(trimmed);
                }
                acc
            })
    }

    fn trace_line(&self, start: Position, mut dir: Direction) -> (Option<Position>, Direction) {
        let mut pos = start;
        loop {
            pos = self.step(pos, dir);
            match (self.grid.get(pos), dir) {
                (Some('┬'), Direction::North)
                | (Some('┬'), Direction::East)
                | (Some('┬'), Direction::West)
                | (Some('┤'), Direction::North)
                | (Some('┤'), Direction::South)
                | (Some('┤'), Direction::East)
                | (Some('┴'), Direction::South)
                | (Some('┴'), Direction::West)
                | (Some('┴'), Direction::East)
                | (Some('├'), Direction::North)
                | (Some('├'), Direction::South)
                | (Some('├'), Direction::West)
                | (Some('╤'), Direction::North)
                | (Some('╢'), Direction::East)
                | (Some('╧'), Direction::South)
                | (Some('╟'), Direction::West)
                | (Some('┼'), _) => {
                    // let i = todo!();
                    // let i = self
                    //     .tokens
                    //     .iter()
                    //     .position(|t| pos == t.position())
                    //     .expect("all tokens have been found");
                    return (Some(pos), dir.rotate_n(2));
                }

                (Some('│'), Direction::North)
                | (Some('│'), Direction::South)
                | (Some('─'), Direction::West)
                | (Some('─'), Direction::East) => {
                    continue;
                }

                (Some('┌'), Direction::North)
                | (Some('┐'), Direction::East)
                | (Some('┘'), Direction::South)
                | (Some('└'), Direction::West) => {
                    dir = dir.rotate_n(1);
                    continue;
                }

                (Some('┌'), Direction::West)
                | (Some('┐'), Direction::North)
                | (Some('┘'), Direction::East)
                | (Some('└'), Direction::South) => {
                    dir = dir.rotate_n(-1);
                    continue;
                }

                c => {
                    return (None, dir);
                }
                (Some(' '), _) | (None, _) => return (None, dir),
            }
        }
    }

    fn step(&self, pos: Position, dir: Direction) -> Position {
        match dir {
            Direction::North => (pos.0.wrapping_sub(1), pos.1),
            Direction::East => (pos.0, pos.1 + 1),
            Direction::South => (pos.0 + 1, pos.1),
            Direction::West => (pos.0, pos.1.wrapping_sub(1)),
        }
    }

    fn is_connected(&self, pos: Position, dir: Direction) -> bool {
        let next_pos = self.step(pos, dir);
        match (self.grid.get(next_pos), dir) {
            (Some(c), Direction::North) => {
                matches!(c, '┌' | '╓' | '╒' | '╔' | '┤' | '╢' | '│' | '║')
            }
            (Some(c), Direction::East) => {
                matches!(c, '─' | '═' | '┐' | '╖' | '╕' | '╗' | '┴' | '╧')
            }
            (Some(c), Direction::South) => {
                matches!(c, '│' | '║' | '├' | '╟' | '┘' | '╛' | '╜' | '╝')
            }
            (Some(c), Direction::West) => {
                matches!(c, '─' | '═' | '└' | '╙' | '╘' | '╚' | '╤' | '┬')
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FN_DECL: &str = r"
  ╓───╖       ╒═══╕
──╢ + ╟──   ──┤ + ├──
  ╙───╜       ╘═══╛
  ";

    const FN_CALL: &str = r"
  ┌───╖      ╓───┐      ╔═══╕      ╒═══╗
──┤ + ╟──  ──╢ + ├──  ──╢ + ├──  ──┤ + ╟──
  ╘═╤═╝      ╚═╤═╛      ╙─┬─┘      └─┬─╜
    │          │          │          │
";
    const LITERAL: &str = r"
  ╔═══╗
  ║ 1 ╟──
  ╚═══╝

  ╔══════════════════════╗
  ║    ╔═══╗             ║
  ║    ║ 1 ╟──           ║
  ║    ╚═══╝             ║
  ║  fine weather today  ║
  ╚══════════════════════╝
  ";
    const LAMBDA_EXPR: &str = r"
    │          │          │          │
  ╒═╧═╗      ╓─┴─╖      ╔═╧═╕      ╔═╧═╗
──┤   ╟──  ──╢   ╟──  ──╢   ├──  ──╢   ╟──
  ╘═╤═╝      ╚═╤═╝      ╚═╤═╛      ╙─┬─╜
    │          │          │          │
    ";
    const LAMBDA_CALL: &str = r"
    │          │          │          │
  ┌─┴─╖      ┌─┴─┐      ╓─┴─┐      ╒═╧═╕
──┤   ╟──  ──┤   ├──  ──╢   ├──  ──┤   ├──
  └─┬─╜      ╘═╤═╛      ╙─┬─┘      └─┬─┘
    │          │          │          │
";

    const NAND_SPLITTER: &str = r"
                                                           ╔═══╗
╔═══╗         ╔═══╗         ──────┬──────                  ║ 4 ║
║ 1 ╟────┬────╢ 2 ║               │                        ╚═╤═╝
╚═══╝    │    ╚═══╝             ╔═╧═╗                        │    ╔═══╗
         │                      ║ 3 ║                        ├────╢ 5 ║
         │                      ╚═══╝                        │    ╚═══╝
";

    const TEE: &str = r"
                       
╔═══╗         ╔═══╗       ╔═══╗    
║ 1 ╟────┬────╢ 2 ╟──┐    ║ 5 ║    
╚═══╝    │    ╚═══╝  │    ╚╤══╝    
         │           └─────┘
         │             
";

    #[test]
    fn parse_boxes() {
        // let mut parser = Parser::new(FN_DECL);
        // let boxes = parser.parse_tokens().unwrap();
        // assert_eq!(boxes.len(), 2);

        // let mut parser = Parser::new(FN_CALL);
        // let boxes = parser.parse_tokens().unwrap();
        // assert_eq!(boxes.len(), 4);

        // let mut parser = Parser::new(LITERAL);
        // let boxes = parser.parse_tokens().unwrap();
        // assert_eq!(boxes.len(), 1);

        // let mut parser = Parser::new(LAMBDA_EXPR);
        // let boxes = parser.parse_tokens().unwrap();
        // assert_eq!(boxes.len(), 4);

        // let mut parser = Parser::new(LAMBDA_CALL);
        // let boxes = parser.parse_tokens().unwrap();
        // assert_eq!(boxes.len(), 4);

        let mut parser = BoxParser::new(TEE);
        let boxes = parser.parse().unwrap();
        assert_eq!(boxes.len(), 4, "{boxes:?}");
    }
}
