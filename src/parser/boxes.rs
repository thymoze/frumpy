use std::iter::repeat;

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
    pub(crate) connections: Connections<RawConnection>,
}

#[derive(Debug)]
pub(crate) struct BoxParser<'src> {
    // TODO: source could be removed
    source: &'src str,
    pub(crate) grid: Grid,
}

fn is_in_box(pos: Position, top_left: Position, bottom_right: Position) -> bool {
    pos.0 >= top_left.0 && pos.0 <= bottom_right.0 && pos.1 >= top_left.1 && pos.1 <= bottom_right.1
}

impl<'src> BoxParser<'src> {
    pub(crate) fn new(source: &'src str) -> Self {
        Self {
            source,
            grid: Grid::new(source),
        }
    }

    pub(crate) fn parse(&self) -> Result<Vec<RawBox>, Vec<ErrorKind>> {
        let mut boxes = self.parse_tokens()?;

        self.trace_connections(&mut boxes)?;

        self.identify_connections(&mut boxes)?;

        Ok(boxes)
    }

    fn identify_connections(&self, boxes: &mut Vec<RawBox>) -> Result<(), Vec<ErrorKind>> {
        let mut done = false;
        while !done {
            done = true;
            for i in (0..boxes.len()) {
                for (dir, c) in boxes[i].connections.iter() {
                    match c {
                        RawConnection::Connected {
                            from_dir,
                            to,
                            to_dir,
                            kind: ConnectionKind::Unknown,
                        } => {
                            done = false;
                        }
                        _ => continue,
                    }
                }
            }
        }

        Ok(())
    }

    fn trace_connections(&self, boxes: &mut Vec<RawBox>) -> Result<(), Vec<ErrorKind>> {
        for i in 0..boxes.len() {
            let traced_connections = boxes[i]
                .connections
                .iter()
                .filter_map(|(dir, &conn)| {
                    if let RawConnection::Unknown(from) = conn {
                        let end = self.trace_line(from, dir);
                        Some((dir, end))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            for (dir, end) in traced_connections {
                if let Some(end_pos) = end.0 {
                    let index = boxes
                        .iter()
                        .position(|b| is_in_box(end_pos, b.top_left, b.bottom_right))
                        .expect("all boxes have been found");
                    boxes[i].connections.update(
                        dir,
                        RawConnection::Connected {
                            from_dir: dir,
                            to: index,
                            to_dir: end.1,
                            kind: ConnectionKind::Unknown,
                        },
                    );
                    boxes[index].connections.update(
                        end.1,
                        RawConnection::Connected {
                            from_dir: end.1,
                            to: i,
                            to_dir: dir,
                            kind: ConnectionKind::Unknown,
                        },
                    );
                } else {
                    boxes[i]
                        .connections
                        .update(dir, RawConnection::Unconnected(end.1));
                }
            }
        }

        if let Some(((_, RawConnection::Unknown(position)))) = boxes.iter().find_map(|b| {
            b.connections
                .iter()
                .find(|(_, conn)| matches!(conn, RawConnection::Unknown(_)))
        }) {
            return Err(vec![ErrorKind::UnresolvedConnection(*position)]);
        }

        Ok(())
    }

    fn parse_tokens(&self) -> Result<Vec<RawBox>, Vec<ErrorKind>> {
        let mut boxes: Vec<RawBox> = Vec::new();
        let mut errors = Vec::new();

        for (pos, c) in self.grid.iter() {
            match c {
                '╓' | '╒' | '┌' | '╔' => match self.trace_box(pos) {
                    Ok(Some(bx)) => {
                        boxes.push(bx);
                    }
                    Ok(None) => {}
                    Err(e) => errors.push(e),
                },

                tee @ ('┴' | '├' | '┬' | '┤') => {
                    let conn = Some(RawConnection::Unknown(pos));
                    let connections = match tee {
                        '┬' => Connections::with(None, conn, conn, conn),
                        '┤' => Connections::with(conn, None, conn, conn),
                        '┴' => Connections::with(conn, conn, None, conn),
                        '├' => Connections::with(conn, conn, conn, None),
                        _ => unreachable!(),
                    };
                    boxes.push(RawBox {
                        shape: RawBoxKind::Tee,
                        top_left: pos,
                        bottom_right: pos,
                        content: String::new(),
                        connections,
                    });
                }
                '┼' => {
                    let conn = Some(RawConnection::Unknown(pos));
                    boxes.push(RawBox {
                        shape: RawBoxKind::Cross,
                        top_left: pos,
                        bottom_right: pos,
                        content: String::new(),
                        connections: Connections::with(conn, conn, conn, conn),
                    });
                }
                '─' | '│' | '┌' | '┐' | '└' | '┘' | ' ' => continue,
                _ => {
                    if !boxes
                        .iter()
                        .any(|bx| is_in_box(pos, bx.top_left, bx.bottom_right))
                    {
                        errors.push(ErrorKind::UnexpectedCharacter(pos, c));
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let mut to_remove = Vec::new();
        for (i, b) in boxes.iter().enumerate() {
            if b.connections.iter().next().is_none() {
                // This box is a comment
                to_remove.push(i);
                continue;
            }

            if boxes
                .iter()
                .any(|bx| b != bx && is_in_box(b.top_left, bx.top_left, bx.bottom_right))
            {
                // This box is inside another box
                to_remove.push(i);
                continue;
            }
        }
        for i in to_remove.into_iter().rev() {
            boxes.remove(i);
        }

        Ok(boxes)
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
                    connections = connections.set(Direction::North, RawConnection::Unknown(pos));
                }
                '├' | '╟' if dir == Direction::South => {
                    if connections.east().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::East, RawConnection::Unknown(pos));
                }
                '┬' | '╤' if dir == Direction::West => {
                    if connections.south().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::South, RawConnection::Unknown(pos));
                }
                '┤' | '╢' if dir == Direction::North => {
                    if connections.west().is_some() {
                        return Err(ErrorKind::UnexpectedConnection(pos));
                    }
                    connections = connections.set(Direction::West, RawConnection::Unknown(pos));
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
