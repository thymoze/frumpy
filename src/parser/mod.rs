use std::iter::repeat;

mod boxes;
mod elements;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
enum Direction {
    North = 0,
    East = 1,
    South = 2,
    West = 3,
}

impl Direction {
    fn rotate_n(&self, n: isize) -> Self {
        let x = (*self as isize + n).rem_euclid(4) as u8;
        unsafe { std::mem::transmute(x) }
    }
}

type Position = (usize, usize);

#[derive(Debug)]
struct Grid {
    width: usize,
    height: usize,
    grid: Vec<char>,
}

impl Grid {
    fn new(source: &str) -> Grid {
        let height = source.lines().count();
        let width = source.lines().map(|line| line.len()).max().unwrap_or(0);

        Self {
            width,
            height,
            grid: source
                .lines()
                .flat_map(|line| line.chars().chain(repeat(' ')).take(width))
                .collect(),
        }
    }

    fn get(&self, pos: Position) -> Option<char> {
        if pos.0 < self.height && pos.1 < self.width {
            self.grid.get(pos.0 * self.width + pos.1).cloned()
        } else {
            None
        }
    }

    fn iter(&self) -> impl Iterator<Item = (Position, char)> + '_ {
        self.grid
            .iter()
            .enumerate()
            .map(move |(i, &c)| ((i / self.width, i % self.width), c))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RawConnection {
    Unknown(Position),
    // Unconnected(Direction),
    // Connected {
    //     from_dir: Direction,
    //     to: usize,
    //     to_dir: Direction,
    //     kind: ConnectionKind,
    // },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConnectionKind {
    Unknown,
    Input,
    Output,
}

#[derive(Debug, PartialEq)]
struct Connections<T> {
    connections: [Option<T>; 4],
}

impl<T> Connections<T> {
    fn new() -> Self {
        Self {
            connections: [None, None, None, None],
        }
    }

    fn with(up: Option<T>, right: Option<T>, down: Option<T>, left: Option<T>) -> Self {
        Self {
            connections: [up, right, down, left],
        }
    }

    fn at(&self, direction: Direction) -> Option<&T> {
        self.connections[direction as usize].as_ref()
    }

    fn north(&self) -> Option<&T> {
        self.at(Direction::North)
    }

    fn east(&self) -> Option<&T> {
        self.at(Direction::East)
    }

    fn south(&self) -> Option<&T> {
        self.at(Direction::South)
    }

    fn west(&self) -> Option<&T> {
        self.at(Direction::West)
    }

    fn set(mut self, direction: Direction, connection: T) -> Self {
        self.update(direction, connection);
        self
    }

    fn update(&mut self, direction: Direction, connection: T) {
        self.connections[direction as usize] = Some(connection);
    }

    fn iter(&self) -> impl Iterator<Item = (Direction, &T)> {
        self.connections.iter().enumerate().filter_map(|(i, c)| {
            c.as_ref().map(|conn| {
                (
                    unsafe { std::mem::transmute::<u8, Direction>(i as u8) },
                    conn,
                )
            })
        })
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = (Direction, &mut T)> {
        self.connections
            .iter_mut()
            .enumerate()
            .filter_map(|(i, c)| {
                c.as_mut().map(|conn| {
                    (
                        unsafe { std::mem::transmute::<u8, Direction>(i as u8) },
                        conn,
                    )
                })
            })
    }
}

impl<T> FromIterator<Option<T>> for Connections<T> {
    fn from_iter<I: IntoIterator<Item = Option<T>>>(iter: I) -> Self {
        let mut iter = iter.into_iter().take(4);
        Connections::with(
            iter.next().flatten(),
            iter.next().flatten(),
            iter.next().flatten(),
            iter.next().flatten(),
        )
    }
}

#[derive(Debug)]
enum ErrorKind {
    UnexpectedCharacter(Position, char),
    UnexpectedLineType(Position, char),
    UnexpectedConnection(Position),
    UnresolvedConnection(Position),
    UnconnectedInput(Position, Direction),
    MalformedBox(Position),

    MissingFunctionReturn(String, Position),
    ConflictingFunctionReturn(String, Direction),
    InvalidFnReturn(String, Direction),
}
