use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io;
use std::str::FromStr;
#[cfg(feature = "animation")]
use std::thread;
#[cfg(feature = "animation")]
use std::time::Duration;

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
enum Axis {
    Vertical,
    Horizontal,
}

impl FromStr for Axis {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "H" => Ok(Axis::Horizontal),
            "V" => Ok(Axis::Vertical),
            _ => Err(())
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "up" => Ok(Self::Up),
            "down" => Ok(Self::Down),
            "left" => Ok(Self::Left),
            "right" => Ok(Self::Right),
            _ => Err(())
        }
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::Up => write!(f, "^"),
            Direction::Down => write!(f, "v"),
            Direction::Left => write!(f, "<"),
            Direction::Right => write!(f, ">"),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
enum Color {
    Red,
    Green,
    LightGreen,
    DarkGreen,
    Blue,
    White,
    Yellow,
    Pink,
    Orange,
    Brown,
    Ocra,
}

impl FromStr for Color {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "red" => Ok(Self::Red),
            "green" => Ok(Self::Green),
            "light_green" => Ok(Self::LightGreen),
            "dark_green" => Ok(Self::DarkGreen),
            "blue" => Ok(Self::Blue),
            "white" => Ok(Self::White),
            "yellow" => Ok(Self::Yellow),
            "pink" => Ok(Self::Pink),
            "orange" => Ok(Self::Orange),
            "brown" => Ok(Self::Brown),
            "ocra" => Ok(Self::Ocra),
            _ => Err(())
        }
    }
}

impl Color {
    fn get_csi(&self) -> String {
        #[cfg(feature = "color")]
        return format!("\x1b[38;2;{}m", match self {
            Color::Red => "255;0;0",
            Color::Green => "138;226;52",
            Color::LightGreen => "0;255;0",
            Color::DarkGreen => "78;154;6",
            Color::Blue => "0;0;255",
            Color::White => "255;255;255",
            Color::Yellow => "252;233;79",
            Color::Pink => "255;192;203",
            Color::Orange => "255;165;0",
            Color::Brown => "150;75;0",
            Color::Ocra => "204;119;34",
        });
        #[cfg(not(feature = "color"))]
        return "".to_string();
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Ord, PartialOrd)]
struct Vehicle {
    id: char,
    x: usize,
    y: usize,
    dir: Option<Axis>,
    length: usize,
    color: Option<Color>,
}

type Node = Vec<Vehicle>;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Ord, PartialOrd)]
struct Move {
    starting: Node,
    id: char,
    direction: Direction,
    amount: usize,
}

#[derive(Debug, Copy, Clone)]
struct Difficulty {
    unique_children: usize,
    sum_information: f64,
    depth: usize,
}

fn parse_input() -> Result<(usize, usize, char, Direction, BTreeMap<char, Vehicle>), Box<dyn Error>> {
    let mut buffer = String::new();

    io::stdin().read_line(&mut buffer)?;
    let (w, h) = buffer.split_once(' ').expect("Invalid board size.");
    let (w, h): (usize, usize) = (w.trim().parse().expect("Invalid integer."),
                                  h.trim().parse().expect("Invalid integer."));

    buffer.clear();
    io::stdin().read_line(&mut buffer)?;
    let (tc, td) = buffer.split_once(' ').expect("Invalid board size.");
    let (tc, td): (char, Direction) = (tc.trim().parse().expect("Invalid letter."),
                                       td.trim().parse().expect("Invalid exit direction."));

    buffer.clear();
    io::stdin().read_line(&mut buffer)?;
    let colors_given: usize = buffer.trim().parse().expect("Invalid number of colors given.");

    let mut colors = BTreeMap::new();

    for _ in 0..colors_given {
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        let (vehicle, color) = buffer.split_once(' ').expect("Invalid color definition.");
        let (vehicle, color): (char, Color) = (vehicle.trim().parse().expect("Invalid letter."),
                                               color.trim().parse().expect("Invalid exit direction."));
        colors.insert(vehicle, color);
    }

    let mut vehicles = BTreeMap::new();

    for i in 0..h {
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        assert_eq!(buffer.trim().len(), w);

        for (j, letter) in buffer.char_indices().take(w) {
            if !letter.is_ascii_alphabetic() { continue; }
            if let Some(Vehicle { x, y, dir, length, .. }) = vehicles.get_mut(&letter) {
                if *length == 1 {
                    if *x == j && *y != i {
                        *dir = Some(Axis::Vertical);
                    } else if *x != j && *y == i {
                        *dir = Some(Axis::Horizontal);
                    } else {
                        panic!("Vehicle is diagonal?")
                    }
                }
                *length += 1;
            } else {
                vehicles.insert(letter, Vehicle {
                    id: letter,
                    x: j,
                    y: i,
                    dir: None,
                    length: 1,
                    color: colors.get(&letter).map(|c| c.clone()),
                });
            }
        }
    }

    Ok((w, h, tc, td, vehicles))
}

fn print_move(vehicles: &BTreeMap<char, Vehicle>, old: &Move, new: &Node, w: usize, h: usize) {
    let fill_table = |state: &Node, table: &mut Vec<char>| {
        for vehicle in state {
            for i in 0..vehicle.length {
                match vehicle.dir {
                    Some(Axis::Vertical) => table[(vehicle.y + i) * w + vehicle.x] = vehicle.id,
                    Some(Axis::Horizontal) => table[vehicle.y * w + (vehicle.x + i)] = vehicle.id,
                    None => unreachable!()
                }
            }
        }
    };

    let mut new_occupied = vec!['_'; w * h];
    fill_table(&old.starting, &mut new_occupied);

    let mut old_occupied = vec!['_'; w * h];
    fill_table(new, &mut old_occupied);

    let render_id = |id: char| {
        let csi = vehicles.get(&id).map(|v| v.color).flatten().map(|c| c.get_csi()).unwrap_or("".to_string());
        format!("{}{}{}", csi, id, if !csi.is_empty() { "\x1b[39m" } else { "" })
    };

    let print_row = |occupied: &Vec<char>, i: usize| {
        for j in 0..w {
            print!("{}", render_id(occupied[i * h + j]));
        }
    };

    for i in 0..h {
        print_row(&old_occupied, i);
        if i == h / 2 {
            print!("  {} {:02}{}  ", render_id(old.id), old.amount, old.direction);
        } else if i == h / 2 + 1 {
            print!("   -->   ");
        } else {
            print!("         ");
        }
        print_row(&new_occupied, i);
        println!();
    }
}

fn estimate_difficulty(root: &Node, states: &BTreeMap<Node, Option<Move>>, solution: &[&Move]) -> Difficulty {
    let mut inverted_states: BTreeMap<&Node, Vec<&Node>> = BTreeMap::new();
    inverted_states.insert(root, vec![]);
    for (node, parent) in states {
        if let Some(parent) = parent {
            if let Some(children) = inverted_states.get_mut(&parent.starting) {
                children.push(node);
            } else {
                inverted_states.insert(&parent.starting, vec![node]);
            }
        }
    }
    let inverted_states = inverted_states;

    let solution: BTreeSet<&Node> = solution.iter().map(|m| &m.starting).collect();

    let mut extra = BTreeMap::new();
    fn visit<'a>(node: &'a Node, depth: usize, states: &BTreeMap<&Node, Vec<&'a Node>>, solution: &BTreeSet<&Node>, extra: &mut BTreeMap<&'a Node, Difficulty>) {
        if let Some(children) = states.get(&node) {
            if !extra.contains_key(node) {
                for child in children {
                    visit(child, depth + 1, states, solution, extra);
                }
            }
            let further_children: Vec<&&Node> = children.iter().filter(|child| extra.get(*child).unwrap().depth > depth).collect();
            let this = f64::log2(further_children.len() as f64);
            let sum_information = this + further_children.iter().map(|n| extra.get(*n).unwrap().sum_information).filter(|f| !f64::is_infinite(*f)).sum::<f64>();
            let value = Difficulty { unique_children: further_children.len(), sum_information, depth };

            // Debug Printing
            #[cfg(feature = "dev")]
            {
                for i in 0..depth { print!(" ") }
                print!("{:?}", value);
                print!("{}", if solution.contains(node) { " solution" } else { "" });
                println!();
            }

            extra.insert(node, value);
        } else {
            extra.insert(node, Difficulty { unique_children: 0, sum_information: 0.0, depth: 0 });
        }
    }
    visit(root, 0, &inverted_states, &solution, &mut extra);
    *extra.get(&root).unwrap()
}

fn main() -> Result<(), Box<dyn Error>> {
    let (w, h, tc, td, vehicles) = parse_input()?;

    let mut states = BTreeMap::new();
    let mut last = None;

    let mut queue = VecDeque::new();
    let parsed: Node = vehicles.values().cloned().collect();
    let (ti, _) = parsed.iter().enumerate().find(|(_i, v)| v.id == tc).expect("how!");

    states.insert(parsed.clone(), None);
    queue.push_back(parsed.clone());

    'solve:
    while let Some(current) = queue.pop_front() {
        let mut occupied = vec![false; w * h];

        for vehicle in &current {
            for i in 0..vehicle.length {
                match vehicle.dir {
                    Some(Axis::Vertical) => occupied[(vehicle.y + i) * w + vehicle.x] = true,
                    Some(Axis::Horizontal) => occupied[vehicle.y * w + (vehicle.x + i)] = true,
                    None => unreachable!()
                }
            }
        }

        let occupied = occupied;

        let check_done = |state: &Node| {
            let target = state[ti];

            let mut done = true;
            match td {
                Direction::Down => {
                    for i in (target.y + target.length)..(h) {
                        if occupied[i * w + target.x] {
                            done = false;
                            break;
                        }
                    }
                }
                Direction::Right => {
                    for i in (target.x + target.length)..(w) {
                        if occupied[target.y * w + i] {
                            done = false;
                            break;
                        }
                    }
                }
                Direction::Up => {
                    for i in (0..target.y).rev() {
                        if occupied[i * w + target.x] {
                            done = false;
                            break;
                        }
                    }
                }
                Direction::Left => {
                    for i in (0..target.x).rev() {
                        if occupied[target.y * w + i] {
                            done = false;
                            break;
                        }
                    }
                }
            }

            done
        };

        let mut add_move = |node: Node, mov: Move| {
            let done = check_done(&node);
            if done { last = Some(mov.clone()); }
            if !states.contains_key(&node) {
                queue.push_back(node.clone());
                states.insert(node, Some(mov));
            }
            done
        };

        for (index, vehicle) in current.iter().enumerate() {
            match vehicle.dir {
                Some(Axis::Vertical) => {
                    for y in (0..vehicle.y).rev() {
                        if !occupied[y * w + vehicle.x] {
                            let mut cloned = current.clone();
                            cloned[index].y = y;
                            let amount = current[index].y - cloned[index].y;
                            if add_move(cloned, Move { starting: current.clone(), id: vehicle.id, direction: Direction::Up, amount }) {
                                break 'solve;
                            }
                        } else {
                            break;
                        }
                    }
                    for y in (vehicle.y + vehicle.length)..h {
                        if !occupied[y * w + vehicle.x] {
                            let mut cloned = current.clone();
                            cloned[index].y = y - vehicle.length + 1;
                            let amount = cloned[index].y - current[index].y;
                            if add_move(cloned, Move { starting: current.clone(), id: vehicle.id, direction: Direction::Down, amount }) {
                                break 'solve;
                            }
                        } else {
                            break;
                        }
                    }
                }
                Some(Axis::Horizontal) => {
                    for x in (0..vehicle.x).rev() {
                        if !occupied[vehicle.y * w + x] {
                            let mut cloned = current.clone();
                            cloned[index].x = x;
                            let amount = current[index].x - cloned[index].x;
                            if add_move(cloned, Move { starting: current.clone(), id: vehicle.id, direction: Direction::Left, amount }) {
                                break 'solve;
                            }
                        } else {
                            break;
                        }
                    }
                    for x in (vehicle.x + vehicle.length)..w {
                        if !occupied[vehicle.y * w + x] {
                            let mut cloned = current.clone();
                            cloned[index].x = x - vehicle.length + 1;
                            let amount = cloned[index].x - current[index].x;
                            if add_move(cloned, Move { starting: current.clone(), id: vehicle.id, direction: Direction::Right, amount }) {
                                break 'solve;
                            }
                        } else {
                            break;
                        }
                    }
                }
                None => unreachable!()
            }
        }
    }

    if let Some(last) = last {
        let mut solution = Vec::new();

        let mut current = &last;
        solution.push(current);
        while let Some(Some(node)) = states.get(&current.starting) {
            current = node;
            solution.push(current);
        }
        solution.reverse();

        println!("Solution has {} moves.", solution.len());
        let diff = estimate_difficulty(&parsed, &states, &solution);
        println!("Sum of all entropy: {:17.4}", diff.sum_information);
        println!("That is, *more or less*, the number of times you should have launched a coin to get the right answer.");

        #[cfg(feature = "steps")]
        for i in 0..solution.len() - 1 {
            let (old, new) = (solution.get(i).unwrap(), solution.get(i + 1).unwrap());
            print_move(&vehicles, old, &new.starting, w, h);

            #[cfg(feature = "animation")]
            {
                thread::sleep(Duration::from_millis(250));
                print!("\x1b[{}A", h);
            }

            #[cfg(not(feature = "animation"))]
            {
                println!();
            }
        }
    } else {
        println!("Arega Buongiorno, there is no solution.")
    }

    Ok(())
}
