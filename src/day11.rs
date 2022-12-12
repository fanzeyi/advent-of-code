use anyhow::{anyhow, Context, Result};
use once_cell::sync::OnceCell;
use regex::Regex;

const PART2: bool = true;

fn monkey_regex() -> &'static Regex {
    static MONKEY_REGEX: OnceCell<Regex> = OnceCell::new();

    MONKEY_REGEX.get_or_init(|| {
        Regex::new(
            r"Monkey (?P<id>\d+):
\s+Starting items: (?P<items>\d+(, \d+)*)
\s+Operation: new = (?P<operation>.+)
\s+Test: divisible by (?P<division>\d+)
\s+If true: throw to monkey (?P<true_monkey>\d+)
\s+If false: throw to monkey (?P<false_monkey>\d+)",
        )
        .expect("monkey regex")
    })
}

fn operation_regex() -> &'static Regex {
    static OPERATION_REGEX: OnceCell<Regex> = OnceCell::new();

    OPERATION_REGEX.get_or_init(|| {
        Regex::new(
            r"old ((\+ (?P<add_factor>\d+))|(\* (?P<multiply_factor>\d+))|(?P<square>\* old))",
        )
        .expect("monkey regex")
    })
}

#[derive(Debug, PartialEq)]
enum Operation {
    Add(u32),
    Multiply(u32),
    Square,
}

impl Operation {
    fn from_str(input: &str) -> Result<Self> {
        let captures = operation_regex()
            .captures(input)
            .with_context(|| anyhow!("operation regex didn't match"))?;

        if let Some(factor) = captures.name("add_factor") {
            Ok(Operation::Add(
                factor
                    .as_str()
                    .parse()
                    .with_context(|| anyhow!("unable to parse add factor"))?,
            ))
        } else if let Some(factor) = captures.name("multiply_factor") {
            Ok(Operation::Multiply(factor.as_str().parse().with_context(
                || anyhow!("unable to parse multiply factor"),
            )?))
        } else if let Some(_) = captures.name("square") {
            Ok(Self::Square)
        } else {
            Err(anyhow!("unable to parse operation"))
        }
    }

    fn execute(&self, item: u128) -> u128 {
        match self {
            Operation::Add(factor) => item + (*factor as u128),
            Operation::Multiply(factor) => item * (*factor as u128),
            Operation::Square => item * item,
        }
    }
}

#[derive(Debug, PartialEq)]
struct Monkey {
    id: u32,
    items: Vec<u128>,
    operation: Operation,
    division: u32,
    true_monkey: usize,
    false_monkey: usize,
    counter: usize,
}

macro_rules! get_capture {
    ($cap: expr, $name: literal) => {
        $cap.name($name)
            .with_context(|| anyhow!("no {}", $name))?
            .as_str()
    };
}

macro_rules! get_capture_parse {
    ($cap: expr, $name: literal) => {
        get_capture!($cap, $name)
            .parse()
            .with_context(|| anyhow!("unable to parse {}", $name))?
    };
}

impl Monkey {
    fn from_str(input: &str) -> Result<Self> {
        let captures = monkey_regex()
            .captures(input)
            .with_context(|| anyhow!("regex didn't match"))?;
        let id = get_capture_parse!(captures, "id");
        let items = get_capture!(captures, "items")
            .split(",")
            .map(|s| s.trim().parse().map_err(From::from))
            .collect::<Result<Vec<_>>>()
            .with_context(|| anyhow!("unable to parse items"))?;
        let operation = Operation::from_str(get_capture!(captures, "operation"))?;
        let division = get_capture_parse!(captures, "division");
        let true_monkey = get_capture_parse!(captures, "true_monkey");
        let false_monkey = get_capture_parse!(captures, "false_monkey");

        Ok(Monkey {
            id,
            items,
            operation,
            division,
            true_monkey,
            false_monkey,
            counter: 0,
        })
    }

    // @return Vec<(items, destination)>
    fn throw(&mut self, modulo: u128) -> Vec<(u128, usize)> {
        self.items
            .drain(0..)
            .map(|item| {
                self.counter += 1;
                let worry = if PART2 {
                    self.operation.execute(item) % modulo
                } else {
                    self.operation.execute(item) / 3
                };
                if worry % (self.division as u128) == 0 {
                    (worry, self.true_monkey)
                } else {
                    (worry, self.false_monkey)
                }
            })
            .collect()
    }

    fn add(&mut self, item: u128) {
        self.items.push(item);
    }
}

pub fn run(inputs: impl Iterator<Item = String>) {
    let mut monkeys = inputs
        .fold(vec![vec![]], |mut acc, elem| {
            if elem.is_empty() {
                acc.push(vec![])
            } else {
                acc.last_mut().unwrap().push(elem)
            }
            acc
        })
        .into_iter()
        .map(|x| Monkey::from_str(&x.join("\n")))
        .collect::<Result<Vec<_>>>()
        .expect("invalid monkey");

    let modulo: u128 = monkeys
        .iter()
        .map(|monkey| monkey.division as u128)
        .product();

    // round
    for round in 1..=(if PART2 { 10000 } else { 20 }) {
        for monkey_id in 0..monkeys.len() {
            let actions = monkeys[monkey_id].throw(modulo);

            for (item, destination) in actions {
                monkeys[destination].add(item);
            }
        }

        if vec![1, 20, 1000].contains(&round) {
            println!("== After round {} ==", round);
            for monkey in monkeys.iter() {
                println!("monkey {}: {}", monkey.id, monkey.counter);
            }
            println!("");
        }
    }

    for monkey in monkeys.iter() {
        println!("monkey {}: {}", monkey.id, monkey.counter);
    }
}

#[test]
fn test_monkey_from_str() {
    let monkey = Monkey::from_str(
        r"
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
",
    )
    .unwrap();

    assert_eq!(
        monkey,
        Monkey {
            id: 0,
            items: vec![79, 98],
            operation: Operation::Multiply(19),
            division: 23,
            true_monkey: 2,
            false_monkey: 3,
            counter: 0,
        }
    );
}

#[test]
fn test_operation_from_str() {
    assert_eq!(Operation::from_str("old + 10").unwrap(), Operation::Add(10));
    assert_eq!(
        Operation::from_str("old * 10").unwrap(),
        Operation::Multiply(10)
    );
    assert_eq!(Operation::from_str("old * old").unwrap(), Operation::Square);
}

#[test]
fn test_monkey_throw() {
    let mut monkey = Monkey::from_str(
        r"
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
",
    )
    .unwrap();

    assert_eq!(monkey.throw(), vec![(500, 3), (620, 3)]);

    let mut monkey = Monkey::from_str(
        r"
Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0
    ",
    )
    .unwrap();

    assert_eq!(monkey.throw(), vec![(20, 0), (23, 0), (27, 0), (26, 0)]);

    let mut monkey = Monkey::from_str(
        r"
Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3",
    )
    .unwrap();

    assert_eq!(monkey.throw(), vec![(2080, 1), (1200, 3), (3136, 3)]);
}
