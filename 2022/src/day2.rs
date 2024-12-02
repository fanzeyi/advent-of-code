#[derive(Debug, PartialEq, Eq)]
enum Outcome {
    Win,
    Tie,
    Loss,
}

impl TryFrom<&str> for Outcome {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Outcome::*;

        match value {
            "X" => Ok(Loss),
            "Y" => Ok(Tie),
            "Z" => Ok(Win),
            _ => Err(()),
        }
    }
}

impl Outcome {
    fn score(&self) -> u64 {
        use Outcome::*;
        match self {
            Win => 6,
            Tie => 3,
            Loss => 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum RockPaperScissor {
    Rock,
    Paper,
    Scissor,
}

impl TryFrom<&str> for RockPaperScissor {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use RockPaperScissor::*;

        match value {
            "A" | "X" => Ok(Rock),
            "B" | "Y" => Ok(Paper),
            "C" | "Z" => Ok(Scissor),
            _ => Err(()),
        }
    }
}

impl RockPaperScissor {
    fn compete(&self, other: &RockPaperScissor) -> Outcome {
        use RockPaperScissor::*;

        if self == other {
            return Outcome::Tie;
        }

        match self {
            Rock => {
                if other == &Scissor {
                    Outcome::Win
                } else {
                    Outcome::Loss
                }
            }
            Paper => {
                if other == &Rock {
                    Outcome::Win
                } else {
                    Outcome::Loss
                }
            }
            Scissor => {
                if other == &Paper {
                    Outcome::Win
                } else {
                    Outcome::Loss
                }
            }
        }
    }

    fn satisfy(&self, outcome: &Outcome) -> Self {
        use RockPaperScissor::*;
        if outcome == &Outcome::Tie {
            return self.clone();
        }
        match self {
            Rock => {
                if outcome == &Outcome::Win {
                    Paper
                } else {
                    Scissor
                }
            }
            Paper => {
                if outcome == &Outcome::Win {
                    Scissor
                } else {
                    Rock
                }
            }
            Scissor => {
                if outcome == &Outcome::Win {
                    Rock
                } else {
                    Paper
                }
            }
        }
    }

    fn score(&self) -> u64 {
        use RockPaperScissor::*;
        match self {
            Rock => 1,
            Paper => 2,
            Scissor => 3,
        }
    }
}

pub fn run(inputs: impl Iterator<Item = String>) {
    let inputs = inputs.collect::<Vec<_>>();
    // part 1
    let total: u64 = inputs
        .clone()
        .into_iter()
        .map(|line| -> (RockPaperScissor, RockPaperScissor) {
            let (x, y) = line.split_once(' ').expect("malformed input");
            (
                x.try_into().expect("invalid rock paper scissor"),
                y.try_into().expect("invalid rock paper scissor"),
            )
        })
        .map(|(oppo, mine)| mine.compete(&oppo).score() + mine.score())
        .sum();
    println!("part 1 - total score: {}", total);

    let total: u64 = inputs
        .clone()
        .into_iter()
        .map(|line| -> (RockPaperScissor, Outcome) {
            let (x, y) = line.split_once(' ').expect("malformed input");
            (
                x.try_into().expect("invalid rock paper scissor"),
                y.try_into().expect("invalid rock paper scissor"),
            )
        })
        .map(|(oppo, outcome)| oppo.satisfy(&outcome).score() + outcome.score())
        .sum();
    println!("part 2 - total score: {}", total);
}
