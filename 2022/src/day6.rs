use std::collections::HashSet;

fn all_unique(chars: &[char]) -> bool {
    chars.iter().map(|x| *x).collect::<HashSet<char>>().len() == chars.len()
}

pub fn run(inputs: impl Iterator<Item = String>) {
    let line = inputs
        .collect::<Vec<_>>()
        .join("")
        .chars()
        .collect::<Vec<_>>();

    let result = line
        .windows(4)
        .enumerate()
        .find(|(_, chars)| all_unique(chars))
        .map(|(i, _)| i)
        .expect("to have result");

    println!("part 1: {}", result + 4);

    let result = line
        .windows(14)
        .enumerate()
        .find(|(_, chars)| all_unique(chars))
        .map(|(i, _)| i)
        .expect("to have result");
    println!("part 2: {}", result + 14);
}
