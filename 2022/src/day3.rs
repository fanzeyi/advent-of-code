use std::collections::HashSet;

fn rank(c: &char) -> u32 {
    if c.is_uppercase() {
        (*c as u32) - ('A' as u32) + 27
    } else {
        (*c as u32) - ('a' as u32) + 1
    }
}

pub fn run(inputs: impl Iterator<Item = String>) {
    let inputs = inputs.collect::<Vec<_>>();
    let total: u32 = inputs
        .clone()
        .into_iter()
        .map(|line| {
            let (left, right) = line.split_at(line.len() / 2);

            let left = left.chars().collect::<HashSet<_>>();
            let right = right.chars().collect::<HashSet<_>>();

            left.intersection(&right).next().map(rank).unwrap_or(0)
        })
        .sum();

    println!("total priority: {}", total);

    let total: u32 = inputs
        .chunks(3)
        .map(|group| {
            group
                .iter()
                .map(|line| line.chars().collect::<HashSet<_>>())
                .reduce(|acc, elem| acc.intersection(&elem).map(|x| *x).collect())
                .expect("must have interaction")
                .iter()
                .next()
                .map(rank)
                .unwrap_or(0)
        })
        .sum();
    println!("total priority: {}", total);
}
