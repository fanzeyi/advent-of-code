use crate::utils::lines_from_stdin;

pub fn run() {
    let mut result: Vec<u64> = lines_from_stdin().fold(vec![0], |mut acc, elem| {
        if let Ok(line) = elem {
            if line.is_empty() {
                acc.push(0);
            } else {
                if let Ok(number) = line.parse::<u64>() {
                    *acc.last_mut().unwrap() += number;
                }
            }
        }
        acc
    });
    result.sort_by(|x, y| y.cmp(x));

    // part 1
    if let Some(max) = result.first() {
        println!("largest calories {} ", max);
    }

    //part 2
    let sum: u64 = result.iter().take(3).sum();
    println!("top 3 sum: {}", sum);
}
