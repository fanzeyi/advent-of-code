use std::{
    fs::File,
    io::{BufRead, BufReader, Result},
};

pub fn lines_from_input(day: u32) -> Result<impl Iterator<Item = String>> {
    let input = File::open(format!("inputs/day{}", day))?;
    let reader = BufReader::new(input);
    Ok(reader.lines().filter_map(|line| line.ok()))
}
