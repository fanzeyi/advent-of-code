use std::io::{self, BufRead, BufReader};

pub fn lines_from_stdin() -> impl Iterator<Item = String> {
    let stdin = io::stdin();
    let handle = BufReader::new(stdin.lock());
    handle.lines().filter_map(|line| line.ok())
}
