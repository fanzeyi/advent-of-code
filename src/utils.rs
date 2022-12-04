use std::io::{self, BufRead, BufReader, Result};

pub fn lines_from_stdin() -> impl Iterator<Item = Result<String>> {
    let stdin = io::stdin();
    let handle = BufReader::new(stdin.lock());
    handle.lines()
}
