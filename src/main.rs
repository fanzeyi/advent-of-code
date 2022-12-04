use argh::FromArgs;

use crate::utils::lines_from_input;

mod support;
mod utils;

with_max_day!(define);

#[derive(FromArgs, Debug)]
/// AoC 2022
struct Options {
    #[argh(positional)]
    day: u32,
}

fn main() -> std::io::Result<()> {
    let options: Options = argh::from_env();
    let input = lines_from_input(options.day)?;
    with_max_day!(run, options.day, input);

    Ok(())
}
