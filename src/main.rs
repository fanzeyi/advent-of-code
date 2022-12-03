use argh::FromArgs;

mod support;

with_max_day!(define);

#[derive(FromArgs, Debug)]
/// AoC 2022
struct Options {
    #[argh(positional)]
    day: u32,
}

fn main() {
    let options: Options = argh::from_env();
    with_max_day!(run, options.day);
}
