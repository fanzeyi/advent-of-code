#[macro_export]
macro_rules! define {
    (inner $name: expr) => {
        paste::paste! {
            mod [<day $name>];
        }
    };
    ($limit: literal,) => {
        ::seq_macro::seq!(i in 1..=$limit {
            define!(inner i);
        });
    };

}

#[macro_export]
macro_rules! run {
    (inner $name: expr, $opt: expr, $inputs: expr) => {
        paste::paste! {
            if $opt == $name {
                [<day $name>]::run($inputs);
                return Ok(());
            }
        }
    };
    ($limit: literal, $opt: expr, $inputs: expr) => {
        ::seq_macro::seq!(i in 1..=$limit {
            run!(inner i, $opt, $inputs);
        })
    }
}

#[macro_export]
macro_rules! with_max_day {
    ($expand: path) => {
        with_max_day!($expand,);
    };
    ($expand: path, $($args: expr),*) => {
        // Change day here
        $expand!(6, $($args),*);
    };
}
