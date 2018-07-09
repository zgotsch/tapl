#![feature(box_syntax, box_patterns)]

mod arith;
mod big_step;
mod small_step;

use arith::Term;

fn main() {
    let tests = vec![
        Term::If(
            box Term::True,
            box Term::Zero,
            box Term::Succ(box Term::Zero),
        ),
        Term::If(
            box Term::False,
            box Term::Zero,
            box Term::Succ(box Term::Zero),
        ),
        Term::If(
            box Term::IsZero(box Term::Zero),
            box Term::Succ(box Term::Zero),
            box Term::Zero,
        ),
        Term::IsZero(box Term::Zero),
        Term::Succ(box Term::Pred(box Term::Succ(box Term::Zero))),
        Term::Succ(box Term::IsZero(box Term::Succ(box Term::Zero))),
    ];

    for test in tests {
        println!("{:?} =>", test);
        println!("\tSmall step eval: {:?}", small_step::eval(&test));
        println!("\tBig step eval: {:?}", big_step::eval(&test));
        println!("");
    }
}
