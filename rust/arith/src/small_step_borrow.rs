#[derive(Debug)]
pub enum Term<'a> {
  True,
  False,
  If(&'a Term<'a>, &'a Term<'a>, &'a Term<'a>),
  Zero,
  Succ(&'a Term<'a>),
  Pred(&'a Term<'a>),
  IsZero(&'a Term<'a>),
}

impl<'a> Clone for Term<'a> {
  fn clone(&self) -> Term<'a> {
    match self {
      Term::True => Term::True,
      Term::False => Term::False,
      Term::If(t1, t2, t3) => Term::If(&t1.clone(), &t2.clone(), &t3.clone()),
      Term::Zero => Term::Zero,
      Term::Succ(t1) => Term::Succ(&t1.clone()),
      Term::Pred(t1) => Term::Pred(&t1.clone()),
      Term::IsZero(t1) => Term::IsZero(&t1.clone()),
    }
  }
}

// // Like clone but gives a new lifetime
// fn deep_clone<'a, 'b>(t: &'a Term<'a>) -> Term<'b> {
//   match t {
//     Term::True => Term::True,
//     Term::False => Term::False,
//     Term::If(t1, t2, t3) => Term::If(deep_clone(t1), deep_clone(t2), deep_clone(t3)),
//     Term::Zero => Term::Zero,
//     Term::Succ(t1) => Term::Succ(deep_clone(t1)),
//     Term::Pred(t1) => Term::Pred(deep_clone(t2)),
//     Term::IsZero(t1) => Term::IsZero(deep_clone(t3))
//   }
// }

enum Evaluation<'a> {
  More(Term<'a>),
  Done(Term<'a>),
}

impl<'a> Evaluation<'a> {
  fn map<F: Fn(Term<'a>) -> Term<'a>>(self, f: F) -> Evaluation<'a> {
    match self {
      Evaluation::More(t) => Evaluation::More(f(t)),
      Evaluation::Done(t) => Evaluation::Done(f(t)),
    }
  }
}

fn is_numeric_val(term: &Term) -> bool {
  match term {
    Term::Zero => true,
    Term::Succ(t1) => is_numeric_val(t1),
    _ => false,
  }
}

fn eval_step<'a>(term: &'a Term<'a>) -> Evaluation<'a> {
  match term {
    Term::If(&Term::True, t2, ..) => Evaluation::More((*t2).clone()),
    Term::If(&Term::False, .., t3) => Evaluation::More((*t3).clone()),
    Term::If(t1, t2, t3) => {
      eval_step(t1).map(|t1_next| Term::If(&t1_next.clone(), &t2.clone(), &t3.clone()))
    }
    Term::Succ(t1) => eval_step(t1).map(|t1_next| Term::Succ(&t1_next.clone())),
    Term::Pred(&Term::Zero) => Evaluation::More(Term::Zero),
    Term::Pred(&Term::Succ(tt1)) if is_numeric_val(tt1) => Evaluation::More(tt1.clone()),
    Term::Pred(t1) => eval_step(t1).map(|t1_next| Term::Pred(&t1_next.clone())),
    Term::IsZero(&Term::Zero) => Evaluation::More(Term::True),
    Term::IsZero(&Term::Succ(tt1)) if is_numeric_val(tt1) => Evaluation::More(Term::False),
    Term::IsZero(t1) => eval_step(t1).map(|t1_next| Term::IsZero(&t1_next.clone())),
    _ => Evaluation::Done(term.clone()),
  }
}

pub fn eval<'a>(term: &'a Term) -> Term<'a> {
  match eval_step(term) {
    Evaluation::More(t) => eval(&t),
    Evaluation::Done(t) => t,
  }
}
