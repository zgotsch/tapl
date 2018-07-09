use arith::*;

enum Evaluation {
  More(Term),
  Done(Term),
}

impl Evaluation {
  fn map<F: Fn(&Term) -> Term>(&self, f: F) -> Self {
    match self {
      Evaluation::More(t) => Evaluation::More(f(t)),
      Evaluation::Done(t) => Evaluation::Done(f(t)),
    }
  }
}

fn eval_step(term: &Term) -> Evaluation {
  match term {
    Term::If(box Term::True, t2, _t3) => Evaluation::More(*t2.clone()),
    Term::If(box Term::False, _t2, t3) => Evaluation::More(*t3.clone()),
    Term::If(t1, t2, t3) => {
      eval_step(t1).map(|t1_next| Term::If(box t1_next.clone(), t2.clone(), t3.clone()))
    }
    Term::Succ(t1) => eval_step(t1).map(|t1_next| Term::Succ(box t1_next.clone())),
    Term::Pred(box Term::Zero) => Evaluation::More(Term::Zero),
    Term::Pred(box Term::Succ(tt1)) if is_numeric_val(tt1) => Evaluation::More(*tt1.clone()),
    Term::Pred(t1) => eval_step(t1).map(|t1_next| Term::Pred(box t1_next.clone())),
    Term::IsZero(box Term::Zero) => Evaluation::More(Term::True),
    Term::IsZero(box Term::Succ(tt1)) if is_numeric_val(tt1) => Evaluation::More(Term::False),
    Term::IsZero(t1) => eval_step(t1).map(|t1_next| Term::IsZero(box t1_next.clone())),
    _ => Evaluation::Done(term.clone()),
  }
}

pub fn eval(term: &Term) -> Term {
  match eval_step(term) {
    Evaluation::More(t) => eval(&t),
    Evaluation::Done(t) => t,
  }
}
