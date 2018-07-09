#[derive(Debug, Clone)]
pub enum Term {
  True,
  False,
  If(Box<Term>, Box<Term>, Box<Term>),
  Zero,
  Succ(Box<Term>),
  Pred(Box<Term>),
  IsZero(Box<Term>),
}

pub fn is_numeric_val(term: &Term) -> bool {
  match term {
    Term::Zero => true,
    Term::Succ(t1) => is_numeric_val(t1),
    _ => false,
  }
}
