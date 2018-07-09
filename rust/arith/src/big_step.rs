use arith::*;

pub fn eval(term: &Term) -> Term {
  match term {
    Term::If(t1, t2, t3)
      if match eval(t1) {
        Term::True => true,
        Term::False => true,
        _ => false,
      } =>
    {
      // yuck, rematch to prune cases
      match eval(t1) {
        Term::True => eval(t2),
        Term::False => eval(t3),
        _ => panic!("Can't happen!"),
      }
    }
    Term::Succ(t1) if is_numeric_val(&eval(t1)) => Term::Succ(box eval(t1)),
    Term::Pred(t1)
      if match eval(t1) {
        Term::Zero => true,
        _ => false,
      } =>
    {
      Term::Zero
    }
    Term::Pred(t1)
      if match eval(t1) {
        Term::Succ(_) => true,
        _ => false,
      } =>
    {
      // yuck, rematch to bind variable
      match t1 {
        box Term::Succ(box tt1) => tt1.clone(),
        _ => panic!("Can't happen"),
      }
    }
    Term::IsZero(t1)
      if match eval(t1) {
        Term::Zero => true,
        _ => false,
      } =>
    {
      Term::True
    }
    Term::IsZero(t1)
      if match eval(t1) {
        Term::Succ(_) => true,
        _ => false,
      } =>
    {
      Term::False
    }
    _ => term.clone(),
  }
}
