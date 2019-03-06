package PurelyFunctionalState

case class State[S,+A](run: S => (A,S)){

  def flatMap[B](f: A => State[S,B]): State[S,B] = State(state => {
    val (value, newState) = run(state)
    f(value).run(newState)
  })

  def map[B](f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](rb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => rb.map(b => f(a,b)))
}


object State {

  def get[S]: State[S, S] = State(s => (s,s))

  def set[S](s: S): State[S, Unit] = State(_ => ((),s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def unit[S,A](a: A): State[S,A] =
    State(state => (a,state))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((x,z) => x.map2(z)(_ :: _))
}
