package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (random, newRng) = rng.nextInt
    if (random < 0)
      (-(random + 1), newRng)
    else (random, newRng)
  }

  // 6.2 rng -> [0, 1)
  def double(rng: RNG): (Double, RNG) = {
    val (random, newRng) = nonNegativeInt(rng)

    (random / (Int.MaxValue.toDouble + 1.0), newRng)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = (1 to count).foldLeft((List.empty[Int], rng)) { case ((ls, r), _) =>
    val (i, r2) = r.nextInt
    (i :: ls, r2)
  }

  // 6.5
  val _double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble - 1))

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rnd => {
      val (a, rnd2) = ra(rnd)
      val (b, rnd3) = rb(rnd2)
      (f(a, b), rnd3)
    }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rnd => fs.reverse.foldLeft((List.empty[A], rnd)) {
    case ((ls, r), ra) =>
      val (a, r2) = ra(r)
      (a :: ls, r2)
  }

  def intsSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs.reverse.foldLeft(unit(List.empty[A])) {
    case (acc, ra) => map2(ra, acc)(_ :: _)
  }


  def nonNegativeLessThan0(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan0(n)(rng)
  }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    val rb = g(a)
    rb(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(int) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      unit(mod)
    else nonNegativeLessThan(n)
  }

  // 6.9
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    _map(rb)(b => f(a, b))
  }
}

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map1[B](f: A => B): State[S, B] = State({ s =>
    val (a, sa) = run(s)
    (f(a), sa)
  })

  // 6.10
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    val sb = f(a)
    sb.run(s1)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def exec(input: Input): Machine =
    if (candies <= 0) this
    else (locked, input) match {
      case (true, Coin) => Machine(false, candies, coins + 1)
      case (false, Turn) => Machine(true, candies - 1, coins)
      case _ => this
    }
}

object Machine {

  import State._

  def simulateMachineViaFold(inputs: List[Input]): State[Machine, (Int, Int)] = State(s => {
    val m = inputs.foldLeft(s)((st, in) => st.exec(in))
    ((m.candies, m.coins), m)
  })

  // 6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val transitions: List[State[Machine, Unit]] = inputs.map(i => State((m: Machine) => {
      val m2 = m.exec(i)
      ((), m2)
    }))
    for {
      _ <- sequence(transitions)
      m <- get[Machine]
    } yield (m.candies, m.coins)
  }

  private def update(i: Input): Machine => Machine = _.exec(i)

  def simulateMachineReference(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val toState: Input => State[Machine, Unit] = (modify[Machine] _) compose update
    val transitions: List[State[Machine, Unit]] = inputs map (toState)
    for {
      _ <- sequence(transitions)
      s <- get
    } yield (s.coins, s.candies)
  }
}

object State {
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List.empty[A])) {
      case (acc, sa) => sa.map2[List[A], List[A]](acc)(_ :: _)
    }
}

object TestRNG extends App {

  import RNG._

  val rnd: RNG = Simple(42)

  val m100 = (1 to 100 * 100000).foldLeft((Map.empty[Int, Int], rnd)) { case ((m, r), _) =>
    val (i100, r2) = nonNegativeLessThan(100)(r)
    (m ++ Map(i100 -> (m.getOrElse(i100, 0) + 1)), r2)
  }._1

  val x = m100.values.map(_.toDouble).toArray
  val mean = x.sum / x.length
  val stdDev = Math.sqrt((x.map(_ - mean)
    .map(t => t * t).sum) / x.length)
  println(s"$mean $stdDev")
}

object TestMachine extends App {

  import Machine._

  val machine = Machine(true, 10, 0)
  val inputs = List(
    Coin, Turn,
    Coin, Turn,
    Coin, Turn,
    Coin, Turn,
    Coin, Turn,
    Turn,
    Turn,
    Turn,
    Coin,
    Coin,
    Coin
  )

  println(simulateMachine(inputs).run(machine))
  println(simulateMachineViaFold(inputs).run(machine))
  println(simulateMachineReference(inputs).run(machine))
}
