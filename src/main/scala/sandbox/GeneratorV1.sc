import cats.Monad
import sandbox.Seed
import cats.syntax.flatMap._
import cats.syntax.functor._

type State[S, A] = S => (S, A)

object State {
  def apply[S, A](fn: S => (S, A)): State[S, A] = fn
}

implicit def stateMonad[S]: Monad[({type L[A] = State[S, A]})#L] = new Monad[({type L[A] = State[S, A]})#L] {
  override def pure[A](x: A): State[S, A] = State[S, A]((_, x))

  override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = State[S, B](state => {
    val (s, a) = fa(state)
    f(a)(s)
  })

  override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] = flatMap(f(a)){
    case Right(endCase) => pure(endCase)
    case Left(loopCase) => tailRecM[A, B](loopCase)(f)
  }
}

type Gen[A] = State[Seed, A]

def genBuild[T](randomGen: Seed => T): Gen[T] = State((value: Seed) => {
  (value.next, randomGen(value.next))
})

def choose(low: Long, high: Long): Gen[Long] =
  genBuild(seed => low + Math.abs(seed.long) % (high - low))

val genLong: Gen[Long] = State(seed => (seed.next, seed.long))
val genInt: Gen[Int] = genBuild(seed => seed.long.toInt)
val genDouble: Gen[Double] = genBuild(_.long.toDouble)
val genBoolean: Gen[Boolean] = genBuild(_.long > 0L)
val genChar: Gen[Char] = choose(low = 33, high = 127).map(_.toChar)

def genConst[T](value: T): Gen[T] = State((_, value))

def genOneOf[T](head: Gen[T], rest: Gen[T]*): Gen[T] = {
  val values = head +: rest
  for {
    choice <- choose(0, rest.size)
    nextV <- values(choice.toInt)
  } yield nextV
}

def genList[T](gen: Gen[T]): Gen[List[T]] = for {
  isEmpty <- genBoolean
  ls <- if (isEmpty)
    genConst(List.empty[T])
  else
    for {
      head <- gen
      tail <- genList(gen)
    } yield head :: tail
} yield ls

def genSet[T](gen: Gen[T]): Gen[Set[T]] = for {
  isEmpty <- genBoolean
  set <- if (isEmpty) genConst[Set[T]](Set.empty)
    else
      for {
        head <- gen
        tail <- genSet(gen)
      } yield tail + head
} yield set

def genString[T] = genList(genChar).map(_.mkString(""))


genString(Seed(20L))

type StackState[A] = State[List[Int], A]
def StackState[A](fn: List[Int] => (List[Int], A)): StackState[A] = State[List[Int], A](fn)
def pop = StackState(state => (state.tail, state.head))
def add(value: Int) = StackState(state => (state :+ value, ()))
val c = for {
  b <- pop
  _ <- add(b)
  _ <- add(b)
} yield ()
c(List(1,2,3)) // (List(2, 3, 1, 1),())