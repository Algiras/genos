import cats.Monad
import sandbox.Seed
import cats.syntax.flatMap._
import cats.syntax.functor._

type StateReader[C, S, A] = (C, S) => (S, A)

object StateReader {
  def apply[C, S, A](fn: (C, S) => (S, A)): StateReader[C, S, A] = fn

  def get[C, S]: StateReader[C, S, C] = StateReader((c, s) => (s, c))
}

implicit def stateMonad[C, S]: Monad[({type L[A] = StateReader[C, S, A]})#L] = new Monad[({type L[A] = StateReader[C, S, A]})#L] {
  override def pure[A](x: A): StateReader[C, S, A] = StateReader[C, S, A]((_, state) => (state, x))

  override def flatMap[A, B](fa: StateReader[C, S, A])(f: A => StateReader[C, S, B]): StateReader[C, S, B] = StateReader[C, S, B]((config, state) => {
    val (s, a) = fa(config, state)
    f(a)(config, s)
  })

  override def tailRecM[A, B](a: A)(f: A => StateReader[C, S, Either[A, B]]): StateReader[C, S, B] = flatMap(f(a)){
    case Right(endCase) => pure(endCase)
    case Left(loopCase) => tailRecM[A, B](loopCase)(f)
  }
}

case class Config(prependToStr: String)

type Gen[A] = StateReader[Config, Seed, A]

val genGet: Gen[Config] = StateReader.get[Config, Seed]

def genBuild[T](randomGen: Seed => T): Gen[T] = StateReader((_, value: Seed) => {
  (value.next, randomGen(value.next))
})

def choose(low: Long, high: Long): Gen[Long] =
  genBuild(seed => low + Math.abs(seed.long) % (high - low))

val genLong: Gen[Long] = StateReader((_, seed) => (seed.next, seed.long))
val genInt: Gen[Int] = genBuild(seed => seed.long.toInt)
val genDouble: Gen[Double] = genBuild(_.long.toDouble)
val genBoolean: Gen[Boolean] = genBuild(_.long > 0L)
val genChar: Gen[Char] = choose(low = 33, high = 127).map(_.toChar)

def genConst[T](value: T): Gen[T] = StateReader((_, state) => (state, value))

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

def genString[T] = for {
  config <- genGet
  ls <- genList(genChar)
} yield config.prependToStr ++ ls


genString(Config(prependToStr = "test-"), Seed(2L))