import cats.{Monad, Monoid}
import sandbox.Seed
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.list._

type StateReaderWriter[C, S, W, A] = (C, S) => (W, S, A)

object StateReaderWriter {
  def apply[C, S, W, A](fn: (C, S) => (W, S, A)): StateReaderWriter[C, S, W, A] = fn

  def ask[C, W: Monoid, S]: StateReaderWriter[C, S, W, C] = StateReaderWriter((c, s) => (Monoid[W].empty, s, c))

  def tell[C, W: Monoid, S](message: W): StateReaderWriter[C, S, W, Unit] = StateReaderWriter((c, s) => (message, s, ()))
}

implicit def stateMonad[C, W: Monoid, S]: Monad[({type L[A] = StateReaderWriter[C, S, W, A]})#L] = new Monad[({type L[A] = StateReaderWriter[C, S, W, A]})#L] {
  override def pure[A](x: A): StateReaderWriter[C, S, W, A] = StateReaderWriter[C, S, W, A]((_, state) => (Monoid[W].empty, state, x))

  override def flatMap[A, B](fa: StateReaderWriter[C, S, W, A])(f: A => StateReaderWriter[C, S, W, B]): StateReaderWriter[C, S, W, B] = StateReaderWriter[C, S, W, B]((config, state) => {
    val (w, s, a) = fa(config, state)
    val (w2, s2, a2)  = f(a)(config, s)
    (Monoid[W].combine(w, w2), s2, a2)
  })

  override def tailRecM[A, B](a: A)(f: A => StateReaderWriter[C, S, W, Either[A, B]]): StateReaderWriter[C, S, W, B] = flatMap(f(a)){
    case Right(endCase) => pure(endCase)
    case Left(loopCase) => tailRecM[A, B](loopCase)(f)
  }
}

case class Config(prependToStr: String)

type Gen[A] = StateReaderWriter[Config, Seed, List[String], A]

val genAsk: Gen[Config] = StateReaderWriter.ask[Config, List[String], Seed]
def genTell(message: List[String]): Gen[Unit] = StateReaderWriter.tell[Config, List[String], Seed](message)

def genBuild[T](randomGen: Seed => T): Gen[T] = StateReaderWriter((_, value: Seed) => {
  (List.empty, value.next, randomGen(value.next))
})

def choose(low: Long, high: Long): Gen[Long] =
  genBuild(seed => low + Math.abs(seed.long) % (high - low))

val genLong: Gen[Long] = StateReaderWriter((_, seed) => (List.empty, seed.next, seed.long))
val genInt: Gen[Int] = genBuild(seed => seed.long.toInt)
val genDouble: Gen[Double] = genBuild(_.long.toDouble)
val genBoolean: Gen[Boolean] = genBuild(_.long > 0L)
val genChar: Gen[Char] = for {
  _ <- genTell(List("Generating Char"))
  c <- choose(low = 33, high = 127).map(_.toChar)
} yield c

def genConst[T](value: T): Gen[T] = StateReaderWriter((_, state) => (List.empty, state, value))

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
  config <- genAsk
  _ <- genTell(List("Generating String"))
  ls <- genList(genChar)
} yield config.prependToStr ++ ls


genString(Config(prependToStr = "test-"), Seed(2L))
