package sandbox

import cats.data.RWST
import cats.data.RWST._
import cats.effect.IO
import cats.implicits._
import cats.{Applicative, MonadError}
import shapeless.{::, Generic, HList, HNil}

import scala.util.Random

object Generator {

  case class GeneratorConfig(maxStringSize: Int, maxListSize: Int, maxSetSize: Int, maxGenTry: Int, debug: Boolean = false)

  type Gen[A] = RWST[IO, GeneratorConfig, List[String], Long, A]

  def rwst[A] = RWST[IO, GeneratorConfig, List[String], Long, A] _

  implicit val genLong: Gen[Long] = RWST((_, value: Long) => {
    val randValue = new Random(value).nextLong
    IO.pure((List(s"Seed $randValue, value $randValue"), randValue, randValue))
  })

  private def genBuild[T](randomGen: Random => T): Gen[T] = RWST((_, value: Long) => {
    val randValue = new Random(value)
    val seed = randValue.nextLong
    val randGenValue = randomGen(randValue)
    IO.pure((List(s"Seed: $seed, value $randGenValue"), seed, randGenValue))
  })

  implicit val genInt: Gen[Int] = genBuild(_.nextInt)
  implicit val genDouble: Gen[Double] = genBuild(_.nextDouble)
  implicit val genBoolean: Gen[Boolean] = genBuild(_.nextBoolean)
  implicit val genChar: Gen[Char] = genBuild(_.nextPrintableChar)

  implicit val genString: Gen[String] = for {
    config <- ask[IO, GeneratorConfig, List[String], Long]
    state <- get[IO, GeneratorConfig, List[String], Long]
    random = new Random(state)
    rest <- Range(0, random.nextInt(config.maxStringSize + 1) - 1).map(_ => genChar).toList.sequence
    _ <- tell[IO, GeneratorConfig, List[String], Long](List(s"Seed: $state, value in String: ${rest.mkString("")}"))
  } yield rest.mkString("")

  def genConst[T](value: T): Gen[T] = RWST((_, s) => IO.pure((List(s"Const value: $value"), s, value)))

  def genOneOf[T](head: Gen[T], rest: Gen[T]*): Gen[T] = for {
    state <- get[IO, GeneratorConfig, List[String], Long]
    randValue = new Random(state)
    values = head +: rest
    nextV <- values(randValue.nextInt(rest.size + 1))
    _ <- tell[IO, GeneratorConfig, List[String], Long](List(s"Seed: $state, value: $nextV"))
  } yield nextV

  implicit def genList[T](implicit genT: Gen[T]): Gen[List[T]] = for {
    config <- ask[IO, GeneratorConfig, List[String], Long]
    state <- get[IO, GeneratorConfig, List[String], Long]
    random = new Random(state)
    list <- Range(0, random.nextInt(config.maxListSize + 1) - 1).map(_ => genT).toList.sequence
    _ <- tell[IO, GeneratorConfig, List[String], Long](List(s"Seed: $state, value in List: ${list.mkString(", ")}"))
  } yield list

  implicit def genSet[T](implicit genT: Gen[T]): Gen[Set[T]] = {

    // not great something infinite.. :(
    def recAdd(rest: Set[T], size: Int, counter: Int): Gen[Set[T]] = if(size == 0) genConst(rest) else for {
      h <- genT
      _ <- if(counter > 0) Applicative[Gen].pure(()) else MonadError[Gen, Throwable].raiseError(new RuntimeException("Failed to produce not repeating set"))
      t <- if(rest.contains(h)) recAdd(rest, size, counter) else recAdd(rest, size - 1, counter - 1)
    } yield t + h

    for {
      config <- ask[IO, GeneratorConfig, List[String], Long]
      state <- get[IO, GeneratorConfig, List[String], Long]
      random = new Random(state)
      set <- recAdd(Set.empty, random.nextInt(config.maxListSize + 1) - 1, config.maxGenTry)
      _ <- tell[IO, GeneratorConfig, List[String], Long](List(s"Seed: $state, value in Set: ${set.mkString(", ")}"))
    } yield set
  }

  def genNonEmptyList[T](implicit genT: Gen[T]): Gen[List[T]] = for {
    head <- genT
    rest <- genList(genT)
  } yield head :: rest

  implicit val hNilGen: Gen[HNil] = genConst(HNil)
  implicit def hListGen[H, T <: HList](implicit hGen: Gen[H], tGen: Gen[T]): Gen[H :: T] = for {
    head <- hGen
    tail <- tGen
  } yield head :: tail

  implicit def caseClassGen[CC, HL <: HList](implicit genAux: Generic.Aux[CC, HL], gen: Gen[HL]): Gen[CC] = gen.map(genAux.from)

  implicit val genTuple0: Gen[Unit] = genConst(())

//  implicit def genTuple1[T1](implicit gen1: Gen[T1]): Gen[T1] = gen1

  implicit def genTuple2[T1, T2](implicit gen1: Gen[T1], gen2: Gen[T2]): Gen[(T1, T2)] = Applicative[Gen].map2(gen1, gen2)((_, _))

  implicit def genTuple3[T1, T2, T3](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3]): Gen[(T1, T2, T3)] = Applicative[Gen].map3(gen1, gen2, gen3)((_, _, _))

  implicit def genTuple4[T1, T2, T3, T4](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4]): Gen[(T1, T2, T3, T4)] = Applicative[Gen].map4(gen1, gen2, gen3, gen4)((_, _, _, _))

  implicit def genTuple5[T1, T2, T3, T4, T5](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5]): Gen[(T1, T2, T3, T4, T5)] = Applicative[Gen].map5(gen1, gen2, gen3, gen4, gen5)((_, _, _, _, _))

  implicit def genTuple6[T1, T2, T3, T4, T5, T6](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6]) = Applicative[Gen].map6(gen1, gen2, gen3, gen4, gen5, gen6)((_, _, _, _, _, _))

  implicit def genTuple7[T1, T2, T3, T4, T5, T6, T7](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7]) = Applicative[Gen].map7(gen1, gen2, gen3, gen4, gen5, gen6, gen7)((_, _, _, _, _, _, _))

  implicit def genTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8]) = Applicative[Gen].map8(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8)((_, _, _, _, _, _, _, _))

  implicit def genTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9]) = Applicative[Gen].map9(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)((_, _, _, _, _, _, _, _, _))

  implicit def genTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10]) = Applicative[Gen].map10(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10)((_, _, _, _, _, _, _, _, _, _))

  implicit def genTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11]) = Applicative[Gen].map11(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11)((_, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12]) = Applicative[Gen].map12(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12)((_, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13]) = Applicative[Gen].map13(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13)((_, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14]) = Applicative[Gen].map14(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14)((_, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15]) = Applicative[Gen].map15(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16]) = Applicative[Gen].map16(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16], gen17: Gen[T17]) = Applicative[Gen].map17(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16], gen17: Gen[T17], gen18: Gen[T18]) = Applicative[Gen].map18(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17, gen18)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16], gen17: Gen[T17], gen18: Gen[T18], gen19: Gen[T19]) = Applicative[Gen].map19(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17, gen18, gen19)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16], gen17: Gen[T17], gen18: Gen[T18], gen19: Gen[T19], gen20: Gen[T20]) = Applicative[Gen].map20(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17, gen18, gen19, gen20)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16], gen17: Gen[T17], gen18: Gen[T18], gen19: Gen[T19], gen20: Gen[T20], gen21: Gen[T21]) = Applicative[Gen].map21(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17, gen18, gen19, gen20, gen21)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

  implicit def genTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit gen1: Gen[T1], gen2: Gen[T2], gen3: Gen[T3], gen4: Gen[T4], gen5: Gen[T5], gen6: Gen[T6], gen7: Gen[T7], gen8: Gen[T8], gen9: Gen[T9], gen10: Gen[T10], gen11: Gen[T11], gen12: Gen[T12], gen13: Gen[T13], gen14: Gen[T14], gen15: Gen[T15], gen16: Gen[T16], gen17: Gen[T17], gen18: Gen[T18], gen19: Gen[T19], gen20: Gen[T20], gen21: Gen[T21], gen22: Gen[T22]) = Applicative[Gen].map22(gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9, gen10, gen11, gen12, gen13, gen14, gen15, gen16, gen17, gen18, gen19, gen20, gen21, gen22)((_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

}