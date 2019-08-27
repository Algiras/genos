package sandbox

import sandbox.Generator.{Gen, GeneratorConfig}

import scala.util.Random

trait Arbitrary[T] {
  def arbitrary: T
}

object Arbitrary {
  trait ArbitrarySpec[Result] {
    implicit def generatorConfig: GeneratorConfig = GeneratorConfig(
      maxStringSize = 10,
      maxListSize = 10,
      maxSetSize = 10,
      maxGenTry = 500,
      debug = true
    )

    implicit def seed: Long = new Random().nextLong()

    trait ArbitraryFunction {
      def asResult: Result
    }

    trait ArbitraryFunction1[T] extends ArbitraryFunction {
      def asResult: Result

      def withArguments(arbitrary: Arbitrary[T]): ArbitraryFunction
    }


    trait ArbitraryFunction2[A, B] extends ArbitraryFunction {
      def asResult: Result

      def withArgument1(arbitrary: Arbitrary[A]): ArbitraryFunction1[B]
      def withArgument2(arbitrary2: Arbitrary[B]): ArbitraryFunction1[A]
      def withArguments(arbitrary: Arbitrary[A], arbitrary2: Arbitrary[B]): ArbitraryFunction
    }

    def prop[T](gen: Generator.Gen[T], test: T => Result): ArbitraryFunction = new ArbitraryFunction { outer =>
      override def asResult: Result = test(Arbitrary.fromGenerator(gen).arbitrary)
    }

    def prop[T: Generator.Gen](test: T => Result): ArbitraryFunction1[T] = new ArbitraryFunction1[T] { outer =>
      override def asResult: Result = test(Arbitrary[T].arbitrary)

      override def withArguments(arbitrary: Arbitrary[T]): ArbitraryFunction = new ArbitraryFunction {
        override def asResult: Result = test(arbitrary.arbitrary)
      }
    }

    def prop[A: Generator.Gen, B: Generator.Gen](test: (A, B) => Result): ArbitraryFunction2[A, B] = new ArbitraryFunction2[A, B] { outer =>
      override def asResult: Result = test(Arbitrary[A].arbitrary, Arbitrary[B].arbitrary)

      override def withArgument1(arbitrary: Arbitrary[A]): ArbitraryFunction1[B] = new ArbitraryFunction1[B] {
        override def asResult: Result = test(arbitrary.arbitrary, Arbitrary[B].arbitrary)

        override def withArguments(arbitrary2: Arbitrary[B]): ArbitraryFunction = new ArbitraryFunction {
          override def asResult: Result = test(arbitrary.arbitrary, arbitrary2.arbitrary)
        }
      }

      override def withArgument2(arbitrary2: Arbitrary[B]): ArbitraryFunction1[A] = new ArbitraryFunction1[A] {
        override def asResult: Result = test(Arbitrary[A].arbitrary, arbitrary2.arbitrary)

        override def withArguments(arbitrary: Arbitrary[A]): ArbitraryFunction = new ArbitraryFunction {
          override def asResult: Result = test(arbitrary.arbitrary, arbitrary2.arbitrary)
        }
      }

      override def withArguments(arbitrary: Arbitrary[A], arbitrary2: Arbitrary[B]): ArbitraryFunction = new ArbitraryFunction {
        override def asResult: Result = test(arbitrary.arbitrary, arbitrary2.arbitrary)
      }
    }
  }

  def apply[A](implicit gen: Gen[A], generatorConfig: GeneratorConfig, seed: Long): Arbitrary[A] = new Arbitrary[A] {
    override def arbitrary: A = {
      val res = gen.run(generatorConfig, Seed(seed)).unsafeRunSync()
      if(generatorConfig.debug) res._1.foreach(println)
      res._3
    }
  }
  def fromGenerator[A](gen: Gen[A])(implicit generatorConfig: GeneratorConfig, seed: Long): Arbitrary[A] = new Arbitrary[A] {
    override def arbitrary: A = {
      val res = gen.run(generatorConfig, Seed(seed)).unsafeRunSync()
      if(generatorConfig.debug) res._1.foreach(println)
      res._3
    }
  }
}