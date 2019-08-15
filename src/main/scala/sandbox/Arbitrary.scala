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

      def withArgument1(arbitrary: Arbitrary[T]): ArbitraryFunction
    }

    def prop[T](gen: Generator.Gen[T], test: T => Result): ArbitraryFunction = new ArbitraryFunction { outer =>
      override def asResult: Result = test(Arbitrary.fromGenerator(gen).arbitrary)
    }

    def prop[T: Generator.Gen](test: T => Result): ArbitraryFunction1[T] = new ArbitraryFunction1[T] { outer =>
      override def asResult: Result = test(Arbitrary[T].arbitrary)

      override def withArgument1(arbitrary: Arbitrary[T]): ArbitraryFunction = new ArbitraryFunction {
        override def asResult: Result = test(arbitrary.arbitrary)
      }
    }
  }

  def apply[A](implicit gen: Gen[A], generatorConfig: GeneratorConfig, seed: Long): Arbitrary[A] = new Arbitrary[A] {
    override def arbitrary: A = {
      val res = gen.run(generatorConfig, seed).unsafeRunSync()
      if(generatorConfig.debug) res._1.foreach(println)
      res._3
    }
  }
  def fromGenerator[A](gen: Gen[A])(implicit generatorConfig: GeneratorConfig, seed: Long): Arbitrary[A] = new Arbitrary[A] {
    override def arbitrary: A = {
      val res = gen.run(generatorConfig, seed).unsafeRunSync()
      if(generatorConfig.debug) res._1.foreach(println)
      res._3
    }
  }
}