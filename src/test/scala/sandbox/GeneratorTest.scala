package sandbox

import org.specs2.Specification
import org.specs2.execute.Result
import sandbox.Arbitrary.ArbitrarySpec

class GeneratorTest extends Specification with ArbitrarySpec[Result] {override def is =
  s2"""
    Generate an arbitrary instance of
       int $genIntTest
       char $genCharTest
       string $genStringTest
       long $genLongTest
       double $genDoubleTest
       boolean $genBooleanTest
       constant value $genConstTest
       one of value $genOneOfTest

    Generate list of random typed values $genListTest
    Generate non empty list of random typed values $genListNonEmptyTest

    Generate an arbitrary constant pair that should be not equal $arbitraryNotEqual
  """

  val genIntTest = prop((res: Int) => res >= Int.MinValue and res <= Int.MaxValue).asResult
  val genLongTest = prop((res: Long) => res >= Long.MinValue and res <= Long.MaxValue).asResult
  val genDoubleTest = prop((res: Double) => res >= Double.MinValue and res <= Double.MaxValue).asResult
  val genBooleanTest = prop((res: Boolean) => (res must beTrue) or (res must beFalse)).asResult
  val genCharTest = prop((res: Char) => res.toInt >= 33 && res.toInt <= 127).asResult
  val genStringTest = prop((res: String) => res.length <= generatorConfig.maxStringSize && res.forall(sym => sym.toInt >= 33 && sym.toInt <= 127)).asResult
  val genListTest = prop((res: List[Int]) => res.length <= generatorConfig.maxListSize).asResult
  val genListNonEmptyTest = prop((res: List[Int]) => res.length <= generatorConfig.maxListSize && res.nonEmpty)
        .withArguments(Arbitrary.fromGenerator(Generator.genNonEmptyList[Int])).asResult

  val arbitraryNotEqual = prop[Int, Int]((a: Int, b: Int) => a != b).withArguments(
    Arbitrary.fromGenerator(Generator.genConst[Int](1)),
    Arbitrary.fromGenerator(Generator.genConst[Int](2))
  ).asResult

  sealed trait ConstTest
  case object Test extends ConstTest
  val genConstTest = prop(Generator.genConst(Test), (value: ConstTest) => value must_=== value).asResult

  sealed trait TestOneOf
  case object TestOneOfA extends TestOneOf
  case object TestOneOfB extends TestOneOf
  val genOneOfTest = prop(Generator.genOneOf(Generator.genConst[TestOneOf](TestOneOfA), Generator.genConst[TestOneOf](TestOneOfB)),
    (value: TestOneOf) => (value must_=== TestOneOfA) or (value must_=== TestOneOfB)).asResult
}
