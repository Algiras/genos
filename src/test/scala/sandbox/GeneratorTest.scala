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

     Generate Case Class $caseClassTest
     Generate Tuple $tupleTest

    Generate list of random typed values $genListTest
    Generate non empty list of random typed values $genListNonEmptyTest
  """

  val genIntTest = prop((res: Int) => res >= Int.MinValue and res <= Int.MaxValue).asResult
  val genLongTest = prop((res: Long) => res >= Long.MinValue and res <= Long.MaxValue).asResult
  val genDoubleTest = prop((res: Double) => res >= Double.MinValue and res <= Double.MaxValue).asResult
  val genBooleanTest = prop((res: Boolean) => (res must beTrue) or (res must beFalse)).asResult
  val genCharTest = prop((res: Char) => res.toInt >= 33 && res.toInt < 127).asResult
  val genStringTest = prop((res: String) => res.length <= generatorConfig.maxStringSize && res.forall(sym => sym.toInt >= 33 && sym.toInt < 127)).asResult
  val genListTest = prop((res: List[Int]) => res.length <= generatorConfig.maxListSize).asResult
  val genListNonEmptyTest = prop((res: List[Int]) => res.length <= generatorConfig.maxListSize && res.nonEmpty)
        .withArgument1(Arbitrary.fromGenerator(Generator.genNonEmptyList[Int])).asResult

  sealed trait ConstTest
  case object Test extends ConstTest
  val genConstTest = prop(Generator.genConst(Test), (value: ConstTest) => value must_=== value).asResult

  sealed trait TestOneOf
  case object TestOneOfA extends TestOneOf
  case object TestOneOfB extends TestOneOf
  val genOneOfTest = prop(Generator.genOneOf(Generator.genConst[TestOneOf](TestOneOfA), Generator.genConst[TestOneOf](TestOneOfB)),
    (value: TestOneOf) => (value must_=== TestOneOfA) or (value must_=== TestOneOfB)).asResult

  case class User(name: String, age: Int)
  val caseClassTest: Result = prop((res: User) => {
    true must_=== true
  }).asResult


  val tupleTest = prop((res: (Int, Int)) => {
    true must_=== true
  }).asResult
}
