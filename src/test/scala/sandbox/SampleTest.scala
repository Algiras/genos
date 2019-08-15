package sandbox

import org.specs2.Specification
import org.specs2.execute.Result
import sandbox.Arbitrary.ArbitrarySpec
import scala.util.Random

class SampleTest extends Specification with ArbitrarySpec[Result] { override def is =
  s2"""
     reversing a reversed list should end up with same list                       $reverseList
     shuffling and ordering the list should produce same result as just ordering  $shuffleOrder
     set addition is commutative                                                  $setPairTheSameSetAdded
    """

  def reverseList = prop((res: List[Int]) => res.reverse.reverse must_=== res).asResult
  def shuffleOrder = prop((res:List[Int]) => res.sorted must_=== Random.shuffle(res).sorted).asResult
  def setPairTheSameSetAdded = prop((res: (Set[Int], Set[Int])) => (res._1 ++ res._2) must_=== (res._2 ++ res._1)).asResult
}
