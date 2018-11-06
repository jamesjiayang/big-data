package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

/*
 * This is not part of course assignment. This is used for testing course 3 (Parallel programming) Week 2 materials
 */
@RunWith(classOf[JUnitRunner]) 
class ScanLeftTree extends FunSuite {
  import LineOfSight._
  test("Scan left function in parallel using Tree structure") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


}

