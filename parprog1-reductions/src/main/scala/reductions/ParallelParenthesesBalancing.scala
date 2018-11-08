package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    //println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    
    def getParenNum(curNum:Int, curChars:Array[Char]): Boolean = {
      
      if (curNum < 0) false 
        
      else {
        curChars match {
          case Array() => curNum == 0
          case Array('(',_) => getParenNum( curNum + 1, curChars.tail)
          case Array(')',_) => getParenNum( curNum - 1, curChars.tail)
        }
      }
      
    }
    
    getParenNum(0, chars)
    
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx == until - 1) {
        chars(idx) match {
          case '(' =>(arg1 + 1, arg2)
          case ')' =>(arg1, arg2 + 1)
          case _ => (arg1, arg2)
        }
      } else {
        chars(idx) match {
          case '(' =>traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' =>traverse(idx + 1, until, arg1, arg2 + 1)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      }
    }

    def reduce(from: Int, until: Int) : Int = {
      
      def r1 ( currDiff:Int, from1: Int, until1: Int):Int = {
        if (currDiff < 0) -1
        else {
          if (until1 - from1 <= threshold)  {
            val (l, r) = traverse(from1, until1, 0, 0)
            currDiff + l -r
          } else {
            val (l, r) = traverse(from1, from1 + threshold,0 , 0)
            r1( l - r, from1 + threshold, until1)
          }
        }
        
      }
      
      r1 (0, from , until)
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
