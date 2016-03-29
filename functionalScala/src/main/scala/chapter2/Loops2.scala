package chapter2

import scala.annotation.tailrec

/**
  * Created by asattar on 2016-03-28.
  */
object Loops2 {

  def fib(n: Int) : Int = {
    n match {
        case 0 => 0
        case 1 => 1
        case _ => fib(n-1) + fib(n-2)
    }
  }

  def fibTailRecur(n: Int) : Int = {
    @tailrec
    def fibHelper (curIndex: Int, n: Int, acc: Int, l1: Int, l2: Int) : Int = {

      if (curIndex <= n) {
        val x = curIndex match {
          case 0 => 0
          case 1 => 1
          case _ => l1 + l2
        }

        fibHelper(curIndex + 1, n, acc + x, l2, x)

      }

      else {
        acc
      }
    }

    fibHelper(0, n, 0, 0, 0)
  }

  def findFirst2[T](x: Array[T], p : T => Boolean) : T = {
    @tailrec
    def loop (i : Int) : T = {
      if (i < x.length) {
        p(x(i)) match {
          case true => x(i)
          case false => loop (i+1)
        }
      }
      else {
        throw new Error("error not found")
      }
    }
    loop(0)
  }

  def isSorted[T](x: Array[T], p: (T, T) => Boolean) : Boolean = {
    @tailrec
    def loop (i : Int) : Boolean = {
      if (i < x.length) {
        p(x(i-1), x(i)) match {
          case true => loop(i+1)
          case false => false
        }
      }
      else {
        true
      }
    }
    if (x.length <= 1) {
      true
    }else {
      loop(1)
    }
  }
}
