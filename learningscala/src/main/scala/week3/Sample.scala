package week3

/**
  * Created by asattar on 2016-04-05.
  */
trait A {

  def m1 : Int
  def m2 : Int
}


class A1(m1Param: => Int, val m2 : Int ) extends A {

  lazy val m1 = m1Param
}