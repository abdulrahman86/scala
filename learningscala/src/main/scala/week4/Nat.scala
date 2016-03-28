package week4

/**
  * Created by asattar on 2016-03-26.
  */
trait Nat {

  def + (other: Nat) : Nat
  def value : Int
  override  def toString = value.toString
}

object Zero extends Nat {

  def + (other : Nat) = other

  def value = 0
}

class Succ(n: Nat) extends  Nat {

  def + (other: Nat) = new Succ(n + other)

  def value = 1 + n.value
}