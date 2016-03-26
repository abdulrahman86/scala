package week3

/**
  * Created by asattar on 2016-03-26.
  */
abstract class IntSet {

  def contains(x: Int) : Boolean
  def incl(x: Int) : IntSet
  def union(x : IntSet) : IntSet
}

object Empty extends IntSet {

  def contains (x: Int) = false

  def incl(x: Int) =
    new NonEmpty(x, Empty, Empty)

  override def toString = "."

  override def union (x: IntSet)  =
    x


}

class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {

  def contains (x: Int) : Boolean =
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true

  def incl(x: Int)  =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

  override def union (x: IntSet) =
    left union right union x incl elem

}