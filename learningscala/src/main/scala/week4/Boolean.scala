package week4

/**
  * Created by asattar on 2016-03-26.
  */
trait Bool {

  def && (other : Bool) : Bool
  def || (other : Bool) : Bool
  def unary_~ : Bool
}

object True extends  Bool {
  def && (other : Bool) = other
  def || (other : Bool) = this
  def unary_~ = False
  override  def toString =
    "true"

}

object False extends  Bool {
  def && (other : Bool) = this
  def || (other : Bool) = other
  def unary_~ = True
  override  def toString =
    "false"
}
