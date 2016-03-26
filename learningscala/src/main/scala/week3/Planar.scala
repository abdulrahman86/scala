package week3

/**
  * Created by asattar on 2016-03-26.
  */
trait Planar {

  def height: Int
  def width: Int
  def surface = height * width
}
