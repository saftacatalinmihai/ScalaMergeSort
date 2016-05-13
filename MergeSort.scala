/**
  * Created by casafta on 13/5/2016.
  */

object MergeSort {

  def mergeSort(xs: List[Int]): List[Int] = xs match {
    case List() => xs
    case y :: ys =>
      val mid = xs.length / 2
      if ( mid == 0 ) xs
      else
        merge(mergeSort(xs.drop(mid)), mergeSort(xs.take(mid)))
  }
  def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case List() => ys
    case _x :: _xs => ys match {
      case List() => xs
      case _y :: _ys =>
        if (_x < _y) _x :: merge(_xs, ys) else _y :: merge(xs, _ys)
    }
  }

  def main(args: Array[String]): Unit ={
    val xs = List(5,1,3,82,1)
    val sorted = mergeSort(xs)
    println(sorted)
  }
}
