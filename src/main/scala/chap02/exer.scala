package org.fpscala.chap02

/**
  * Created by zw on 2017/5/21.
  */
object Exer22 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A], i: Int): Boolean = {
      if (as.length < i + 2)
        true
      else
        ordered(as(i), as(i+1)) && loop(as, i + 1)
    }
    loop(as, 0)
  }
}

object Exer23 {
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)
}

object Exer24 {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}
