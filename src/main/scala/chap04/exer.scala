package org.fpscala.chap04

trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(_) => this
    }

  def flatMap1[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse1[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Exer42 {
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => Math.pow(x - m, 2))))
}

object Exer43 {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }

  def map2a[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b.map (bb => f(aa, bb)))
}

object Exer44 {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def seq[A](a: List[Option[A]], b: List[A]): List[A] =
      a match {
        case Nil => b
        case None :: _ => Nil
        case Some(x) :: xs => seq(xs, x :: b)
      }

    seq(a, Nil) match {
      case Nil => None
      case xs => Some(xs)
    }
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => x flatMap(xx => sequence1(xs) map (ys => xx :: ys))
    }
  }
}

object Exer45 {
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => f(x) flatMap(xx => traverse(xs)(f) map (ys => xx :: ys))
    }
  }

  import Exer43.map2

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs => map2(f(x), traverse(xs)(f))(_::_)
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}
