package org.fpscala.chap03

object Exer39 {
  def length[A](as: List[A]): Int =
    as.foldRight(0)((_, i) => i + 1)
}

object Exer310 {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }
}

object Exer311 {
  import Exer310._

  // def sum[A](as: List[A]): A = foldLeft(as, 0)((x: A, y: A) => x + y)

  // def product[A](as: List[A]): A = foldLeft(as, 1)((x: A, y: A) => x * y)

  def length[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)
}

object Exer312 {
  def reverse[A](as: List[A]): List[A] = {
    def revAppend(l1: List[A], l2: List[A]): List[A] = {
      l1 match {
        case Nil => l2
        case x::xs => revAppend(xs, x::l2)
      }
    }
    revAppend(as, List())
  }

  def reverse1[A](as: List[A]): List[A] = {
    import Exer310._

    foldLeft(as, List[A]())((ret, x) => x::ret)
  }
}

object Exer314 {
  def append[A](l1: List[A], l2: List[A]): List[A] =
    l1.foldRight(l2)((x, xs) => x :: xs)
}

object Exer315 {
  import Exer314._

  def concat[A](l: List[List[A]]): List[A] = {
    l.foldRight(List[A]())((xs1, xs2) => append(xs1, xs2))
  }
}

object Exer318 {
  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case x::xs => f(x)::map(xs)(f)
    }
}

object Exer319 {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case x::xs => if(f(x)) x::filter(xs)(f) else filter(xs)(f)
    }
  }
}

object Exer320 {
  import Exer315._
  import Exer318._

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))
}

object Exer321 {
  import Exer320.flatMap

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) List(x) else Nil)
}

object Exer323 {
  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = {
    (xs, ys) match {
      case (Nil, _) | (_, Nil) => Nil
      case (x::xs, y::ys) => f(x, y) :: zipWith(xs, ys)(f)
    }
  }
}

object Exer324 {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasSub(xs: List[A], ys: List[A], continuous: Boolean): Boolean = {
      (xs, ys) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (x::xs1, y::ys1) =>
          if (x == y)
            hasSub(xs1, ys1, continuous=true)
          else if(continuous)
            false
          else
            hasSub(xs1, ys, continuous=false)
      }
    }
    hasSub(sup, sub, continuous=false)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Exer325 {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }
}

object Exer326 {
  def maxium(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maxium(left) max maxium(right)
  }
}

object Exer327 {
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }
}

object Exer328 {
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}

object Exer329 {
  def foldRight[A, B](tree: Tree[A], init: B)(f: (A, B) => B): B = {
    tree match {
      case Leaf(v) => f(v, init)
      case Branch(left, right) =>
        foldRight(left, foldRight(right, init)(f))(f)
    }
  }

  def foldLeft[A, B](init: B,tree: Tree[A])(f: (B, A) => B): B = {
    tree match {
      case Leaf(v) => f(init, v)
      case Branch(left, right) =>
        foldLeft(foldLeft(init, left)(f), right)(f)
    }
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(left, right) =>
      g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maxium(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((x, y) => (x max y) + 1)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
