/**
 * University of Skövde
 * Course: Advanced Programming
 */

package linalg

import scala.::


/**
 * This is our own implementation of List.
 * More details are given in lectures 6 and 7 (check the slides).
 *
 * Some of the methods/functions are already implemented.
 * Others you should implement. These should not be too different from those
 * you've exercised with in the labs.
 *
 * Note: we're using advanced typing here (e.g., [+A]). Don't worry, I'm
 * giving you the signatures of the functions so that you only have to care to
 * about their content (types should not get in your way).
 */

/**
 * Lista is an algebraic data type, this means that it is a type composed of two
 * or more types. In this case, Lista is composed of Emp (an empty list) and Cons
 * (a none empty list). These two are farther down.
 *
 * YOUR WORK IS IN case class Cons. There you'll have to implement some of the methods.
 *
 * The trait below and object Emp can be left the way the are (unless you want to
 * add extra things which you thing could be helpful).
 */
sealed trait Lista[+A] {
  /**
   * The first element of the list.
   */
  def head: A

  /**
   * The rest of the list.
   */
  def tail: Lista[A]

  /**
   * Number of elements in the list.
   */
  def size: Int

  /**
   * Returns the ith element in the list.
   */
  def apply(i: Int): A

  /**
   * See exercise 2.6.
   */
  def zip[B](ns: Lista[B]): Lista[(A, B)]

  /**
   * See exercise 3.2.
   */
  def map[B](f: A => B): Lista[B]

  /**
   * Does only the merge part of exercise 3.8.
   */
  def merge[B >: A](ns: Lista[B], f: (B, B) => B): Lista[B]

  /**
   * Checks whether a condition holds for all elements.
   */
  def forall(f: A => Boolean): Boolean

  /**
   * See exercise 3.6.
   */
  def reduce[B >: A](f: (B, B) => B): B

  /**
   * Returns a new list without the first 'n' elements.
   */
  def drop(n: Int): Lista[A]

  /**
   * Returns a new list with the first 'n' elements.
   */
  def take(n: Int): Lista[A]

  /**
   * Returns a the list reversed.
   */
  def reverse: Lista[A]
  /**
   * Prettyfies the output of a lista.
   */
  override def toString: String = {
    val str = map( _.toString ).reduce( (a, b) => {
      s"$a, $b"
    } )
    s"[$str]"
  }
}

/**
 * An empty lista.
 */
case object Emp extends Lista[Nothing] {
  def head: Nothing = throw new Exception( "Head of an empty list" )

  def tail: Lista[Nothing] = throw new Exception( "Tail of an empty list" )

  def apply(i: Int): Nothing = throw new Exception( "Index on empty list" )

  def size: Int = 0

  def map[B](f: Nothing => B): Lista[B] = Emp

  def merge[B >: Nothing](ns: Lista[B], f: (B, B) => B): Lista[B] = Emp

  def zip[B](ns: Lista[B]): Lista[(Nothing, B)] = Emp

  def drop(n: Int): Lista[Nothing] = Emp

  def take(n: Int): Lista[Nothing] = Emp

  def forall(f: Nothing => Boolean): Boolean = true

  def reduce[B >: Nothing](f: (B, B) => B): B = throw new Exception( "Reduce on empty list" )

  def reverse: Lista[Nothing] =  throw new Exception( "Reverse on empty list" )
}


/**
 * A none empty list. Some of these methods you'll need to implement.
 */
case class Cons[A](val head: A, val tail: Lista[A]) extends Lista[A] {
  lazy val size: Int = 1 + tail.size

  def apply(i: Int): A = {
    if (i == 0) head
    else tail( i - 1 )
  }

  def map[B](f: A => B): Lista[B] = {
    Cons( f( head ), tail.map( f ) )
  }

  def reduce[B >: A](f: (B, B) => B): B = {
    def loop(ns: Lista[A], acc: B): B = {
      if (ns.tail == Emp) f( acc, ns.head )
      else loop( ns.tail, f( acc, ns.head ) )
    }

    if (tail == Emp) head
    else loop( tail, head )
  }

  def zip[B](ns: Lista[B]): Lista[(A, B)] = ns match {
    case Emp => Emp
    case Cons( h, t ) => Cons( (head, h), tail.zip( t ) )
  }


  // =================================================================
  // ===================== Your working area =========================
  /**
   * These for methods are the only ones you'll need to implement in this class.
   */

  /**
   * Checks whether a condition holds for all elements.
   */
  def forall(f: A => Boolean): Boolean = f( head ) && tail.forall( f )

  /**
   * Does only the merge part of exercise 3.8.
   */
  def merge[B >: A](ns: Lista[B], f: (B, B) => B): Lista[B] = ns match {
    case Emp => Emp
    case Cons( h, t ) => Cons( f( head, h ), tail.merge( t, f ) )
  }

  /**
   * Returns a new list without the first 'n' elements.
   */
  def drop(n: Int): Lista[A] = {
    def drop[A](l: Lista[A], n: Int): Lista[A] = l match {
      case Emp => Emp
      case _ if n == 0 => l
      case Cons( _, tail ) => drop( tail, n - 1 )
    }

    drop( Cons( head, tail ), n )
  }

  /**
   * Returns a new list with the first 'n' elements.
   */
  def take(n: Int): Lista[A] = {
    def take[A](l: Lista[A], n: Int): Lista[A] = l match {
      case Emp => Emp
      case _ if n == 0 => Cons(l.head,Emp)
      case Cons(_,tail) if n >=1 => Cons(l.head,take(tail, n - 1 ))
    }
    take( Cons( head, tail ), n )
  }
  def reverse: Lista[A] = {
    def reverse(oldList:Lista[A], newList:Lista[A]): Lista[A] = oldList match {
      case Emp => Emp
      case Cons(head, Emp) => Cons(head,newList)
      case Cons(head, tail) => reverse(tail, Cons(head, newList))
    }
    reverse(Cons(head, tail), Lista())
  }
}

/**
 * Companion object of Lista.
 * Defines comodity methods for instantiating a list.
 */
object Lista {
  def apply[A](ns: A*): Lista[A] = {
    if (ns.isEmpty) Emp
    else Cons( ns.head, apply( ns.tail: _* ) )
  }
}
