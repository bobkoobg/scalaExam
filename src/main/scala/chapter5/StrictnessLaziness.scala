
import scala.annotation.tailrec

object HelloWorld  {

  def main(args: Array[String]) {
    println( "Triggered main");
    exampleFunc( "Mityo Krika" )
  }

  def exampleFunc(name: String): Unit = {
    println("Hello World, " + name + "!")
  }
}

import Stream._

object c5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists2(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    //ex5.1
    def toList: List[A] = this match {
      case Cons( h, t ) => h() :: t().toList
      case Empty => Nil
    }

    //ex5.2
    def take(n: Int): Stream[A] = this match {
      case Cons( h, t ) if (n > 0) =>  Cons( h, () => t().take(n-1) )
      case Cons( h, t ) if (n <= 0) => Empty
      case Empty => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons( h, t ) if (n > 0) =>  t().drop(n-1)
      case Cons( h, t ) if (n <= 0) => Cons( h, () => t().drop(0) )
      case Empty => Empty
    }

    //ex5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons( h, t ) => if (p(h())) Cons(h, () => t().takeWhile(p) ) else Empty
      case Empty => Empty
    }

    //ex5.4
    def forAll(p: A => Boolean): Boolean = this match {
      case Cons( h, t ) => if( p(h() ) ) t().forAll(p) else false
      case Empty => true
    }

    //ex5.5
    def takeWhileFoldRight( p: A => Boolean ): Stream[A] = {
      this.foldRight( Empty: Stream[A] )( ( a, b) => if( p( a )) Cons( () => a, () => b) else Empty )
    }

    //ex5.6
    def headOptionFoldRight(): Option[A] = {
      this.foldRight( None: Option[A] )( ( a, b) =>  Some(a) )
    }

    //ex5.7 - Solutions not working, too hard, skip.
//    def map[B](f: A => B) : Stream[B] = {
//      this.foldRight( Empty: Stream[B] )( (a, b) => cons( f(a), () => b ) )
//    }

    //ex5.14
//    def startsWith[A](s: Stream[A]): Boolean =  this match {
//      case Empty => false
//      case Cons( thisH, thisT ) => s match {
//        case Empty => true
//        case Cons( thatH, thatT ) => if( thisH == thatH ) thisT().startsWith( thatT )
//    }


  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    //ex5.8
    def constant[A](a: A): Stream[A] = {
      lazy val rEEE: Stream[A] = Cons(() => a, () => constant(a) )
      rEEE
    }

    //ex5.9
    def from(n: Int): Stream[Int] = {
      Cons( () => n, () => from(n+1) )
    }

    //ex5.10
    def fibs(): Stream[Int] = {

      def go( a: Int, b: Int ): Stream[Int] = {
        cons( b, go(b, a+b ) )
      }

      go(0, 1)
    }

    //ex5.11
//    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = z match {
//      case Cons(h, t) => cons( f(z), unfold( t )( f ) )
//      case Empty => Empty
//    }

    def unfold[A, S] (z:S) (f: S => Option[(A, S)]): Stream[A] = f(z).map {
      case (h, t) => cons(h, unfold (t) (f))
    }.getOrElse (Empty)

    //ex5.13 - Fuck this bullshit.
//    def zipAll[A, B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
//    }

    val ones: Stream[Int] = cons(1, ones)
    val soKurwa: Stream[Int] = cons(1, cons(2, cons(3, soKurwa)))
    val test = 12 :: 14 :: List()

    val ex5_1 = Stream(1,2,3).toList
    val ex5_2_1 = ones.take(5).toList
    val ex5_2_2 = ex5_2_1.drop(2).toList
    val ex5_3 = Stream(1,2,3,4,5).takeWhile( (x: Int) => x <= 4 ).toList
    val ex5_4 = Stream(1,2,3,4,5).forAll( (x: Int) => x <= 4 )
    val ex5_5 = Stream(1,2,3,4,5).takeWhileFoldRight( (x: Int) => x <= 2 ).toList
    val ex5_6 = Stream(1,2,3,4,5).headOptionFoldRight
    val ex5_8 = Stream.constant(1)
    val ex5_9 = Stream.from(1).take(15).toList
    val ex5_10 = Stream.fibs().take(15).toList

    var ex5_12_1 = unfold( (0,1) ) {
      case (a,b) => Some( a, (b, a+b) )
    }.take(3).toList //fibonacci

    var ex5_12_2 = unfold( (0) ) {
      case (a) => Some( a, (a+1) )
    }.take(3).toList //from

    var ex5_12_3 = unfold( (0) ) {
      case (a) => Some( a, (a) )
    }.take(3).toList //constant

    var ex5_12_4 = unfold( (0) ) {
      case (a) => Some( 1, 1 )
    }.take(3).toList //ones

  }


}

