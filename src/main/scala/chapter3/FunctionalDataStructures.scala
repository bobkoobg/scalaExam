
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

//sealed abstract class List[+A] <- almost the same
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  //  def apply[A](as: A*): List[A] =
  //    if (as.isEmpty) Nil
  //    else Cons(as.head, apply(as.tail: _*))
  //

  //
  //  def append[A](a1: List[A], a2: List[A]): List[A] =
  //    a1 match {
  //      case Nil => a2
  //      case Cons(h,t) => Cons(h, append(t, a2))
  //    }
  //
  //  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
  //    as match {
  //      case Cons(h,t) if f(h) => dropWhile(t)(f)
  //      case _ => as
  //    }
  //
  //  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  //    as match {
  //      case Nil => z
  //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //    }
  //
  //  def sum2(ns: List[Int]) =
  //    foldRight(ns, 0)((x,y) => x + y)
  //
  //  def product2(ns: List[Double]) =
  //    foldRight(ns, 1.0)(_ * _)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  //ex3_2
  def tail[A]( ds: List[A] ): List[A] = ds match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  //ex3_3
  def setHead[A]( ds: List[A], newHead: A ): List[A] = ds match {
    case Nil => Nil
    case Cons(h, t) => Cons(newHead, t)
  }

  //ex3_4
  def drop2[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      n match {
        case 0 => l
        case _ => drop2(t, n-1)
      }
    }
  }

  //ex3_5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if (f(h)) dropWhile(t,f) else l
  }

  //ex3_6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h,t) => Cons(h, init(t) )
  }

  //ex3_7 & ex 3_8
  def foldRight[A,B](as: List[A], z: B)(f: (A, B ) => B): B = as match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t,z)(f) )
  }

  def product2(ds: List[Double]): Double = {
    foldRight(ds, 1.0)( _*_ )
  }

  def sum2(ds: List[Int]): Int = {
    foldRight(ds, 0)( _+_ )
  }

  //ex3_9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)( (a: A, count: Int) =>  count+1 )
  }

  //ex3_10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft( t, f( z,h ) )(f)
  }

  //ex3_11
  def product3(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)( _*_ )
  }

  def sum3(ds: List[Int]): Int = {
    foldLeft(ds, 0)( _+_ )
  }

  def length3[A](as: List[A]): Int = {
    foldLeft(as, 0)( (count: Int, a: A) => count+1 )
  }

  //ex3_12
  def reverse3[A](as: List[A]): List[A] = {
    // wrong :
//    foldLeft(as, List() )( (a: A, zeList: Int) => Cons(zeList, Cons(a, Nil) ) )
//    foldRight(as, Nil )( (a: A, zeList: List[Int] ) => Cons(a, zeList) )
    @annotation.tailrec
    def loop( l: List[A], newList: List[A] ) : List[A] = l match {
      case Nil => newList
      case Cons(h,t) => {
        newList match {
          case Nil => loop( t, Cons(h, Nil) )
          case _ => loop( t, Cons(h, newList) )
        }
      }
    }

    loop(as, Nil)
  }

  //ex3_13 - Fuck you.

  //ex4_14
  def append2[A]( first: List[A], second: List[A] ): List[A] = {
    //solution
    //foldRight(first, second)( Cons(_,_) )
    foldRight(first, second)( (firstListElem: A, secondList: List[A]) => Cons(firstListElem, secondList) )
  }

  def append3[A]( first: List[A], second: List[A] ): List[A] = {
    foldLeft(first, second)( ( secondList: List[A], firstListElem: A) => Cons( firstListElem, secondList ) )
  }

  //ex3_15 - Fuck you.

  //ex3_16
  def addOne(as: List[Int]): List[Int] = {
    foldRight(as, Nil:List[Int] )( (a: Int, theOtherPartOfTheList: List[Int] ) => Cons(a+1, theOtherPartOfTheList) )
  }

  //ex3_17
  def doubleToString(as: List[Double]): List[String] = {
    foldRight(as, Nil:List[String])( (h: Double, t: List[String] ) => Cons(h.toString() + "Boombaye!", t) )
  }

  //ex3_18
  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h,t) => Cons( f(h), map(t)(f) )
  }

  //ex3_19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h,t) => if (f(h)) Cons(h, filter(t)(f) ) else filter(t)(f)
  }

  //ex3_20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h,t) => append2( f(h), flatMap(t)(f) )
  }

  //ex3_21
  def flatMapFilter[A,B]( as: List[A], f: A => List[B] ): List[B] = as match {
    case Nil => Nil
    case Cons(h,t) => if( f(h) == Nil ) flatMapFilter(t, f) else append2( f(h), flatMapFilter(t,f) )
  }

  //ex3_22
  def concat[A,B,C]( left: List[A], right: List[B], f: (A,B) => C ): List[C] = left match {
    case Nil => Nil
    case Cons(lh, lt) => {
      right match {
        case Nil => Nil
        case Cons(rh, rt) => Cons( f(lh, rh), concat(lt, rt, f) )
      }
    }
  }

  //ex3.23
  def zipWith[A,B,C]( left: List[A], right: List[B], f: (A,B) => C ): List[C] = concat(left, right, f)

  //ex3_24 - Fuck you.
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons( lh, Nil ) => false
    case Cons( lh, Cons(lh2,lt ) ) => sub match {
      case Nil => false
      case Cons( rh, Nil ) => false
      case Cons( rh, Cons(rh2,rt ) ) => if( lh == rh && lh2 == rh2 ) true else hasSubsequence( Cons(lh2,lt ), sub )
    }
  }

  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))
  val ex4: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  val ex5: List[Double] = Cons(1.0, Cons(2.0, Cons(3.0, Cons(4.0, Cons(5.0, Nil)))))
  val ex6: List[Int] = Cons(6, Cons(5, Cons(4, Cons(3, Cons(999, Nil)))))

  val ex3_2 = tail(ex4);
  val ex3_3 = setHead(ex4, "Pesho");
  val ex3_4 = drop2(ex4, 2);
  val ex3_5_1 = dropWhile(ex4, (x: Int) => x < -100)
  val ex3_5_2 = dropWhile(ex4, (x: Int) => x < 100);
  val ex3_5_3 = dropWhile(ex4, (x: Int) => x < 3);
  val ex3_6 = init(ex4);
  val ex3_7_pre_1 = foldRight(ex4,0)( _ + _ );
  val ex3_7_pre_2 = foldRight(ex4,1)( _ + _ );
  val ex3_7_1 = product2(ex5);
  val ex3_7_2 = product(ex5);
  val ex3_7_pre_5 = sum2(ex4);
  val ex3_7_pre_6 = sum(ex4);
  val ex3_8_1 = product2(Nil);
  val ex3_8_2 = foldRight( Cons(1, Cons(2, Cons(3, Nil ) ) ), Nil:List[Int] ) ( Cons(_,_) )
  val ex3_9 = length(ex4)
  val ex3_10_1 = foldLeft( ex4, 0)( (a: Int, b: Int) => a+b )
  val ex3_10_2 = foldLeft( ex4, 1)( (a: Int, b: Int) => a+b )
  val ex3_11_1 = product3( ex5 )
  val ex3_11_2 = sum3( ex4 )
  val ex3_11_3 = length3( ex3 )
  val ex3_12 = reverse3(ex4)
  val ex3_14_1 = append2(ex4,ex3)
  val ex3_14_2 = append3(ex4,ex3)
  val ex3_14_3 = reverse3(append3(ex4,ex3))
  val ex3_16 = addOne(ex4)
  val ex3_17 = doubleToString(ex5)
  val ex3_18 = map(ex4)( (x:Int)=>x*2 )
  val ex3_19 = filter( ex4 )( (x: Int) => x%2==0 )
  val ex3_20 = flatMap( ex4 )( i => Cons(i, Cons(i, Nil) ) )
  val ex3_21_1 = flatMapFilter( ex4, (x: Int) => if(x%2 == 1) Cons(x, Nil) else Nil )
  val ex3_21_2 = flatMapFilter( ex4, (x: Int) => if(x%2 == 0) Cons(x, Nil) else Nil )
  val ex3_22_1 = concat( ex4, ex6, (x: Int, y: Int) => x+y )
  val ex3_22_2 = concat( ex4, ex4, (x: Int, y: Int) => x+y )
  val ex3_23 = zipWith( ex5, List.ex6, (x: Double, y: Int) => x*y )
  //blow me ^^
  val ex3_24_1 = hasSubsequence( ex4, Cons(2, Cons(3, Nil)) )
  val ex3_24_2 = hasSubsequence( ex4, Cons(1, Cons(3, Nil)) )

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  val ex7: Tree[Int] = Branch( Branch( Leaf(2), Leaf(2) ), Branch( Leaf(3), Branch( Leaf(1), Leaf(4) ) ) )
  val ex8: Tree[Int] = Branch( Branch( Leaf(2), Leaf(2) ), Branch( Leaf(3), Branch( Leaf(1), Leaf(43) ) ) )
  val ex9: Tree[Int] =
    Branch(
      Branch(
        Branch(
          Leaf(4),
          Branch(
            Branch(
              Leaf(1),
              Leaf(2)
            ),
          Leaf(5) )
        ), Leaf(3)
      ), Leaf(2)
    )

  def size[A]( t: Tree[A] ): Int = t match {
    case Leaf(v) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum( t: Tree[Int] ): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l).max( maximum(r) )
  }

  def depth[A]( t: Tree[A] ): Int = t match {
    case Leaf(v) => 1
    case Branch(l,r) => ( 1 + depth(l) ).max( 1 + depth(r) )
  }

  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch( map(l)(f), map(r)(f) )
  }

  def fold[A,B](as: Tree[A])(f: A => B)( cmp: (B, B) => B ): B = as match {
    case Leaf(v) => f(v)
    case Branch(l,r) => cmp( fold(l)(f)(cmp), fold(r)(f)(cmp) )
  }

  val ex3_25_1 = size( ex7 )
  val ex3_25_2 = size( ex9 )
  val ex3_26_1 = maximum( ex7 )
  val ex3_26_2 = maximum( ex8 )
  val ex3_27_1 = depth( ex8 )
  val ex3_27_2 = depth( ex9 )
  val ex3_28_1 = map( ex9 )( (x: Int) => x*2 )
  val ex3_28_2 = map( ex8 )( (x: Int) => x*2 )
  var ex3_29_1 = fold( ex8 )( ( x: Int ) => x )( ( x: Int, y: Int ) => x.max(y) ) //maximum
  var ex3_29_2 = fold( ex9 )( ( x: Int ) => x )( ( x: Int, y: Int ) => x.max(y) ) //maximum
  var ex3_29_3 = fold( ex8 )( ( x: Int ) => 1 )( ( x: Int, y: Int ) => 1 + x + y ) //size
  var ex3_29_4 = fold( ex9 )( ( x: Int ) => 1 )( ( x: Int, y: Int ) => 1 + x + y ) //size
  var ex3_29_5 = fold( ex8 )( ( x: Int ) => 1 )( ( x: Int, y: Int ) => (1+x).max(1+y) ) //depth
  var ex3_29_6 = fold( ex9 )( ( x: Int ) => 1 )( ( x: Int, y: Int ) => (1+x).max(1+y) ) //depth
}