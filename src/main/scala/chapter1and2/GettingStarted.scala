//package chapter1and2
import scala.annotation.tailrec

//Отиваш директно в папката chapter1and2 и бичиш scalac GettingStarted.scala за да билднеш файла
//и после отваряш скала терминала със scala и директно викаш функциите от MyModule, то си знае къде си от pwd, I guess.

object HelloWorld  {

  def main(args: Array[String]) {
    println( "Triggered main");
    exampleFunc( "Mityo Krika" )
  }

  def exampleFunc(name: String): Unit = {
      println("Hello World, " + name + "!")
  }
}

//class Cafe {
//
//  def buyCoffee( cc: CreditCard): Coffee = {
//    val cup = new Coffee()
//
//    cc.charge( cup.price )
//
//    cup
//  }
//}

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def kek(n: Int, q: Int): Int =
    (n+q)*2

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }
  //MyModule.partial1(5, (x: Int, y: Int) => x+y )
  //res1(4)
  //results in 9
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  //MyModule.curry( (x: Int, y: Int) => x+y )
  //res1(1)
  //res1(2)
  //results in 3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  //MyModule.uncurry( (x: Int) => ( (y: Int) => x+y ) )
  //res3(2,3)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //MyModule.compose( (x: Int) => x+1, (y: Int) => y+1 )
  //res(5)
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f( g(a) )
  }

  //MyModule.isSorted( Array(1,2), (x: Int, y: Int) => x<y )
  //MyModule.isSorted( Array(3,2), (x: Int, y: Int) => x<y )
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop( index: Int ): Boolean = {
      if ( index == 0 ) {
        true
      } else {
        if ( ordered( as(index-1), as(index) ) ) {
          loop( index-1 )
        } else {
          false
        }
      }
    }

    loop( as.length-1 )
  }

  //MyModule.fib(1)
  def fib(n: Int): Int = {

      @annotation.tailrec
      def loop( left: Int, preLast: Int, last: Int ): Int = {
        if( left == 0 ) {
          last
        } else {
          if( left == 1 && preLast == 1 ) {
            last
          } else {
            loop( left-1, last, preLast+last )
          }
        }
      }

      loop(n, 1, 1)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
    println( formatResult( "factorial", 9, factorial ) )
    println( findFirst( Array("Blaga","Pesho","Gosho"), (x: String) => x == "Pesho2" ) )
}



