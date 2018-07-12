
object HelloWorld  {

  def main(args: Array[String]) {
    println( "Triggered main");
    exampleFunc( "Mityo Krika" )
  }

  def exampleFunc(name: String): Unit = {
    println("Hello World, " + name + "!")
  }

}

import java.util.concurrent._
import language.implicitConversions

object c7 {

  object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
      a(s)
    }

    def unit[A](a: A): Par[A] = {
      // `unit` is represented as a function that returns a `UnitFuture`,
      // which is a simple implementation of `Future` that just wraps a constant value.
      // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
      // Its `get` method simply returns the value that we gave it.
      (es: ExecutorService) => UnitFuture(a)
    }

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      // `map2` doesn't evaluate the call to `f` in a separate logical thread,
      // in accord with our design choice of having `fork` be the sole function in the API
      // for controlling parallelism.
      //
      // We can always do `fork(map2(a,b)(f))`
      // if we want the evaluation of `f` to occur in a separate thread.
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
        // This implementation of `map2` does _not_ respect timeouts,
        // and eagerly waits for the returned futures.
        // This means that even if you have passed in "forked" arguments,
        // using this map2 on them will make them wait.
        // It simply passes the `ExecutorService` on to both `Par` values,
        // waits for the results of the Futures `af` and `bf`,
        // applies `f` to them, and wraps them in a `UnitFuture`.
        // In order to respect timeouts, we'd need a new `Future` implementation
        // that records the amount of time spent evaluating `af`,
        // then subtracts that time from the available time allocated for evaluating `bf`.
      }
    }

    def fork[A](a: => Par[A]): Par[A] = {
      // This is the simplest and most natural implementation of `fork`,
      // but there are some problems with it--for one,
      // the outer `Callable` will block waiting for the "inner" task to complete.
      // Since this blocking occupies a thread in our thread pool,
      // or whatever resource backs the `ExecutorService`,
      // this implies that we're losing out on some potential parallelism.
      // Essentially, we're using two threads when one should suffice.
      // This is a symptom of a more serious problem with the implementation,
      // and we will discuss this later in the chapter.
      es =>
        es.submit(new Callable[A] {
          def call = a(es).get
        })
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a, _) => f(a))
    }

    def sortPar(parList: Par[List[Int]]) = {
      map(parList)(_.sorted)
    }

    def sortPar2(parList: Par[List[Int]]): Par[List[Int]] =
      map2(parList, unit(()))((a, _) => a.sorted)

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = {
      p(e).get == p2(e).get
    }

    def delay[A](fa: => Par[A]): Par[A] = {
      es => fa(es)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      es =>
        if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
        else f(es)
    }

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {


    }

  }

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = { // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }
  }

  //ex7_1 map2 Syntax - done
  // def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {

  //ex7_2
  // Idk, you are supposed to teach me that wtf? Before continuing please do our job :D

  //ex7_3
  // Done, by the initial code.

  //ex7_4
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => lazyUnit(f(a))
  }

  //ex7_5
  // Not working
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    def loop[A](l: List[Par[A]]): Par[List[A]] = l match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(loop(t)))((a: A, b: List[A]) => a :: b)
    }

    loop(ps)
  }

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)((a: A, b: List[A]) => a :: b))

  def parMap[A,B]( ps: List[A] )( f: A => B ) : Par[ List[B] ] = fork {
    val fbs: List[Par[B]] = ps.map( asyncF(f) ) //create Par object of each A and push it towards the sequence

    sequence_simple(fbs)
  }

  //ex7_6
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: tail => x + sum(tail)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    def loop(as: List[A]): List[A] = as match {
      case Nil => Nil
      case h :: t => if (f(h)) h :: loop(t) else t
    }

    unit(loop(as))
  }

  //7.7 - What do they want from me ?
  //7.8 - blahblahblah
  //7.9 - blahblahblah
  //7.10 - blahblahblah

  //7.11

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)
  }

  def choiceN[A]( n: Par[Int] )( choices: List[ Par[A] ] ): Par[A] = {
    es => {
      //you pass executorservice to the run. Then you pass the item. What will happen is that
      //the executorservice will be applied to the item and then you can call .get on it cause
      //we will be working with our Future object at that point

      val ind = run(es)(n).get // get the "index" that is called "N"
      run(es)(choices(ind)) //use that index to get an item from the choices list

      //my wrong idea:
//      def loop( cc: List[Par[A] ] ): Par[A] = cc match {
//        case Nil => Nil
//        case x => if (run(es)(cond).get)t
//      }
//
//      loop( choices )
    }
  }

  def choiceInTermsOfChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
//    es => {
      val n = map( cond )( (x: Boolean) => if (x) 0 else 1)
//      val n = unit( if( run(es)(cond).get ) 0 else 1 )
      choiceN( n )( List(t, f) )
//    }
  }

  //7_12
  def choiceMap[K,V](key: Par[K])( choices: Map[K,Par[V]] ): Par[V] = {
//    val n = map( key )( (x: K) => x)
//    choices( n )

    es => {
      val n = run(es)(key).get
      run(es)(choices(n))
    }
  }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val n = run(es)(pa).get
      run(es)(choices(n))
    }
  }

  //7_14
  def join[A]( a: Par[ Par[A] ] ): Par[A] = {
    es => {
      val k = run(es)(a).get
      k(es)
    }
  }

  def flatMap[A,B]( a: Par[A] )( f: A => Par[B] ): Par[B] = {
    es => {
      val k = run(es)(a).get
      f(k)(es)
    }
  }



  val es: ExecutorService = Executors.newFixedThreadPool(2)
  val list = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil
  val sumExample = sum( IndexedSeq(1,2,3) )
  val ex7_4 = asyncF( (x: Int) => x+1 )(5)( es ).get
  val ex7_4_2 = asyncF( (x: Int) => x+2 )(6)( es ).get
  val parList1 = List( unit( 1 ), unit( 2 ) )
  val parList2 = List( unit( 3 ), unit( "Bob" ) )

//  val ex7_4_1 = fork( asyncF( (x: Int) => x+1 )(5) )( es ).get
  var ex7_5 = sequence_simple( parList1 )( es ).get
  var ex7_6 = parFilter( list )( (x: Int) => x > 3 )
  var ex7_6_1 = parFilter( list )( (x: Int) => x > 3 )( es ).get

  import java.util.concurrent.Executors
  val p = parMap(List.range(1, 100000))(math.sqrt(_))
  val x = run(Executors.newFixedThreadPool(2))(p)

  val ex_11_t_1 = choice( unit(true) )( unit(1), unit(2) )( es ).get
  val ex_11_t_2 = choice( unit(false) )( unit(1), unit(2) )( es ).get

  var ex_11_1 = choiceN( unit(4) )( List( unit(1),unit(2),unit(3),unit(4),unit(5555) ) )( es ).get

  val ex_11_2_1 = choiceInTermsOfChoiceN( unit(true) )( unit(1), unit(2) )( es ).get
  val ex_11_2_2 = choiceInTermsOfChoiceN( unit(false) )( unit(1), unit(2) )( es ).get

  val ex_12_1 = choiceMap( unit(2) )( Map( 0 -> unit(5), 1 -> unit(34), 2 -> unit(111), 3 -> unit(934) ) )( es ).get
  val ex_12_2 = chooser( unit(1) )( Map( 0 -> unit(5), 1 -> unit(34), 2 -> unit(111), 3 -> unit(934) ) )( es ).get

  //ex7_13
  //def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
  val ex_13_choice_1 = chooser( unit(1) )( (x: Int) => if(x>1) unit(5) else unit(3) )( es ).get
  val ex_13_choice_2 = chooser( unit(4) )( (x: Int) => if(x>1) unit(5) else unit(3) )( es ).get
  //def choiceN[A]( n: Par[Int] )( choices: List[ Par[A] ] ): Par[A] = {
  val ex_13_choiceN_1 = chooser( unit(1) )( (x: Int) => {
    val l = List( unit(1),unit(2),unit(3),unit(4),unit(5555) )
    l(x)
  } )( es ).get

  //7_14
  val ex_14_1 = join( unit( unit(5) ) )( es ).get
  val ex_14_2 = flatMap( unit(5) )( (x: Int) => unit(x+5) )( es ).get

  //idk...
  val ex_14_1_flatMap = join( unit( unit(5) ) )( es ).get + join( unit( unit(5) ) )( es ).get

  //done ^^
}

