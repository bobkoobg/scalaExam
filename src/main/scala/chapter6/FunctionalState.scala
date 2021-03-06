
object HelloWorld  {

  def main(args: Array[String]) {
    println( "Triggered main");
    exampleFunc( "Mityo Krika" )
  }

  def exampleFunc(name: String): Unit = {
    println("Hello World, " + name + "!")
  }

}

object c6 {
  val rng = new scala.util.Random

  val rng2 = rng.nextDouble
  val rng3 = rng.nextDouble
  val rng4 = rng.nextInt
  val rng5 = rng.nextInt(10)

  def rollDie: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }

  def rollDie(rng: scala.util.Random): Int = rng.nextInt(6)

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }

  def notRandomPair(rng: RNG): (Int,Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

//  def nonNegativeEven: Rand[Int] =
//    map(nonNegativeInt)(i => i - i % 2)

//  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
//    map2(ra, rb)((_, _))

//  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
//    val (i, rng2) = nonNegativeInt(rng)
//    val mod = i % n
//    if (i + (n-1) - mod >= 0)
//      (mod, rng2)
//    else nonNegativeLessThan(n)(rng)
//}

  //ex6_1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextVal, nextRng) = rng.nextInt
    if( nextVal < 0 ) {
      val (newVal, newRng) = nonNegativeInt(nextRng)
      (newVal, newRng)
    } else {
      (nextVal, nextRng)
    }
  }

  //ex6_2
  def double(rng: RNG): (Double, RNG) = {
    val (nextVal, nextRng) = rng.nextInt
    val doubleValue = ( nextVal.toDouble / (Int.MaxValue+1) )
    (doubleValue, nextRng)
  }

  //ex6_3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, nextIntRng) = nonNegativeInt(rng)
    val (nextDouble, nextDoubleRng) = double(nextIntRng)
    ( ( nextInt , nextDouble ), nextDoubleRng )
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (nextDouble, nextDoubleRng) = double(rng)
    val (nextInt, nextIntRng) = nonNegativeInt(nextDoubleRng)
    ( ( nextDouble , nextInt ), nextIntRng )
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (nextDouble1, nextDoubleRng1) = double(rng)
    val (nextDouble2, nextDoubleRng2) = double(nextDoubleRng1)
    val (nextDouble3, nextDoubleRng3) = double(nextDoubleRng2)
    ( (nextDouble1, nextDouble2, nextDouble3), nextDoubleRng3 )
  }

  //ex6_4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =  {

    def loop( count: Int)( rng: RNG )(list: List[Int]) : (List[Int], RNG) = count match {
      case 0 => (list, rng)
      case _ => {
        val (newVal, newRng) = nonNegativeInt(rng)
        val (nextVal, nextRng) = loop( count-1 )( newRng )( newVal :: list )
        ( nextVal, nextRng )
      }
    }

    loop( count )( rng )( List() )
  }

  //ex5_5 - solution down there
  val rngGod = SimpleRNG(42)
  val (n1, rngGod2) = rngGod.nextInt
  val (n2, rngGod3) = rngGod2.nextInt
  val (pleb1, pleb2) = notRandomPair( rngGod )
  val ( (n3, n4), rngGod4 ) = randomPair( rngGod )

  //ex6_5
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  //Rand[A] is just a type alias for a function type RNG => (A, RNG)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  val rngGod5 = map( unit(1) )( (x: Int) => x+1 )

  //ex6_6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B ) => C): Rand[C] = {
    rng => {
      val (aVal, aRng) = ra(rng)
      val (bVal, bRng) = rb(aRng)
      ( f(aVal, bVal), bRng )

    }
  }

  //ex6_7 - something like that... broken syntax, fuck it.
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//
//    def loop( fs: List[Rand[A]] )( list: List[A] ): Rand[ List[A] ] = fs match {
//      case Nil => unit(list)
//      case Cons(h,t) => {
//        rng => {
//          val (aVal, aRng) = h(rng)
//          (aVal :: loop(t)(list) )
//        }
//      }
//    }
//
//    loop( fs )
//  }

  //ex6_8

  val gifNextInt: Rand[Int] = x => x.nextInt

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val( fVal, fRNG ) = f(rng)
      g(fVal)(fRNG)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap( gifNextInt )( i => (rng => ( n , rng ) ) )
  }

  def mapInTermsOfFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)( a => unit( f(a)) )

  def map2InTermsOfFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B ) => C): Rand[C] = {
    flatMap( ra )( a =>
      flatMap( rb )( b =>
        unit( f(a, b) )
      )
    )
  }

  val ex6_1_1 = nonNegativeInt( rngGod )
  val ex6_1_2 = nonNegativeInt( rngGod2 )
  val ex6_1_3 = nonNegativeInt( rngGod3 )
  val ex6_2 = double( rngGod )
  val ex6_3_1 = intDouble( rngGod )
  val ex6_3_2 = doubleInt( rngGod )
  val ex6_3_3 = double3( rngGod )
  val ex6_4 = ints(5)( rngGod )

  val ex6_5 = map( nonNegativeInt )( (_ / (Int.MaxValue.toDouble + 1) ) )( rngGod )

  val ex6_6 = map2( nonNegativeInt, double )( (x: Int, y: Double) => x+y )( rngGod )
  val ex7_prep = List.fill( 3 )( ints(2)( rngGod ) )

  val ex6_8 = nonNegativeLessThan( 5 )( rngGod )
  val ex6_9_1_t = map( gifNextInt )( (i: Int) => i+1 )( rngGod )
  val ex6_9_1 = mapInTermsOfFlatMap( gifNextInt )( (i: Int) => i+1 )( rngGod )
  val ex6_9_2_t = map2( gifNextInt, gifNextInt )( (x: Int, y: Int) => x+y )( rngGod )
  val ex6_9_2 = map2InTermsOfFlatMap( gifNextInt, gifNextInt )( (i: Int, y: Int ) => i+y )( rngGod )


}

object c6State {

  //ex6_9
  import State._

  case class State[S, +A](run: S => (A, S)) {
    type State[S,+A] = S => (A,S)

    def map[B](f: A => B): State[S, B] =

      //State signature is - S => (A, S)
      //State is a transformation function. It takes the previous State and returns a (new value, new state)
      //Transformation function == automaton
      (state: S) => {
        val (currA, newState) = run( state )
        val currB = f(currA)
        (currB, newState)
      }

//    val ex6_10_2 = map( unit )( (s: State) => {
//      val (currA: Int, newState: State) = run(s)
//      ( currA+1, newState)
//    } )
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] =
      State( state => ( a, state ) )

    def unit2[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }
//
//  val gifNextInt: Rand[Int] = x => x.nextInt
  def random_int :Rand[Int] =  State (_.nextInt)
  val randomInt = random_int
  val rngGod = SimpleRNG(42)

  val ex6_10_1_s = unit2( rngGod ).run( 27 )
  val ex6_10_1 = unit( rngGod ).run( 27 )
//  ex6_10_1._1.map( ex6_10_1 )( (x: Int) => x+1 )
  //.... I have no clue how to pass proper data.

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    val machine = Machine( true, 10, 0 )
//    State (
//      ( machine, ( machine.candies, machine.coins ) )
//    )
//  }

}

