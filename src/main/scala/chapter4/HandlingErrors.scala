//package chapter4

import java.util.List

object HelloWorld  {

  def main(args: Array[String]) {
    println( "Triggered main");
    exampleFunc( "Mityo Krika" )
  }

  def exampleFunc(name: String): Unit = {
    println("Hello World, " + name + "!")
  }
}

object Failer {

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  //Failer.mean( Seq() )
  //Failer.mean( Seq(1, 2, 3) )
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  //Failer.mean_1( IndexedSeq(1,2,3) , 23.0 )
  //Failer.mean_1( IndexedSeq() , 23.0 )
  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

}
object Handin {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(v) => f(v)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(v) => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case None => None
      case Some(v) => if(f(v)) this else None
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {

    def mean( xs: Seq[Double] ): Option[Double] = {
      if( xs.isEmpty )
        None
      else
        Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] = { //xs match {
//      case h :: t => Some(h)
      val m = mean(xs)
      m.flatMap( (x:Double) => mean( xs.map( seqElem => x + seqElem ) ) ) //iskame mean za da vurnem option
    }

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B ) => C): Option[C] = a match {
      case None => None
      case Some(lh) => b match {
        case None => None
        case Some(rh) => Some( f( lh,rh ) )
      }
    }

//

//      a match {
//      case Nil => Some(Nil)
//      case x :: xs => None
//    }

//
//    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
//      case Nil => Some(Nil)
//      case x :: xs => f( List(x, traverse(xs)(f) ) )
//    }

  }

//  def sequence[A]( a: List[ Option[A] ] ): Option[ List[A] ] = {
//
//    def loop( a: List[ Option[A] ] ): Option[ A ] = a match {
//      case Nil => None
//      case Cons(x,xy) => x
//    }
//
//    loop( a )
//    Option( List( a ) )
//  }

  val d1 = Some("cola")
  val d2 = None
  val d3 = Some("Pesho we")
  val ex4_1_1 = d2.map( (x:String) => x + " Boombaye!" )
  val ex4_1_2 = d3.map( (x:String) => x + " Boombaye!" )
  val ex4_1_3 = d2.flatMap( (x:String) => Some(x + " Boombaye!") )
  val ex4_1_4 = d3.flatMap( (x:String) => Some(x + " Boombaye!") )
  val ex4_1_5 = d2.getOrElse(" kaji chestno :D!" )
  val ex4_1_6 = d3.getOrElse(" kaji chestno :D!" )
  val ex4_1_7 = d2.orElse(Some("2nd opciq"))
  val ex4_1_8 = d3.orElse(Some("2nd opciq"))
  val ex4_1_9 = d2.filter( (x: String ) => x == "Pesho we")
  val ex4_1_10 = d3.filter( (x: String ) => x == "Pesho we")
  val ex4_1_11 = d1.filter( (x: String ) => x == "Pesho we")
  val ex4_2_1 = Option.variance( Seq(1,2,3) )
  val ex4_2_2 = Option.variance( Seq() )
  val ex4_3_1 = Option.map2( d3, d1 )( (a: String, b: String) => a+b )
  val ex4_3_2 = Option.map2( d3, d2 )( (a: String, b: String) => a+b )

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(v) => Left(v)
      case Right(v) => Right( f(v) )
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(v) => b
      case Right(v) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B ) => C): Either[EE, C] =
      this.flatMap( aa => b.map( bb => f(aa, bb) ) )
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
//  case class Left2[+E](value: List[E]) extends Either[E, Nothing]

  object Either {
//    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
//      case Nil => Right(List())
//      case List(_) => Right(List())
//    }
  }

  val d4 = Left(None)
  val d5 = Right( "Success")
  val d6 = Right( "bo")
  val ex4_6_1 = d4.map( (x: String) => x + " Gosho" )
  val ex4_6_2 = d5.map( (x: String) => x + " Gosho" )
  val ex4_6_3 = d4.flatMap( (x: String) => if(x.length > 3) Right(x + "veri nais") else Left("Not long enough :(") )
  val ex4_6_4 = d5.flatMap( (x: String) => if(x.length > 3) Right(x + "veri nais") else Left("Not long enough :(") )
  val ex4_6_5 = d6.flatMap( (x: String) => if(x.length > 3) Right(x + "veri nais") else Left("Not long enough :(") )
  val ex4_6_6 = d4.orElse( Right("veri nais") )
  val ex4_6_7 = d5.orElse( Right("veri nais") )
  val ex4_6_8 = d4.map2( d5 )( (a: String, b: String ) => Right(a+b) )
  val ex4_6_9 = d6.map2( d5 )( (a: String, b: String ) => Right(a+b) )
//  val ex4_7_1 = Either.sequence( List( Right("A"), Right("B"), Right("C") ) )

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))


  var d7_name = new Name("Me")
  var d7_age = new Age(24)
  val d7 = Person(d7_name,d7_age)

  var ex4_8_1 = mkName("Me")
  var ex4_8_2 = mkAge(24)

  def isPersonAllowed( name: String, age: Int): Either[ String, Person ] = {
    mkName(name).map2( mkAge(age) )( ( a, b ) => {
      println("a is : " + a )
      println("b is : " + b )
      Person( new Name(name), new Age(age) )
    } )
  }
}


