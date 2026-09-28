package fpinscala.eithererrorhandler

sealed trait Either[+E,+A] {
  //Es 4.6, implementa map, flatMap, orElse, map2 per Either
  def map[B](f: A=>B): Either[E, B] = this match{
    case Right(a) => Right(f(a))
    case Left(e)=>Left(e)
  }
  def flatMap[EE>:E, B](f: A=>Either[EE,B]): Either[EE,B] =this match{
    case Left(e) => Left(e)
    case Right(value) => f(value) 
  }
  def orElse[EE>:E,B>:A](b: =>Either[EE,B]): Either[EE, B] =this match{
    case Left(e) => b
    case Right(value) => Right(value) 
  }
  def map2[EE>:E, B, C](b: Either[EE,B])(f:(A,B)=>C): Either[EE,C] ={
    for{ //for-comprehension
        a <- this //Tutte le estrazioni prima dell'ultima vengono chiamate a flatMap
        b1 <- b //L'ultima si traduce in una chiamata a map con arg b
    } yield f(a,b1)
  //questo for traduce a this.flatMap(a=>b.map(b1=>f(a,b1)))
  } 
}
//La differenza fra Option e Either è che Either fornisce più informazioni:
//Right è usato nel caso sia tutto corretto, Left per gli errori 
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
    def mean(s: IndexedSeq[Double]): Either[String, Double]=
        if(s.isEmpty)
            Left("mean of empty list")
        else
            Right(s.sum/s.length)

    def division(p: Double, q: Double): Either[Exception, Double] =
        try Right(p/q)
        catch{case e: Exception => Left(e)}


    def Try[A](a: =>A): Either[Exception,A] =
        try Right(a)
        catch{case e: Exception => Left(e)}
}