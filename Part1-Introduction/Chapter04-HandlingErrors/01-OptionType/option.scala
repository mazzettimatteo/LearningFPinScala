sealed trait Option[+A]{
    //Es 4.1, implementa le seguenti funzioni
    //Map può essere usata per trasformare un risultato dentro un Option
    def map[B](f: A=>B): Option[B] = this match{
        case None => None
        case Some(a)=>Some(f(a))
    }
    def flatMap[B](f:A=>Option[B]): Option[B] = this match{
        case None => None
        case Some(a) => f(a)
    }
    //Ritorna il valore default nel caso None, altrimenti il val giusto
    def getOrElse[B>:A](default: =>B): B = this match{ 
        //B>:A vuol dire B supertipo di A. 
        //Il tipo =>B indica che l'arg è di tipo B ma non verrà valutato fichè la funzione non lo necessita
        case None => default
        case Some(a) => a 
    }
    //Ritorna la prima Option se è definita, altrimenti la seconda
    def orElse[B>:A](o: =>Option[B]): Option[B] =
        map(Some(_)).getOrElse(o) //Da Some(a) ottengo Some(Some(a)) che attraverso getorElse diventa Some(a), da None ottengo Some(None) e poi None
    def filter(f: A=>Boolean): Option[A] ={
        flatMap(a => {if f(a) then Some(a) else None})
    }
    
}
//Si può pensare al tipo Option come una lista che ha al più un elem
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{
    def mean(s: Seq[Double]): Option[Double] = 
        if(s.isEmpty) None
        else Some(s.sum / s.length)

    //Es 4.2, definisci la funzione per calcolare la varianza usando flatMap
    def sigmaSquared(s: Seq[Double]): Option[Double] ={
        mean(s).flatMap(mediaDiSeq=> mean(s.map(value=>math.pow(value-mediaDiSeq, 2))))
    }
}


