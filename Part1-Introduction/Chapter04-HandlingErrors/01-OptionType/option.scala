package fpinscala.errhandlingopt

sealed trait Option[+A]{
    //Es 4.1, implementa le seguenti funzioni: map, flatMap, getOrElse, orElse, filter
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

    //Lift serve a trasformare ogni funzione che abbiamo in mod che possa operare su tipi Option
    def lift[A,B](f: A=>B): Option[A] =>Option[B] =
        _.map(f)
    //Ad esempio se vogliamo fare l'absolute val di un option usiamo lift per scrivere absolute
    val absolute: Option[Double]=>Option[Double] = lift(math.abs)

    //Try è una funzione usata per trasformare le API da exception-based a Option-based
    def Try[A](a: =>A): Option[A] =
        try Some(a)
        catch{ case e: Exception => None}

    //Es 4.3, scrivi map2 che combina due Option usando una funzione binaria f.
    def map2[A,B,C](oa: Option[A], ob: Option[B])(f:(A,B)=>C): Option[C]= (oa,ob) match{
        case (None,_) => None
        case (_, None) => None
        case (Some(a), Some(b)) =>Some(f(a,b))
    }
    def map2_2[A,B,C](oa: Option[A], ob: Option[B])(f:(A,B)=>C): Option[C] =
        oa.flatMap(a=>ob.map(b=>f(a,b)))

    //Es 4.4, scrivi sequence che combina una lsit di Option  in una Option contenente la lista di tutti i Some
    //Se la lista originale contienealmeno un None ritorna None.
    def sequence[A](l: List[Option[A]]): Option[List[A]] =l match{
        case Nil => Some(Nil)
        case oh::ot => oh.flatMap( h =>sequence(ot).map(t=> h::t))
    }
    //Es 4.5, implementa traverse e poi implementa sequence usando traverse
    def traverse[A,B](al: List[A])(f: A=>Option[B]): Option[List[B]] = al match{
        case Nil => Some(Nil)
        case h::tail => map2(f(h),traverse(tail)(f))(_::_)
    }
    def sequence_2[A](l: List[Option[A]]): Option[List[A]] =
        traverse(l)(x=>x)


}


