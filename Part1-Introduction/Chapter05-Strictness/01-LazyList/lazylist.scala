package fpinscala.laziness

sealed trait Stream[+A]{
    def headOption: Option[A]= this match{
        case Cons(h, t) => Some(h()) //Forzo esplicitamente il thunk di h usando h()
        case Empty => None
    }

    //Es 5.1, scrivi una funz che converta una Stream in una List, che forzerà la sua evaluation.
    def toList: List[A] = this match{
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }
    def toList_tailrec: List[A] = {
    @annotation.tailrec
        def loop(lazyl: Stream[A], acc: List[A]): List[A]= lazyl match{
            case Cons(h, t) => loop(t(), h()::acc) //il secondo argomento sarebbe acc::h() ma per associatività non posso farlo, quindi reverso acc allla fine
            case Empty => acc.reverse
        }

        loop(this, Nil)
    }

    
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=> A, t:()=>Stream[A]) extends Stream[A] //Nella def dei casi è necessario esplicitare i thunk ()=>Something, perchè non si può usare passaggio per nome

object Stream{
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] ={ //costruttore smart per creare, senza dover passare funz ()=>val delle stream non vuote 
        lazy val head=hd //Con lazy facciamo sì che head e tail vengano cacheati per evitare ripetizioni di evaluation
        lazy val tail=tl
        Cons(()=>head, ()=>tail)
    }

    def empty[A]: Stream[A] = Empty //Costruttore smart per creare empty stream di un tipo particolare

    def apply[A](as: A*): Stream[A]= //* rende la funz variadica: accetta n numero indef di args di tipo A
        if(as.isEmpty) empty else cons(as.head, apply(as.tail*))

}