package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{

    def apply[A](alist: A*): List[A] ={ 
        if(alist.isEmpty) Nil
        else Cons(alist.head, apply(alist.tail*)) 
    }

    //Es 3.2 implementa tail che rimuove il primo elem da una lista Fallo in O(1) Che scelte puoi fare se List è Nil?
    def tail[A](alist: List[A]): List[A] =alist match{
        case Nil => Nil //oppure potrei fare: sys.error("tail on empty list")
        case Cons(_,tail) => tail
    }

    //Es 3.3 implementa setHead per cambiare il valore del primo elem di List
    def setHead[A](alist: List[A], newVal: A): List[A]= alist match{
        case Nil => Nil
        case Cons(_,tail) => Cons(newVal, tail)
    }

    //Es 3.4 generalizza tail alla funzione drop, che rimuove i primi m elementi dalla lista, tempo O(m)
    def drop[A](alist: List[A], m: Int): List[A] = {
        if(m==0) alist
        else alist match{
            case Nil => Nil
            case Cons(head, tail) => drop(tail, m-1)
        }
    }

    //Es 3.5 dropWhile deve rimuovere gli elementi in testa alla lista finchè matchano il predicato f
    def dropWhile[A](alist: List[A], f: A=>Boolean): List[A] =alist match{
        case Nil => Nil
        case Cons(head, tail) => {
            if(f(head)) dropWhile(tail, f)
            else alist
        }
        /*Oppure usando le patternguards:
        case Cons(h, t) if f(h) => dropWhile(t,f)
        case _ => alist
        */
    }

    //Es 3.6 Scrivi init che ritorna una list contenente tutti gli elementi tranne l'ultimo di l
    def init_1[A](l: List[A]): List[A] =l match{
        case Nil => sys.error("init_1 called with empty list as arg")
        case Cons(_,Nil) => Nil 
        case Cons(head, tail) => Cons(head, init_1(tail))
    } 
    def init_2[A](l: List[A]): List[A]={ //Questo metodo usa un buffer, ma non lo espone fuori, preservando la trasparenza referenziale
        import collection.mutable.ListBuffer
        val buff= new ListBuffer[A]
        def go(curr: List[A]): List[A] = curr match{
            case Nil => sys.error("init_2 called with empty list as arg")
            case Cons(_, Nil)=>List(buff.toList*)
            case Cons(head, tail) => {
                buff+=head
                go(tail)
            }

        } 

        go(l)
    }

    def dropWhile_2[A](alist: List[A])(f: A=>Boolean): List[A] = alist match{
        case Nil => Nil
        case Cons(head, tail) => {
            if(f(head)) dropWhile_2(tail)(f) //In pratica dropWhile_2 ritorna una funzione che chiamiamo con l'argomento f, quindi facciamo currying
            else alist
            /*Currying: 
            In programmazione funzionale, una funzione che accetta due parametri, con firma (A, B)=>C è A=>B=>C ossia A=>(B=>C).
            Quando il tuo codice esegue l'istruzione dropWhile_2(tail)(f), il compilatore valuta l'espressione in due passaggi sequenziali:
            Passaggio 1: dropWhile_2(tail)
            Il programma passa solo il primo argomento (la coda della lista). Poiché manca il secondo blocco di parametri, il metodo non restituisce la List[A] finale, ma restituisce un oggetto funzione temporaneo, la cui firma è (A => Boolean) => List[A]. Questa funzione "ricorda" che la lista su cui operare è tail ed è in attesa del predicato.
            Passaggio 2: L'applicazione di (f)
            Immediatamente dopo, il codice affianca (f). Questo invoca la funzione temporanea appena generata passandole l'argomento mancante, completando l'operazione e restituendo finalmente la List[A]
            */
        } 
    }

    def main(args: Array[String]): Unit ={
        val ilist: List[Int] = List(1,2,3,4,5)
        val ilist_mod=dropWhile(ilist, (x: Int)=>x<4) 
        //Perchè devo dire che la funzione anonima per dropWhile prende un Int, non lo può capire Scala da solo dato l'altro arg di dropWhile?
        //Per "risolvere" definisco dropWhile in maniera diversa (qui per non cancellare l'es ho scritto dropWhile_2)
        val ilist_mod_2=dropWhile_2(ilist)(x=>x<4) //Qui non ho dovuto annotare il tipo di x, scala l'ha dedotto da solo
        //In generale se la funzione contiene più gruppi di argomenti le annotazioni di tipo scorrono da sx a dx fra questi gruppi
        
    }
}

