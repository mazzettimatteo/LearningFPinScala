//package kebab.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
    def apply[A](alist: A*): List[A] ={
        if(alist.isEmpty) Nil
        else Cons(alist.head, apply(alist.tail*)) 
    }

    def sum_1(ilist: List[Int]): Int = ilist match{
        case Nil => 0
        case Cons(head, tail) => head+sum_1(tail)
    }

    def product_1(dlist: List[Double]): Double = dlist match{
        case Nil => 1.0
        case Cons(head, tail) => head*product_1(tail)
    }

    def foldRight[A,B](alist: List[A], baseVal: B)(f: (A,B)=>B): B = alist match{ //Siccome il corpo di sum_1 e product_1 è molto simile, allora posso creare una funzione che con i dovuti argomenti permette di implementare più genericamente sum e product
        case Nil => baseVal
        case Cons(head, tail) => f(head,foldRight(tail, baseVal)(f))
    }

    def sum_2(ilist: List[Int]): Int =
        foldRight(ilist,0)((x,y)=>x+y)
    
    def product_2(dlist: List[Double]): Double =
        foldRight(dlist,1.0)(_*_) //Notazine per (x,y)=>x*y 

    //Es. 3.9 calcola la lunghezza du una lista usando rightfold, vediamo prima come scriverla senza Rightfold
    def len_1[A](alist: List[A]): Int =alist match{
        case Nil => 0
        case Cons(_, tail) =>1+len_1(tail) 
    }

    def len_2[T](tlist: List[T]): Int =
        foldRight(tlist,0)((_,res)=>res+1)

    //Es 3.10 foldRight non è tailrec, scrivi foldLeft tailrec
    @annotation.tailrec
    def foldLeft[A,B](alist: List[A], baseVal: B)(f: (B,A)=>B):B= alist match{
        case Nil => baseVal
        case Cons(head, tail) => foldLeft(tail,f(baseVal, head))(f)
    }

    //Es 3.11 scrivi sum, product e len con foldLeft
    def sum_3(ilist: List[Int]): Int =
        foldLeft(ilist,0)(_+_)
    def prod_3(dlist: List[Double]): Double =
        foldLeft(dlist,1.0)(_*_)
    def len_3[T](tlist: List[T]): Int =
        foldLeft(tlist,0)((acc,_)=>1+acc)
    
    //Es 3.12 scrivi una funzione reverse_1. Controlla se puoi scriverla con fold.
    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match{
        case Nil => l2
        case Cons(head, tail) => Cons(head, append(tail,l2))
    }
    def reverse_1[A](alist: List[A]): List[A] = alist match{
        case Nil => Nil
        case Cons(head, tail) => append(reverse_1(tail), List(head))
    } 
    def reverse_2[A](alist: List[A]): List[A] =
        foldLeft(alist, List[A]())((stuff,head)=>Cons(head,stuff))
    
    
    //Es 3.13 Implementa foldLeft usando foldRight. Implementa foldRight usando foldLeft.
    //def foldLeft_usingFR[A,B](alist: List[A], baseVal: B)(f: (B,A)=>B):B=     
    def foldRight_usingFL[A,B](alist: List[A], baseVal: B)(f: (A,B)=>B): B = 
        foldLeft(reverse_2(alist),baseVal)((b,a)=>f(a,b))

    //Es 3.14 implementa append usando Fold
    def append_2[A](l1: List[A], l2: List[A]): List[A] =
        foldRight(l1,l2)(Cons(_,_))

    //Es 3.15 implementa concat che concatena una lista di liste in una singola lista. Usa le funzioni introdotte sopra
    def concat[A](ll: List[List[A]]): List[A] =ll match{
        case Nil => Nil
        case Cons(hList, tListList) => append(hList, concat(tListList)) 
    }
    def concat_2[A](ll:List[List[A]]): List[A] =
        foldRight(ll, Nil: List[A]) (append)

    //Es 3.16, scrivi una funzione che trasforma una lista di interi aggiungendo 1 ad ogni elem. 
    //La funzione deve essere pure e ritornare una nuova lista
    def add1(l: List[Int]): List[Int] = l match{
        case Nil => Nil
        case Cons(head, tail) => append(List(head+1),add1(tail))
    }
    def add1_1(l: List[Int]): List[Int] = l match{
        case Nil => Nil
        case Cons(head, tail) => Cons(head+1,add1_1(tail))
    }
    def add1_2(l: List[Int]): List[Int] =
        foldRight(l,Nil:List[Int])((h,acc)=>Cons(h+1,acc))

    //Es 3.17, scrivi una funzione che trasforma una List[Double] in una List[String]
    //Usa val.toString per trasformare un val in testo
    def doubToStr(l: List[Double]): List[String] =
        foldRight(l,Nil: List[String])((h,acc)=>Cons(h.toString, acc)) 
    
    //Es 3.18, scrivi map che generalizzi la modifica di ogni elem di una lista mantenendone la struttura
    def map[A,B](alist: List[A])(f: A=>B): List[B] ={
        foldRight(alist, Nil: List[B])((h,t)=>Cons(f(h),t))
    }

}