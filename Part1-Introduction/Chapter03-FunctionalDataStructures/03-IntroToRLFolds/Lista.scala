//package kebab.datastructures
import javax.net.ssl.TrustManager

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

    //Es 3.19, scrivi filter che rimuove da una lista tutti gli elem che non soddisfano un certo predicato.
    def filter[A](l: List[A])(f: A=>Boolean): List[A] =l match{
        case Nil => Nil
        case Cons(head, tail) =>{
            if(f(head)) Cons(head,filter(tail)(f))
            else filter(tail)(f)
        } 
    }
    def filter_1[A](l:List[A])(f: A=>Boolean): List[A] =
        foldRight(l,Nil: List[A])((h,t)=>{if f(h) then Cons(h,t) else t})
    
    //Es 3.20, scrivi flatmap che funziona come map ma la funz in input ritorna una lista invece che un risultato.
    //Eg: flatMap(List(1,2,3))(i=>List(i,i)) results in List(1,1,2,2,3,3)
    def flatMap[A,B](l:List[A])(f: A=>List[B]): List[B] =l match{
        case Nil => Nil
        case Cons(head, tail) => append(f(head), flatMap(tail)(f))
    }
    def flatMap_2[A,B](l:List[A])(f: A=>List[B]): List[B] ={
        foldRight(l,Nil)((h,t)=>append(f(h),t))
    }
    def flatMap_3[A,B](l:List[A])(f: A=>List[B]): List[B] =
        concat(map(l)(f))

    //Es 3.21, usa flatMap per implementare filter
    def filter_2[A](l:List[A])(f: A=>Boolean): List[A] =
        flatMap(l)(x=>{if f(x) then List(x) else Nil})

    //Es 3.22, scrivi una funz che accetti due liste e ne costruisca una nuova sommando gli elementi corrispondenti di ciascuna lista
    // addTogether(List(1,2,3), List(4,5,6)) results in List(5,7,9)
    def addTogether(l1: List[Int], l2: List[Int]):List[Int] =(l1,l2) match{
        case (Nil,_) => Nil
        case (_,Nil) => Nil
        case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,addTogether(t1,t2)) 
    }

    //Es 3.23, generalizza la funzione in modo che valga per qualsiasi tipo
    def zipWith[A](l1: List[A], l2: List[A])(f:(A,A)=>A): List[A] =(l1,l2) match{
        case (Nil,_)=>Nil
        case (_,Nil)=>Nil
        case(Cons(h1,t1), Cons(h2,t2))=>Cons(f(h1,h2),zipWith(t1,t2)(f))
    }

    //Es 3.24, implementa hasSubsequence che controlla se l1 ha come sottolista l2
    @annotation.tailrec
    def helper[A](l1: List[A], l2: List[A]): Boolean =(l1,l2) match{
        case (Nil,Nil)=>true
        case (Nil,_)=>false
        case (_,Nil)=>true
        case(Cons(h1,t1), Cons(h2,t2))=>{if (h1==h2) then helper(t1,t2) else false}
        
    }
    def hasSubsequence[A](l1: List[A], l2: List[A]): Boolean =(l1,l2) match{
        case (_,Nil) => true
        case (Nil,_) => false
        case (Cons(h1,t1), Cons(h2,t2)) => {
            if ((h1==h2) && helper(t1,t2)) true
            else hasSubsequence(t1,l2)
        }

    }
}