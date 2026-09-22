package mazzo.datastructures //Definisce il namespace in cui vengono definiti i traits

//Un trait è una interfaccia astratta che può contenere l'implementazione di qualche metodo. 
//Sealed indica che tutte le implementazione dei metodi devono avvenire in questo file 
//Il [+A] indica che la lista è parametrica sul tipo A. Il + indica che il parametro A è un parametro covariante di List
//Quindi List[Dog] è considerato sottotipo di List[Animal] se assumo che Dog è sottotipo di Animal 
sealed trait List[+A] 
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
//I due case specificanio quali sono le due forme che questa lista può assumere. Sono detti "data constructor"

object List{ //Companion Object, contiene funzioni per lavorare sulle liste

    def sum(ilist: List[Int]): Int = ilist match{ //Pattern matching(Se più pattern matchano l'espressione target, Scala sceglie il primo che matcha)
        case Nil => 0
        case Cons(headVal,tail) => headVal+sum(tail)
    }

    def prod(dlist: List[Double]): Double = dlist match{
        case Nil => 1
        case Cons(0.0, _) => 0.0 // _ indica che non ci interessa la lista che segue
        case Cons(headVal, tail) => headVal*prod(tail)
    }

    def apply[A](alist: A*): List[A] ={ //Funzione variadica(accetta un numero variabile di args)
        //E' solo zucchero sintattico per creare una lista passandole una sequenza Seq di elementi separati da virgola.
        //Seq in Scala è l'interfaccia per oggetti ordinati come code, liste e vettori
        if(alist.isEmpty) Nil
        //else Cons(alist.head, apply(alist.tail: _*)) // _* ci permette di passare Seq a un metodo variadico, è DEPRECATO
        else Cons(alist.head, apply(alist.tail*)) //Metodo per Scala 3+
    }

    def main(args: Array[String]): Unit ={
        val iList: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
        val emptyList: List[String]=Nil
        println(sum(iList))
    }
}

/*
NB:
in questo caso stiamo definendo noi una struttura List, ma in realtà questa esiste già in scala e ha queste differenze:
- Cons(h,tail) è in realtà h::tail, dove :: è associativo a dx e quindi si può scrivere h::k::tail al posto di Cons(h,Cons(k,tail))
- h::tail è zucchero sintattico per h.::(tail)
- esistono tante funzioni già esistenti per List con specifiche nella documentazione ufficiale
*/