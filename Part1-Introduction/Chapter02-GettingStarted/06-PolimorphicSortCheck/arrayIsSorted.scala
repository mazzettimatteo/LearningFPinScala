/*Es 2.2 implementa una funzione parametrica che controlla se un'array è ordinato data una funzione di confronto ordered: (A,A)=>Boolean*/

object modulelele{

    def isSorted[A](as: Array[A], ordered: (A,A)=>Boolean): Boolean ={
        def loop(n: Int): Boolean =
            if(n+1>=as.length) true
            else if(!ordered(as(n),as(n+1))) false
            else loop(n+1)

        loop(0)
    }
}