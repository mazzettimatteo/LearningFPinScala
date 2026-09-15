//Test del metodo con funzione anonima (o function literal) usando REPL:
/*
> scala
scala> :load findFirst.scala
scala> Module.findFirst(Array(7,9,42), (x: Int)=>x==9)
val res1: Int = 1
*/
object Mod{

    def findFirst[T](arr: Array[T], p: T=>Boolean): Int={ //Polimorfismo parametrico sul parametro T
        def loop(n: Int): Int =
            if(n>=arr.length) -1
            else if(p(arr(n))) n //p è una funzione per implementare il controllo di uguaglianza
            else loop(n+1)

        loop(0)
    }
}