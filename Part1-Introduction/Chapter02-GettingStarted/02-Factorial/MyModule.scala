
object Mymodule{

    def factorial(n: Int): Int ={
        @annotation.tailrec
        //Questa annotazione serve per generare un errore se la funzione non è effettivamenet ricorsiva in coda, e quindi genera più di un RdA
        def go(n: Int, res: Int): Int ={
            if(n<=0) res
            else go(n-1, n*res)
        }

        go(n,1)
    }

    def main(args: Array[String]): Unit =
        println(factorial(7))
}