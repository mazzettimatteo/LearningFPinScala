/*Es 2.1
Scrivi una funzione ricorsiva per fibonacci. 
Questa funzione deve usare una funzione locale ricorsiva in coda.
Sequenza di fib: 0,1,1,2,3,5,..
*/

object fibonacci{
    def calculate(n: Int): Int ={
        @annotation.tailrec
        def go(n: Int, prev: Int, curr: Int): Int ={
            if(n==0) prev
            else go(n-1, curr, prev+curr)
        }

        go(n, 0, 1)
    }

    def main(args: Array[String]): Unit =
        println(calculate(6))
}