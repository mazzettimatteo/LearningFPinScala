/*
Per eseguire questo programma che è un solo file
> scala FirstModule.scala   
Altrimenti utilizzare REPL(Read-Eval-Print Loop)
> scala
...
scala> :load FirstModule.scala
...
scala> MyModule.abs(-89)
...
scala> :exit
*/

object MyModule{ //Il nome del file e quello dell'oggetto non devono coincidere per forza, come in questo caso
    def abs(n: Int): Int = //Non servono le graffe perchè il corpo della funzione contiene un solo statement
        if(n<0) -n
        else n
    

    private def formatAbs(x: Int) ={ //Il metodo è privato, quindi posso non dichiarare il tipo di ritorno, tanto questo metodo lo userò solo io
        val msg="The absoulute value of %d is %d"
        msg.format(x,abs(x)) //String.format ritorna una stringa quindi Scala inferisce che formatAbs ha tipo Int=>String
    }

    def main(args: Array[String]): Unit = //main è una funzione impura(come suggerisce il tipo di ritorno Unit)
        println(formatAbs(-42)) //questo è un side-effect che la rende impuro. Notare come println ritorna Unit  
}