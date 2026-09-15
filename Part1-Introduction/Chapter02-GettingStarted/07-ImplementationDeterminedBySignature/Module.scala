/*
Es 2.3, 2.4 Esiste una sola implementazione valida per una segnatura parametrica di currye, quale? E per uncurry? 
Es 2.7 Anche per compose l'implementazione è completamente determinata dalla firma della funzione.
*/
object MazzoModule{

    def curry[A,B,C](f: (A,B)=>C ): A=>(B=>C) ={
        //(a: A) => ( (b: B) => f(a,b) )        
        a=>b=>f(a,b) //Posso scrivere così perchè scala usa inferenza di tipo per capire i tipi di a, b, f(a,b)
        /*
        Il metodo seguito è questo: per ogni parametro che non "possiedo" negli argomenti mi invento una variabile associata:
        - per A uso a
        - per B uso b
        C invece la posso ottenere in un solo modo, usando la funzione f data in input, dunque la ricavo come f(a,b)
        */
    }

    def uncurry[A,B,C](f: A=>(B=>C) ): (A,B)=>C ={ //Nota che A=>(B=>C) per l'associatività dx di => è uguale ad A=>B=>C
        (a: A, b: B)=>f(a)(b)
    }


    def compose[A,B,C](f: B=>C, g: A=>B): A=>C ={
        (a: A)=>f(g(a))
        /*
        Compose è così utilizzata nel FP che è un metodo della classe delle funzioni:
        Per comporre due funzioni f(g()) si scrive in uno dei due modi:
        f compose g 
        g andThen f
        */
    }

}

