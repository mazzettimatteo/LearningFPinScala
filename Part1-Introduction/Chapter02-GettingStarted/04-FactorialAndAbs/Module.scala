object Module{

    def abs(n: Int): Int =
        if(n<0) -n
        else n 
    
    def fact(n: Int): Int =
        @annotation.tailrec
        def loop(n: Int, res: Int): Int =
            if(n<=0) res
            else loop(n-1, n*res)
        loop(n,1)
        
    def formatResult(name: String, n: Int, f: Int=>Int): String ={ //Higher Order Function (HOF) that takes another function (f) as argument
        val msg="The %s of %d is %d"
        msg.format(name,n,f(n))
    }

    def main(args: Array[String]): Unit =
        println(formatResult("factorial", 5, fact))
        println(formatResult("absolute value", -5, abs))
}