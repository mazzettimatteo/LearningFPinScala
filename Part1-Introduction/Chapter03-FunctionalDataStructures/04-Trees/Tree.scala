sealed trait Tree[+A]
case class Leaf[A](val: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

    //Es 3.25, scrivi una funzione size che conta il numero di nodi(leaf o branch) di un tree
    def size[A](t: Tree[A]): Int = t match{
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right) + 1
    }

    //Es 3.26, scrivi maximum che ritorna l'elemento più grande in un albero. 
    //Le funzioni "x.max(y)" e "x max y" ritornano il massimo fra due interi x e y
    def maximum(t: Tree[Int]): Int = t match{
        case Leaf(x) => x 
        case Branch(left, right) => maximum(left).max(maximum(right))
    }

    //Es 3.27, scrivi depth che calcola la profondità massima dell'albero
    def depth[A](t: Tree[A]): Int = t match{
        case Leaf(_) => 0
        case Branch(left, right) => 1 + depth(left).max(depth(right))
    }

    //Es 3.28, scrivi la funzione map che data f:A=>B trasforma Tree[A] in Tree[B]
    def map[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match{
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f),map(right)(f)) 
    }

    //Es 3.29, scrivi fold che generalizzi size, depth e map
    def fold[A,B](t: Tree[A])(baseCase: A=>B, g:(B,B)=>B):B =t match{
        case Leaf(x) => baseCase(x)
        case Branch(l,r) => g(fold(l)(baseCase,g),fold(r)(baseCase,g))
    }
    def size_withFold[A](t: Tree[A]): Int =
        fold(t)(x=>1, 1+_+_)
    def depth_withFold[A](t: Tree[A]): Int = 
        fold(t)(x=>0 ,(l,r)=>1+(l max r))
    def map_withFold[A,B](t: Tree[A])(f: A=>B): Tree[B] =
        //fold(t)(a=>Leaf(f(a)), (l,r)=>Branch(l,r))
        //siccome le funzioni argomenti di fold hanno signature: A=>B e (B,B)=>B allora il typechecker pensa che B sia di tipo Leaf.
        //B però è di tipo Tree che è supertipo di Leaf, quindi posso fare l'upcasting senza problema.
        fold(t)(a=>Leaf(f(a)): Tree[B], (l,r)=>Branch(l,r))
        //Altrimenti potevo dare una definizione estensionale specificando esplicitamente i tipi che fold avrebbe ricevuto così:
        //fold[A, Tree[B]](t)(a=>Leaf(f(a)), (l,r)=>Branch(l,r))
}
