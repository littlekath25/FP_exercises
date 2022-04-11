package Chapter2

object Chapter2 extends App {

    // Exercise 2.1
    def fib(n: Int): Int =
        if (n == 1 || n == 0) n
        else fib(n - 1) + fib(n - 2)

    println("Exercise 2.1: " + fib(12))   
    println("Exercise 2.1: " + fib(23) + "\n")
    
    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = 
        def loop(n: Int): Boolean =
            if (n >= as.length) true
            else if (ordered(as(n - 1), as(n)) == false) false
            else loop(n + 1)
        
        loop(1)

    println("Exercise 2.2: " + isSorted(Array(1, 2, 3, 4), (x, y) => x < y))
    println("Exercise 2.2: " + isSorted(Array(1, 2, 4, 3), (x, y) => x < y) + "\n")

    println("Exercise 2.2: " + isSorted(Array('A', 'B', 'C', 'D'), (x, y) => x < y))
    println("Exercise 2.2: " + isSorted(Array('A', 'C', 'B', 'D'), (x, y) => x < y) + "\n")

    // Exercise 2.3
    
}
