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
    def curry[A, B, C](f: (A, B) => C) : A => (B => C) =
        a => b => f(a, b)

    def addFunction(x: Int, y: Int) : Int =
        x + y

    def curriedAdd = curry(addFunction)

    println("Exercise 2.3: " + curriedAdd(2)(3))
    println("Exercise 2.3: " + curriedAdd(20)(12) + "\n")

    // Exercise 2.4
    def uncurry[A, B, C](f: A => B => C) : (A, B) => C =
        (a, b) => f(a)(b)

    def uncurriedAdd = uncurry(curriedAdd)

    println("Exercise 2.4: " + uncurriedAdd(2, 3))
    println("Exercise 2.4: " + uncurriedAdd(20, 12) + "\n")

    // Exercise 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C =
        a => f(g(a))

    def addOne(x: Int) : Int =
        x + 1
    
    def timesTwo(y: Int) : Int =
        y * 2

    def newCompose = compose(addOne, timesTwo)

    println("Exercise 2.5: " + newCompose(1))
    println("Exercise 2.5: " + newCompose(20))
}
