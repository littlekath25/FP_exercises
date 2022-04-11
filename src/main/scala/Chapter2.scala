package Chapter2

object Chapter2 {

    // Exercise 2.1
    def fib(n: Int): Int =
        if (n == 1 || n == 0) n
        else fib(n - 1) + fib(n - 2)

    def main(arg: Array[String]): Unit = {
        println("Exercise 2.1: " + fib(12))
        
    }

    // Exercise 2.2
}
