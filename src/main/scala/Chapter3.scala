package Chapter3
import scala.{List => SList}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

def sum(ints: List[Int]): Int = 
    ints match
        case Nil => 0
        case Cons(h, t) => h + sum(t)

def product(ds: List[Double]): Double =
    ds match
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)

object Chapter3 extends App {

    //Exercise 3.1
    val exercise = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        // case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    println("Exercise 3.1: " + exercise + "\n")

    // Exercise 3.2
    def tail[A](list: List[A]) : List[A] =
        list match
            case Nil => Nil
            case Cons(h, t) => t

    val exampleList = List(1, 2, 3, 4)
    val exampleList2 = Nil
    println("Exercise 3.2: " + tail(exampleList))
    println("Exercise 3.2: " + tail(exampleList2) + "\n")

    // Exercise 3.3
    def setHead[A](list: List[A], element: A) : List[A] =
        list match
            case Nil => Nil
            case Cons(h, t) => Cons(element, t)

    println("Exercise 3.3: " + setHead(exampleList, 2))
    println("Exercise 3.3: " + setHead(exampleList2, 5) + "\n")

    // Exercise 3.4
    def drop[A](list: List[A], total: Int) : List[A] =
        total match
            case 0 => list
            case _ => drop(tail(list), total - 1)

    println("Exercise 3.4: " + drop(exampleList, 2))
    println("Exercise 3.4: " + drop(exampleList, 1) + "\n")
}