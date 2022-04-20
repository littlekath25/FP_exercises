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
    

    // Exercise 3.5
    def dropWhile[A](list: List[A], f: A => Boolean) : List[A] =
        list match
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => list

    println("Exercise 3.5: " + dropWhile(exampleList, x => x < 3))
    println("Exercise 3.5: " + dropWhile(exampleList, x => x < 3) + "\n")

    // Exercise 3.6
    def init[A](list: List[A]) : List[A] =
            list match
                case Cons(h, t) if (t != Nil) => Cons(h, init(t))
                case _ => Nil

    println("Exercise 3.6: " + init(exampleList))
    println("Exercise 3.6: " + init(exampleList) + "\n")

    // Exercise 3.7
    // No, because it have to go back and multiply the previous values.

    // Exercise 3.8
    // I think it will construct a new list of ints. The relationship between foldRight and the list constructor is that they both
    // start building from the last element to the first.

    // Exercise 3.9
    def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B) : B =
        list match
            case Nil => z
            case Cons(h, t) => f(h, foldRight(t, z)(f))

    def length[A](list: List[A]) : Int =
        foldRight(list, 0)((_, y) => y + 1)
    
    println("Exercise 3.9: " + length(exampleList))
    println("Exercise 3.9: " + length(exampleList2) + "\n")

    // Exercise 3.10
    def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B) : B =
        list match
            case Nil => z
            case Cons(h, t) => foldLeft(t, f(z, h))(f)

    def lengthLeftFold[A](list: List[A]) : Int =
        foldLeft(list, 0)((y, _) => y + 1)

    println("Exercise 3.10: " + lengthLeftFold(exampleList))
    println("Exercise 3.10: " + lengthLeftFold(exampleList2) + "\n")
}