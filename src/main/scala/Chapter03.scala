package Chapter03

object Chapter03P1 extends App {
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

    //Exercise 3.1
    val exercise = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
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
    val exampleList3 = List(2, 2, 2, 2, 2)

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

    // Exercise 3.11
    def lengthLeftFold[A](list: List[A]) : Int =
        foldLeft(list, 0)((y, _) => y + 1)

    def sumLeftFold(ints: List[Int]): Int = 
        foldLeft(ints, 0)(_ + _)

    def productLeftFold(ds: List[Double]): Double =
        foldLeft(ds, 1.0)(_ * _)

    println("Exercise 3.11: " + lengthLeftFold(exampleList))
    println("Exercise 3.11: " + lengthLeftFold(exampleList2) + "\n")

    println("Exercise 3.11: " + sumLeftFold(exampleList))
    println("Exercise 3.11: " + sumLeftFold(exampleList3) + "\n")

    val doubleList = List(1.0, 2.0, 3.0, 4.0)
    val doubleList2 = List(0.0, 0.0, 0.0, 0.0)

    println("Exercise 3.11: " + productLeftFold(doubleList2))
    println("Exercise 3.11: " + productLeftFold(doubleList) + "\n")

    // Exercise 3.12
    def reverse[A](list: List[A]) : List[A] =
        foldLeft(list, Nil: List[A])((acc, elem) => Cons(elem, acc))

    println("Exercise 3.12: " + reverse(exampleList))
    println("Exercise 3.12: " + reverse(exampleList3) + "\n")

    // Exercise 3.13
    // TODO

    // Exercise 3.14
    def append[A](a1: List[A], a2: List[A]) : List[A] =
        foldRight(a1, a2)((acc, elem) => Cons(acc, elem))
    
    println("Exercise 3.14: " + append(exampleList, List(9, 10)))
    println("Exercise 3.14: " + append(exampleList, List(9, 10)) + "\n")

    // Exercise 3.15
    def concatenates[A](list: List[List[A]]) : List[A] =
        foldRight(list, Nil: List[A])((acc, elem) => append(acc, elem))

    val listsOfLists = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

    println("Exercise 3.15: " + concatenates(listsOfLists) + "\n")

    // Exercise 3.16
    def addOne(list: List[Int]) : List[Int] =
        foldRight(list, Nil: List[Int])((elem, acc) => Cons(elem + 1, acc))

    println("Exercise 3.16: " + addOne(exampleList) + "\n")

    // Exercise 3.17
    def doubleToString(list: List[Double]) : List[String] =
        foldRight(list, Nil: List[String])((elem, acc) => Cons(elem.toString, acc))

    println("Exercise 3.17: " + doubleToString(doubleList) + "\n")

    // Exercise 3.18
    def map[A, B](list: List[A])(f: A => B) : List[B] =
        foldRight(list, Nil: List[B])((elem, acc) => Cons(f(elem), acc))

    println("Exercise 3.18: " + map(exampleList)(x => x + 1) + "\n")

    // Exercise 3.19
    def filter[A](list: List[A])(f: A => Boolean): List[A] =
        foldRight(list, Nil: List[A])((elem, acc) => if f(elem) then Cons(elem, acc) else acc)

    // def filterViaPattern[A](list: List[A])(f: A => Boolean): List[A] =
    //     foldRight(list, Nil: List[A])((elem, acc) => if f(elem) then Cons(elem, acc) else acc)

    println("Exercise 3.19: " + filter(exampleList)(x => x % 2 == 1))
    println("Exercise 3.19: " + filter(exampleList)(x => x % 2 == 0) + "\n")

    // Exercise 3.20
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
        concatenates(map(list)(f))

    def flatMapViaFold[A, B](list: List[A])(f: A => List[B]): List[B] =
        foldRight(list, Nil: List[B])((elem, acc) => append(f(elem), acc))

    println("Exercise 3.20: " + flatMap(exampleList)(i => List(i, i)))
    println("Exercise 3.20: " + flatMap(exampleList)(i => List(i, i)) + "\n")

    println("Exercise 3.20: " + flatMapViaFold(exampleList)(i => List(i, i)))
    println("Exercise 3.20: " + flatMapViaFold(exampleList)(i => List(i, i)) + "\n")

    // Exercise 3.21
    def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] =
        flatMapViaFold(list)(x => if f(x) then List(x) else Nil)

    println("Exercise 3.21: " + filterViaFlatMap(exampleList)(x => x % 2 == 1))
    println("Exercise 3.21: " + filterViaFlatMap(exampleList)(x => x % 2 == 0) + "\n")

    // Exercise 3.22
    def sumLists(l1: List[Int], l2: List[Int]) : List[Int] =
        (l1, l2) match
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(l1, t1), Cons(l2, t2)) => Cons(l1 + l2, sumLists(t1, t2))

    println("Exercise 3.22: " + sumLists(List(1, 2, 3), List(4, 5, 6)))
    println("Exercise 3.22: " + sumLists(List(1, 1, 1), List(1, 1, 1)) + "\n")

    // Exercise 3.23
    def zipWith[A, B, C](l1: List[A], l2: List[B], f: (A, B) => C) : List[C] =
        (l1, l2) match
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(l1, t1), Cons(l2, t2)) => Cons(f(l1, l2), zipWith(t1, t2, f))
    
    println("Exercise 3.23: " + zipWith(List(6, 5, 4), List(3, 2, 1), (x, y) => x - y))
    println("Exercise 3.23: " + zipWith(List(6, 2, 4), List(4, 5, 6), (x, y) => x * y) + "\n")

    // Exercise 3.24
    val myListInts = SList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val myListIntsShort = SList(1, 2, 3, 4, 5)
    val myListChars = SList('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')

    /* 
    ScanLeft:
        0 + 1             =    1
        1 + 2             =    3
        1 + 2 + 3         =    6
        1 + 2 + 3 + 4     =   10
        1 + 2 + 3 + 4 + 5 =   15

    ScanRight:
        5 + 4 + 3 + 2 + 1 =   15
        5 + 4 + 3 + 2     =   14
        5 + 4 + 3         =   12
        5 + 4             =    9
        5 + 0             =    5
        0                 =    0
    */

    println("Exercise 3.24: " + myListInts.take(5))
    println("Exercise 3.24: " + myListInts.takeWhile(x => x <= 5) + "\n")
    println("Exercise 3.24: " + myListIntsShort.scanLeft(0)(_ + _))
    println("Exercise 3.24: " + myListIntsShort.scanRight(0)(_ + _) + "\n")
    println("Exercise 3.24: " + myListChars.forall(x => x < 'Z'))
    println("Exercise 3.24: " + myListChars.exists(x => x == 'J') + "\n")
}

object Chapter03P2 extends App {
    // Exercise 3.24
    def foundSub[A](sup: List[A], sub: List[A]) : Boolean =
        (sup, sub) match
            case (_, Nil) => true
            case (x :: xt, y :: yt) => 
                if (x == y) then foundSub(xt, yt) 
                else false
            case (_, _) => false

    def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean =
        sup match
            case (Nil) => false
            case (h1 :: t1) =>
                sub match
                    case (Nil) => false
                    case (h2 :: t2) => 
                        if (h1 == h2) then
                            if ((foundSub(sup, sub)) == true) then true 
                            else hasSubsequence(t1, sub)
                        else hasSubsequence(t1, sub)

    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(1)))
    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2)))
    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)))
    println("Exercise 3.24: " + hasSubsequence(List(2, 2, 3, 4, 5), List(2, 3)))
    println("Exercise 3.24: " + hasSubsequence(List(2, 2, 3, 4, 5), List(2, 3, 4, 5)))
    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(4)) + "\n")

    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(0)))
    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(2, 2)))
    println("Exercise 3.24: " + hasSubsequence(List(1, 2, 3, 4, 5), List(3, 2)) + "\n")

    // Exercise 3.25
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    def treeSize[A](tree: Tree[A]) : Int =
        tree match
            case Leaf(_) => 1
            case Branch(left, right) => 1 + treeSize(left) + treeSize(right)

    val myTree1 = Leaf(1)
    val myTree5 = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val myTree9 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))

    println("Exercise 3.25: " + treeSize(myTree1))
    println("Exercise 3.25: " + treeSize(myTree5))
    println("Exercise 3.25: " + treeSize(myTree9) + "\n")

    // Exercise 3.26
    def maxTree(tree: Tree[Int]) : Int =
        tree match
            case Leaf(x) => x
            case Branch(left, right) => maxTree(left).max(maxTree(right))

    println("Exercise 3.26: " + maxTree(myTree1))
    println("Exercise 3.26: " + maxTree(myTree5))
    println("Exercise 3.26: " + maxTree(myTree9) + "\n")

    // Exercise 3.27
    def depthTree[A](tree: Tree[A]) : Int =
        tree match
            case Leaf(_) => 0
            case Branch(left, right) => 1 + depthTree(left).max(depthTree(right))

    println("Exercise 3.27: " + depthTree(myTree1))
    println("Exercise 3.27: " + depthTree(myTree5))
    println("Exercise 3.27: " + depthTree(myTree9) + "\n")

    // Exercise 3.28
    def mapTree[A, B](f: A => B, tree: Tree[A]) : Tree[B] =
        tree match
            case Leaf(x) => Leaf(f(x))
            case Branch(left, right) => Branch(mapTree(f, left), mapTree(f, right))

    println("Exercise 3.28: " + mapTree(x => x.toString, myTree1) + "\n")

    // Exercise 3.29
    def fold[A, B](f: A => B, g: (B,B) => B): B =
        ???
}