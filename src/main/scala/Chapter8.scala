package Chapter8

/* Exercise 8.1
- Reverse and summing the same as summing the original
- Sorting and summing the same as summing the original
- Sum list with same value = value * length of list
- All zeros in list
- Empty list
*/

/* Exercise 8.2
- Empty list = 0
- 1 element = max is the 1 element
- Sorted must be the last one
- Sorted and reverse should be first one
*/

object Chapter8 extends App {

    object Prop:
        type FailedCase = String
        type SuccesCount = Int

    trait Prop:
        def check: Either[(FailedCase, SuccesCount), SuccesCount]
        def &&(p: Prop): Prop = new Prop { def check = this->check && p.check }

    // Exercise 8.2
    // def listOf[A](a: Gen[A]) : Gen[List[A]] =
    //     ???

    // def listOfN[A](n: Int, a: Gen[A]) : Gen[List[A]] =
    //     ???

    // def forAll[A](a: Gen[A])(f: A => Boolean) : Prop =
    //     ???
}