package Chapter8

import ownGen.*

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
        import Prop.*

        // def check: Either[(FailedCase, SuccesCount), SuccesCount]
        def check: Boolean
            def &&(p: Prop): Prop =
                new Prop:
                    def check = check && p.check

    case class Gen[A](sample: State[RNG, A]):
        def choose(start: Int, stopExclusive: Int) : Gen[Int] = ???
}