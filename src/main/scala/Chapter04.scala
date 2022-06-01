package Chapter04

object Chapter04 extends App {
    case class User(name: String, age: Int)

    val userBase = List(
        User("Travis", 28),
        User("Kelly", 33),
        User("Jennifer", 44),
        User("Dennis", 23))

    val twentySomethingsFor =
        for (user <- userBase if user.age >=20 && user.age < 30)
        yield user.name

    val twentySomethingsMap = userBase.flatMap(user => if user.age >=20 && user.age < 30 then List(user.name) else None)

    println(twentySomethingsMap)
    println(twentySomethingsFor)

    def fooFor(n: Int, v: Int) =
        for (i <- 0 until n;
             j <- 0 until n if i + j == v)
        yield (i, j)

    def fooMap(n: Int, v: Int) =
        (1 to n).flatMap (x => (0 to n).flatMap(y => if (x + y == v) then println(s"($x, $y)") else Unit)

    fooFor(5, 5) foreach {
        case (i, j) =>
            println(s"($i, $j)")
    }
    println("")
    fooMap(5, 5) foreach {
        case (i, j) =>
            println(s"($i, $j)")
    }
}