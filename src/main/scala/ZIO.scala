package ZIO

import zio._
import java.io.IOException
import zio.Console

object ZIO extends ZIOAppDefault {
    val myApp: ZIO[Has[Console], IOException, Unit] =
        Console.printLine("Hello, World!")

    def run = myApp
}