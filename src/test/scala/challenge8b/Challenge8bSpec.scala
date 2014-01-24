package challenge8b

import core._, Syntax._
import challenge7._

object Challenge8bSpec extends test.Spec {
  import Laws._
  import StateT._
  import StateTArbitraries._
  import HttpArbitraries._

  "Http" should {
    "satisfy monad laws" ! monad.laws[Http]
  }

  implicit def HttpEqual[A: Equal]: Equal[Http[A]] =
    Equal.from[Http[A]]((a, b) => {
      val read = HttpRead(Get, "hello", Headers())
      val state = HttpState(Headers())
      val (wa, sa, ra) = a(read, state) //.run(read, state)
      val (wb, sb, rb) = b(read, state) //.run(read, state)
      wa == wb && sa == sb && ra == rb
    })

  "Echo service" should {
    val echo: Http[String] = HttpExample.echo
    "return body as string" ! prop((r: HttpRead, s: HttpState) =>
      echo(r, s)._3 must_== HttpValue.ok(r.body)
    )

    "add 'content-type' header of 'text/plain'" ! prop((r: HttpRead, s: HttpState) =>
      echo(r, s)._2.resheaders.headers must contain ("content-type" -> "text/plain")
    )

    "log a message with the length of the body in characters" ! prop((r: HttpRead, s: HttpState) =>
      echo(r, s)._1.log must contain (r.body.length.toString)
    )
  }
}
