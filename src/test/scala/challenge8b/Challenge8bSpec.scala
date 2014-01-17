package challenge8b

import core._, Syntax._
import challenge7._

object Challenge8bSpec extends test.Spec {
  import Laws._
  import StateT._
  import StateTArbitraries._

  "Http" should {
    "satisfy monad laws" ! monad.laws[Http]
  }

  implicit def HttpEqual[A: Equal]: Equal[Http[A]] =
    Equal.from[Http[A]]((a, b) => {
      val read = HttpRead(Get, "hello", Headers())
      val state = HttpState(Headers())
      val (wa, sa, ra) = a.run(read, state)
      val (wb, sb, rb) = b.run(read, state)
      wa == wb && sa == sb && ra == rb
    })

}
