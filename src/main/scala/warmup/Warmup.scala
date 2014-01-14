package warmup

/*
 * Warm-up exercises. Helpful to get more comfortable with scala before the heavy stuff.
 */
object Warmup {

  /*
   *
   * The following examples are all based upon the 'List'
   * data structure.
   *
   * In scala this data structure this looks like:
   *
   * {{{
   *   sealed trait List[+A]
   *   case object Nil extends List[Nothing]
   *   case class ::[A](h: A, t: List[A]) extends List[A]
   * }}}
   *
   * We call this a "sum-type", where "List" is a type
   * constructor that has two data constructors, "Nil",
   * and "::" (pronounced ~cons~). We can declare values
   * of type List using either the data constructors or
   * via the convenience function `List`.
   *
   * {{{
   *   val xs = "goodbye" :: "cruel" :: "world" :: Nil
   *   val ys = List("we", "have", "the", "technology")
   * }}}
   *
   * Lists can be worked with via pattern matching or via
   * the standard library methods foldRight & foldLeft
   * that have are defined as:
   *
   * {{{
   *   List[A]#foldRight[B](z: B)(f: (A, B) => B)
   *   List[A]#foldLeft[B](z: B)(f: (B, A) => A)
   * }}}
   *
   */


  /*
   * Example 0.1:
   *
   * Implement length using pattern matching.
   *
   * scala> import warmup.Warmup._
   * scala> length(List(1, 2, 3, 4))
   * resX: Int = 4
   */
  def length[A](xs: List[A]): Int =
    xs match {
      case Nil =>
        0
      case y :: ys =>
        1 + length(ys)
    }


  /*
   * Example 0.2:
   *
   * Implement length using foldRight.
   *
   * scala> import warmup.Warmup._
   * scala> lengthX(List(1, 2, 3, 4))
   * resX: Int = 4
   */
  def lengthX[A](xs: List[A]): Int =
    xs.foldRight(0)((_, acc) => acc + 1)

  /*
   * Exercise: 0.1:
   *
   * Append two lists to produce a new list.
   *
   * scala> import warmup.Warmup._
   * scala> append(List(1, 2, 3, 4), List(5, 6, 7, 8))
   * resX: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)
   */
  def append[A](x: List[A], y: List[A]): List[A] =
    x.foldRight(y)((h, acc) => h :: acc)
//    x match {
//      case Nil => y
//      case h :: t => h :: append(t, y)
//    }

  /*
   * Exercise: 0.2:
   *
   * Map the given function across each element of the list.
   *
   * scala> import warmup.Warmup._
   * scala> map(List(1, 2, 3, 4))(x => x + 1)
   * resX: List[Int] = List(2, 3, 4, 5)
   *
   * ~~~ Syntax hint: type annotations
   *
   *     (Nil : List[A]) // Nil _is of type_ List[A]
   *
   *     Type annotations are required when scala can
   *     not infer what you mean.
   */
  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List[B]())((h, acc) => f(h) :: acc)
//    xs match {
//      case Nil => Nil
//      case h :: t => f(h) :: map(t)(f)
//    }

  /*
   * Exercise: 0.3:
   *
   * Return elements satisfying the given predicate.
   *
   * scala> import warmup.Warmup._
   * scala> filter(List(1, 2, 3, 4))(i => i < 3)
   * resX: List[Int] = List(1, 2)
   */
  def filter[A](xs: List[A])(p: A => Boolean): List[A] =
    xs.foldRight(List[A]())((h, acc) => if (p(h)) h :: acc else acc)
//    xs match {
//      case Nil => Nil
//      case h :: t if p(h) => h :: filter(t)(p)
//      case h :: t => filter(t)(p)
//    }

  /*
   * Exercise: 0.4:
   *
   * Reverse a list to produce a new list.
   *
   * scala> import warmup.Warmup._
   * scala> reverse(List( 1, 2, 3, 4))
   * resX: List[Int] = List(4, 3, 2, 1)
   *
   * ~~~ Syntax hint: type annotations
   *
   *     (Nil : List[A]) // Nil _is of type_ List[A]
   *
   *     Type annotations are required when scala can
   *     not infer what you mean.
   */
  def reverse[A](xs: List[A]): List[A] = {
//    xs.foldLeft(List[A]())((acc, h) => h :: acc)
//    xs.foldRight(List[A]())((h, acc) => append(acc, h :: Nil))
    def loop(ys: List[A], acc: List[A]): List[A] =
      ys match {
        case Nil => acc
        case h :: t => loop(t, h :: acc)
      }

    loop(xs, Nil)
  }

  /*
   * *Challenge* Exercise: 0.5:
   *
   * Return a list of ranges.
   *
   * scala> import warmup.Warmup._
   * scala> ranges(List(1, 2, 3, 4, 7, 8, 9, 10, 30, 40, 41))
   * resX: List[(Int, Int)] = List((1, 4), (7, 10), (30, 30), (40, 41))
   *
   * ~~~ library hint: use can just use List[A]#sorted to sort the list before you start.
   * ~~~ library hint: List[A]#min and List#max exist.
   */
  def ranges(xs: List[Int]): List[(Int, Int)] = {
    def loop(ys: List[Int], acc: List[(Int, Int)]): List[(Int, Int)] =
      (ys, acc) match {
        case (Nil, _) => acc
        case (h :: t, Nil) => loop(t, List((h, h)))
        case (h :: t, ah :: at) if (h == ah._2 + 1) => loop (t, (ah._1, h) :: at)
        case (h :: t, ah :: at) => loop (t, (h, h) :: acc)
      }

    loop(xs.distinct.sorted, Nil).reverse
  }
}
