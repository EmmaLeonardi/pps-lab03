package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03 Task 1 - svolto da sola

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => skip(t)(n - 1)
      case Cons(_, _) => s
      case Nil() => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _) | (_, Nil()) => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Nil() => s2
      case Cons(h1, t1) => Cons(h1,concat(t1,s2))

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _reverse(seq: Sequence[A], reversed: Sequence[A]): Sequence[A] = seq match
        case Nil() => reversed
        case Cons(h, t) => _reverse(t, Cons(h, reversed))
      s match
        case Nil()=>Nil()
        case Cons(h, t)=>_reverse(t,Cons(h,Nil()))


    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t) (mapper))

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      @tailrec
      def _min(seq: Sequence[Int], currentMin: Int): Optional[Int] = seq match
        case Nil() => Just(currentMin)
        case Cons(h, t) if h<currentMin =>_min(t, h)
        case Cons(_, t) =>_min(t, currentMin)
      s match
        case Nil() => Empty()
        case Cons(h, t) => _min(t, h)

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      def _evenIndices[B](seq: Sequence[B], isIndexEven: Boolean): Sequence[B] = seq match
        case Nil() => Nil()
        case Cons(h, t) if isIndexEven => Cons(h, _evenIndices(t, !isIndexEven))
        case Cons(_, t) => _evenIndices(t, !isIndexEven)

      s match
        case Nil() => Nil()
        case Cons(h, t) => Cons(h, _evenIndices(t, false))

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Nil()=>false
      case Cons(h, t) if h==elem =>true
      case Cons(_, t) => contains(t) (elem)

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = ???

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      s match
        case Nil() => (Nil(), Nil())
        case Cons(h, t) if pred(h) => (Cons(h, filter(t)(pred)), filter(t)(!pred(_)))
        case Cons(h, t) => (filter(t)(pred), Cons(h, filter(t)(!pred(_))))

  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52
