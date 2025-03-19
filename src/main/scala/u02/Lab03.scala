package u02

import u03.Sequences.*
import Sequence.*
import u02.Modules.Person
import u02.Modules.Person.{Student, Teacher}

import scala.annotation.tailrec

object Lab03 extends App {

  def allTeacherCourses(s: Sequence[Person]): Sequence[String] = flatMap(s)(_ match
    case Teacher(_, c) => Cons(c, Nil())
    case Student(_, _) => Nil())

  @tailrec
  def foldLeft[A, B](l: Sequence[A])(defaultValue: B)(f: (B, A) => B): B = l match
    case Nil() => defaultValue
    case Cons(h, t) => foldLeft(t)(f(defaultValue, h))(f)

  def countAllTeacherCourses(s: Sequence[Person]): Int = s match
    case Nil() => 0
    case Cons(h, t) => h match
      case Teacher(_, _) => 1 + countAllTeacherCourses(t)
      case Student(_, _) => countAllTeacherCourses(t)

}





