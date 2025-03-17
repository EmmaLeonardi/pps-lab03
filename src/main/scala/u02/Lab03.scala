package u02

import u03.Sequences.*
import Sequence.*
import u02.Modules.Person
import u02.Modules.Person.{Student, Teacher, name}

object Lab03 extends App{

  def allTeacherCourses(s: Sequence[Person]): Sequence[String] = flatMap(s) (_ match
    case Teacher(_,c)=>Cons(c,Nil())
    case Student(_, _) => Nil())

  println(allTeacherCourses(Cons(Teacher("mario", "OOP"), Cons(Teacher("mario", "OOP2"), Cons(Student("pappero", 2025),Cons(Teacher("mario", "OOP3"),Nil()))))))

}


