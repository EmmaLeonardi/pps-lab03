package u02

import org.junit.*
import org.junit.Assert.*
import u02.Lab03.*
import u02.Modules.Person.{Student, Teacher}
import u03.Sequences.Sequence.{Cons, Nil}

class Lab03Test:

  val course1 = "OOP"
  val course2 = "LCMC"
  val teacher1 = Teacher("Mario", course1)
  val teacher2 = Teacher("Antonio", course2)
  val student = Student("Luigi", 2025)
  val list = Cons(teacher1, Cons(student, Cons(teacher2, Nil())))

  //Task 2 - svolto da sola
  @Test def testAllTeacherCoursesEmptyList() =
    assertEquals(allTeacherCourses(Nil()), Nil())

  @Test def testAllTeacherCoursesWithStudents() =
    assertEquals(allTeacherCourses(list),
      Cons(course1, Cons(course2, Nil())))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(foldLeft(lst)(0)(_ - _), -16)
    assertEquals(foldLeft(Cons(0, Nil()))(0)(_ - _), 0)

  @Test def testCountTeacherCoursesEmptyList() =
    assertEquals(countAllTeacherCourses(Nil()), 0)

  @Test def testCountTeacherCourseWithStudents() =
    assertEquals(countAllTeacherCourses(list), 2)

end Lab03Test
