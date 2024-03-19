package u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence.*
import u03.Person.*

class TestStudentTeacher:

  val sequence = Cons(
    Teacher("Mirko", "PPS"),
    Cons(Student("Alex", 2000), Nil())
  )

  val emptyTeacherSequence = Cons(
    Student("Alex", 2000),
    Cons(Student("Ale", 3000), Nil())
  )

  @Test def correctTakeCourseFromTeacher() =
    val expectedResult = Cons("PPS", Nil())
    assertEquals(expectedResult, sequence.takeCourseFromTeacher)

  @Test def sequenceWithoutTeacher() = 
    val expectedResult = Nil()
    assertEquals(expectedResult, emptyTeacherSequence.takeCourseFromTeacher)
