package u03
import org.junit.*
import org.junit.Assert.*

class Lab03Test:
  
  //Test Sequence
  import Sequences.*
  import Sequence.*
  import u03.Optionals.Optional
  
  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testSequenceMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testSequenceFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zip(l2))
    assertEquals(Nil(), l.zip(Nil()))
    assertEquals(Nil(), Nil().zip(l2))
    assertEquals(Nil(), Nil().zip(Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concat(l2))
    assertEquals(Cons(40, Cons(50, Nil())), Nil().concat(l2))
  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Optional.Just(10), min(l))
    assertEquals(Optional.Just(1), min(Cons(1, Nil())))
    assertEquals(Optional.Empty(), min(Nil()))
  
  @Test def foldLeft() = 
    val list = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val expectedResult = -16
    assertEquals(expectedResult, list.foldLeft(0)(_ - _))
  
  //Test Stream
  import u03.Stream.toList
  import Stream.*
  import Stream.take
  
  @Test def testIterate(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))
  
  @Test def fill(): Unit = 
    val expectedResult: Sequence[String] = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(expectedResult, toList(Stream.fill(3)("a")))

  @Test def pellStream(): Unit =
    val expectedPellStream = Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil())))))
    assertEquals(expectedPellStream, Stream.toList(take(Stream.pellNumber())(5)))
  
  //Test Student-Teacher
  val sequence = Cons(
    Person.Teacher("Mirko", "PPS"),
    Cons(Person.Student("Alex", 2000), Nil())
  )

  val emptyTeacherSequence = Cons(
    Person.Student("Alex", 2000),
    Cons(Person.Student("Ale", 3000), Nil())
  )

  @Test def correctTakeCourseFromTeacher() =
    val expectedResult = Cons("PPS", Nil())
    assertEquals(expectedResult, sequence.takeCourseFromTeacher)

  @Test def sequenceWithoutTeacher() = 
    val expectedResult = Nil()
    assertEquals(expectedResult, emptyTeacherSequence.takeCourseFromTeacher)
