package lab

import lab.Task1.*
import org.junit.Assert.assertEquals
import org.junit.Test
import u02.Modules.Person
import u02.Modules.Person.Teacher
import u03.Lists.*

class Task1Test:

  import List.*
  import u02.Modules.*
  import Person.*

  val list: List[Int] = Cons(1, Cons(2, Cons(3, Nil())))


  @Test def testDrop(): Unit =
    assertEquals(Cons(1, Cons(3, Nil())), drop(list, 1))
    assertEquals(Cons(2, Cons(3, Nil())), drop(list, 0))

  @Test def testDropNegative(): Unit =
    assertEquals(list, drop(list, -1))

  @Test def testDropNil(): Unit =
    assertEquals(Nil(), drop(Nil(), 0))

  @Test def testAppend(): Unit =
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), append(list, Cons(4, Nil())))

  @Test def testAppendNil(): Unit =
    assertEquals(Cons(1, Nil()), append(Nil(), Cons(1, Nil())))

  @Test def testNilAppendNil(): Unit =
    assertEquals(Nil(), append(Nil(), Nil()))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(2, Nil()), flatMap(Cons(1, Nil()))(v => Cons(v + 1, Nil())))

  @Test def testMap(): Unit =
    assertEquals(Cons(2, Nil()), map2(Cons(1, Nil()))(v => v + 1))

  @Test def testFilter(): Unit =
    assertEquals(Cons(2, Cons(3, Nil())), filter2(list)(_ > 1))
    assertEquals(Nil(), filter2(Cons(1, Nil()))(_ > 2))

  @Test def testMax(): Unit =
    import u02.Optionals.Option.*

    import Option.*
    assertEquals(Some(3), max(list))
    assertEquals(Some(2), max(Cons(2, Cons(1, Nil()))))


  @Test def testPersonCourses(): Unit =
    val p1: Person = Student("s1", 3);
    val p2: Person = Teacher("t1", "Math");
    val p3: Person = Student("s2", 3);
    val p4: Person = Teacher("t2", "English");
    val l: List[Person] = Cons(p1, Cons(p2, Cons(p3, Cons(p4, Nil()))))
    assertEquals(Cons("Math", Cons("English", Nil())), personCourses(l))

  @Test def testPersonNilCourses(): Unit =
    assertEquals(Nil(), personCourses(Cons(Student("a", 1), Nil())))


  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(0, foldLeft(Nil[Int]())(0)(_ + _))

  @Test def testFoldRight(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(0, foldRight(Nil[Int]())(0)(_ + _))

  @Test def testStreamDrop(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))),
      Stream.toList(Stream.drop(s)(6)))
    assertEquals(s, Stream.drop(s)(-1))

  @Test def testConstant(): Unit =
    val k = "k"
    val s = Stream.toList(Stream.take(Stream.constant(k))(5))
    assertEquals(Cons(k, Cons(k, Cons(k, Cons(k, Cons(k, Nil()))))), s)


  @Test def testFib(): Unit =
    val fibs: Stream[Int] = Stream.fib()
    val s = Stream.toList(Stream.take(fibs)(8))
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), s)



