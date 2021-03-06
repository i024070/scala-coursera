package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    new TestSets {
      assert(contains(x => true, 100))
      assert(contains(s123, 3))
    }
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s12 = union(s1, s2)
    val s123 = union(s12,s3)
    val s23 = union(s2, s3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(s12, 1), "Union 1")
      assert(contains(s12, 2), "Union 2")
      assert(!contains(s12, 3), "Union 3")
    }
  }

  test("intersect contains common elements of each set") {
    new TestSets {
      assert(contains(intersect(s12, s1), 1), "Intersect 1")
      assert(!contains(intersect(s12, s1), 2), "Intersect 2")
      assert(contains(intersect(s12, s2), 2), "Intersect 3")
      assert(!contains(intersect(s12, s3), 2), "Intersect 4")
      assert(!contains(intersect(s12, s3), 3), "Intersect 5")
    }
  }

  test("diff contains different elements of set1 than set2") {
    new TestSets {
      assert(!contains(diff(s12, s1), 1), "Diff 1")
      assert(contains(diff(s12, s1), 2), "Diff 2")
      assert(!contains(diff(s12, s2), 3), "Diff 3")
      assert(contains(diff(s123, s23), 1), "Diff 4")
      assert(!contains(diff(s123, s23), 3), "Diff 5")
    }
  }

  test("filtered elements of set regarding a predicate") {
    new TestSets {
      assert(!contains(filter(s12, (_) => false), 1), "Filter 1")
      assert(!contains(filter(s12, (_) => true), 3), "Filter 2")
      assert(contains(filter(s123, (x) => x>1), 3) , "Filter 3")
      assert(!contains(filter(s123, (x) => x>1), 1) , "Filter 3")
      assert(!contains(filter(s123, (x) => x==2), 3), "Filter 5")
      assert(contains(filter(s123, (x) => x==2), 2), "Filter 6")
    }
  }

  test("forall elements of each set rule is valid") {
    new TestSets {
      assert(!forall(s12, (_) => false), "Forall 1")
      assert(forall(s123, (_) => true), "Forall 2")
      assert(!forall(s123, (x) => x>1), "Forall 3")
      assert(forall(s23, (x) => x>1), "Forall 3")
      assert(!forall(s123, (x) => x==2), "Forall 5")
      assert(forall(intersect(s12, s23), (x) => x==2), "Forall 6")
      assert(!forall(s123, (x) => x>1), "Forall 7")
    }
  }

  test("exists elements of set satisfying a rule") {
    new TestSets {
      assert(!exists(s12, (_) => false), "Exists 1")
      assert(exists(s123, (_) => true), "Exists 2")
      assert(exists(s123, (x) => x>1), "Exists 3")
      assert(exists(s123, (x) => x  == 1), "Exists 1 in 123")
      assert(exists(s123, (x) => x  == 3), "Exists 3 in 123")
      assert(!exists(s23, (x) => x < 2), "Exists 4")
      assert(!exists(s123, (x) => x==4), "Exists 5")
      assert(exists(intersect(s12, s23), (x) => x==2), "Exists 6")
      assert(exists(s123, (x) => x*x == 1), "Exists 7")
      assert(exists(s123, (x) => x*x == 4), "Exists 8")
      assert(!exists(s123, (x) => x*x == 2), "Exists 9")
    }
  }

  test("map elements of each set") {
    new TestSets {
      assert(contains(map(s123, (x) => x*x), 1), "Map 1")
      assert(contains(map(s123, (x) => x*x), 9), "Map 2")
      assert(!contains(map(s123, (x) => x*x), 2), "Map 3")
      assert(!contains(map(s123, (x) => x*x), -501), "Map 4")
    }
  }

}
