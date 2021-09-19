import homegrown.collections._

import org.scalatest._

class SetSuite extends FunSuite with Matchers {

  test("empty set 2") {
    1 to 1000 foreach { _ =>
      Set.empty(randomString) shouldBe false
    }

  }

  test("add on an empty Set should yield a new Set with one element 2") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on a non empty Set should yield a new Set with two elements 2") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first).add(second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove on a non empty Set should yield a new Set without the element 2.1") {
    val element = randomString
    val setWithElement = Set.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }

  test("remove removes only the element in question 2.1") {
    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(second)

    setWithoutElement(second) shouldBe false
    setWithoutElement(first) shouldBe true

  }

  test("remove on an empty Set should yield an empty Set 2.1") {
    val element = randomString
    val stillEmpty = OldSet.empty.remove(element)
    stillEmpty(element) shouldBe false
  }

  test("remove on a non empty Set should yield a new Set without the element2") {
    val element = randomString
    val setWithElement = Set.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }

  test("remove removes only the element in question 2.2") {
    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(first)

    setWithoutElement(first) shouldBe false
    setWithoutElement(second) shouldBe true
  }

  test("remove removes only the element in question (ordering test) 2.2") {
    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(second)

    setWithoutElement(first) shouldBe true
    setWithoutElement(second) shouldBe false
  }

  test("add/remove combo should ensure that all elements are distinct 2.3") {
    val a = randomString
    val b = randomString
    val c = randomString

    val set = Set.empty.add(a).add(b).add(c).add(a).remove(a)

    set(a) shouldBe false
  }

  test("union on empty Set should yield an empty Set 2.2") {
    Set.empty.union(Set.empty) shouldBe Set.empty
  }

  test(
    "union on a non empty Set with an empty set should yield the original Set untouched 2.2"
  ) {
      val first = randomString
      val second = randomString

      first should not be second

      val emptySet = Set.empty
      val nonEmptySet = Set.empty.add(first).add(second)

      emptySet.union(nonEmptySet)(first) shouldBe true
      emptySet.union(nonEmptySet)(second) shouldBe true

      nonEmptySet.union(emptySet)(first) shouldBe true
      nonEmptySet.union(emptySet)(second) shouldBe true
    }

  test("union on two non empty Sets should yield their union 2.2") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b)
    val right = Set.empty.add(c).add(d)

    val union = left.union(right)

    union(a) shouldBe true
    union(b) shouldBe true
    union(c) shouldBe true
    union(d) shouldBe true

  }

  test("union on a non empty Set with an empty set should yield the original Set untouched 2.3") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b)
    val right = Set.empty.add(c).add(d)

    // works because of the implementations of equals
    left.union(right) shouldBe Set.empty.add(a).add(b).add(c).add(d)
    right.union(left) shouldBe Set.empty.add(a).add(b).add(c).add(d)

  }

  test("intersction on empty Set should yield empty Set 2.2") {

    Set.empty.intersection(Set.empty)(randomString) shouldBe false

  }

  test("intersection on two non empty Sets should yield their intersection 2.2") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(c).add(d)

    val intersection = left.intersection(right)

    intersection(a) shouldBe false
    intersection(b) shouldBe false
    intersection(c) shouldBe true
    intersection(d) shouldBe false

  }

  test("difference on empty Set should yield empty Set 2.2") {

    Set.empty.difference(Set.empty)(randomString) shouldBe false

  }

  test("difference on a non empty Sets with empty should yield an empty Set 2.2") {

    val a = randomString
    val b = randomString

    val nonEmptySet = Set.empty.add(a).add(b)
    val emptySet = Set.empty

    emptySet.difference(nonEmptySet)(a) shouldBe false
    emptySet.difference(nonEmptySet)(b) shouldBe false

    nonEmptySet.difference(emptySet)(a) shouldBe true
    nonEmptySet.difference(emptySet)(b) shouldBe true

  }

  test("difference on two non empty Sets should yield their intersection 2.2") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b).add(c).add(d)
    val right = Set.empty.add(c).add(d)

    val difference = left.difference(right)

    difference(a) shouldBe true
    difference(b) shouldBe true
    difference(c) shouldBe false
    difference(d) shouldBe false

  }

  test("isSubSetOf on an empty Set should yield true") {

    Set.empty.isSubSetOf(Set.empty) shouldBe true
    Set.empty.isSubSetOf(Set.empty.add(randomString)) shouldBe true

  }

  test("isSubSetOf of it self should yield true 2.2") {

    val set = Set.empty.add(randomString)
    set.isSubSetOf(set) shouldBe true

  }

  test("isSubSetOf of non empty set should yield true 2.2") {

    val a = randomString
    val b = randomString
    val c = randomString

    val left = Set.empty.add(a).add(b)
    val right = left.add(c)

    left.isSubSetOf(right) shouldBe true
    right.isSubSetOf(left) shouldBe false

  }

  test("isSupersetOf on an empty Set should yield true") {
    Set.empty.isSupersetOf(Set.empty) shouldBe true
    Set.empty.add(randomString).isSupersetOf(Set.empty) shouldBe true
  }

  test("isSupersetOf on itself should yield true") {
    val set = Set.empty.add(randomString)

    set.isSupersetOf(set) shouldBe true
  }

  test("isSupersetOf on a non empty Set should yield false") {
    val a = randomString
    val b = randomString
    val c = randomString

    val left = Set.empty.add(a).add(b)
    val right = left.add(c)

    left.isSupersetOf(right) shouldBe false
    right.isSupersetOf(left) shouldBe true
  }

  test("hashCode on an empty Set should not be random") {
    Set.empty.hashCode shouldBe Set.empty.hashCode

    val element = randomString

    Set.empty.add(element).hashCode shouldBe Set.empty.add(element).hashCode
  }

  test("hashCode on an empty Set should not be 0") {
    Set.empty.hashCode should not be 0
  }

  test("hashCode on a non empty Set should be the sum of all the hashCodes and the hashCode of the empty Set") {
    val first = randomString
    val second = randomString

    val expected = Set.empty.hashCode + first.hashCode + second.hashCode

    Set.empty.add(first).add(second).hashCode shouldBe expected
  }

  test("foreach on an empty Set should not apply the function") {
    noException should be thrownBy Set.empty.foreach(_ => sys.error("should not be thrown"))
  }

  test("foreach on a non empty Set should apply the function") {
    var functionWasApplied = false

    Set.empty.add(randomString).foreach(_ => functionWasApplied = true)

    functionWasApplied shouldBe true
  }

  test("foreach should be able to calculate the size of the given set 0") {
    var size = 0

    val set = Set.empty

    set.foreach(_ => size += 1)

    size shouldBe 0
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of the given set 1") {
    var size = 0

    val set = Set.empty.add(randomString)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of the given set 2") {
    var size = 0

    val set = Set.empty.add(randomString).add(randomString)

    set.foreach(_ => size += 1)

    size shouldBe 2
    size shouldBe set.size
  }

  test("foreach should be able to calculate the size of the given set 3") {
    var size = 0

    val element = randomString

    val set = Set.empty.add(element).add(element)

    set.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe set.size
  }

  test("Calling the varargs apply method on the Set companion object should yield a Set with all the arguments as elements") {
    val a = randomString
    val b = randomString
    val c = randomString

    Set(a, b, c) shouldBe Set.empty.add(a).add(b).add(c)
  }

  private def randomString: String =
    scala.util.Random.alphanumeric.take(5).mkString

}
