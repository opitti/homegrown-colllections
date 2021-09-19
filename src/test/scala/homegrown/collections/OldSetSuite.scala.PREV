import homegrown.collections._

import org.scalatest._

class OldSetSuite extends FunSuite with Matchers {

  test("**OLD empty set ") {
    1 to 1000 foreach { _ =>
      OldSet.empty(randomString) shouldBe false
    }

  }

  test("**OLD add on an empty Set should yield a new Set with one element") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = OldSet.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("**OLD add on a non empty Set should yield a new Set with two elements") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = OldSet.empty.add(first).add(second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("**OLD remove on a non empty Set should yield a new Set without the element") {
    val element = randomString
    val setWithElement = OldSet.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }

  test("**OLD remove removes only the element in question") {
    val first = randomString
    val second = randomString

    val setWithElement = OldSet.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(second)

    setWithoutElement(second) shouldBe false
    setWithoutElement(first) shouldBe true

  }

  test("**OLD remove on an empty Set should yield an empty Set") {
    val element = randomString
    val stillEmpty = OldSet.empty.remove(element)
    stillEmpty(element) shouldBe false
  }

  test("**OLD remove on a non empty Set should yield a new Set without the element2") {
    val element = randomString
    val setWithElement = OldSet.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }

  test("**OLD remove removes only the element in question2") {
    val first = randomString
    val second = randomString

    val setWithElement = OldSet.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(first)

    setWithoutElement(first) shouldBe false
    setWithoutElement(second) shouldBe true
  }

  test("**OLD remove removes only the element in question (ordering test)") {
    val first = randomString
    val second = randomString

    val setWithElement = OldSet.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(second)

    setWithoutElement(first) shouldBe true
    setWithoutElement(second) shouldBe false
  }

  test("**OLD add/remove combo should ensure that all elements are distinct") {
    val element = randomString

    val set = OldSet.empty.add(element).add(element).remove(element)

    set(element) shouldBe false
  }

  test("**OLD union on empty Set should yield an empty Set") {
    OldSet.empty.union(OldSet.empty) shouldBe OldSet.empty
  }

  test(
    "**OLD union on a non empty Set with an empty set should yield the original Set untouched"
  ) {
      val first = randomString
      val second = randomString

      first should not be second

      val emptySet = OldSet.empty
      val nonEmptySet = OldSet.empty.add(first).add(second)

      emptySet.union(nonEmptySet)(first) shouldBe true
      emptySet.union(nonEmptySet)(second) shouldBe true

      nonEmptySet.union(emptySet)(first) shouldBe true
      nonEmptySet.union(emptySet)(second) shouldBe true
    }

  test("**OLD union on two non empty Sets should yield their union") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = OldSet.empty.add(a).add(b)
    val right = OldSet.empty.add(c).add(d)

    val union = left.union(right)

    union(a) shouldBe true
    union(b) shouldBe true
    union(c) shouldBe true
    union(d) shouldBe true

  }

  test("**OLD intersction on empty Set should yield empty Set") {

    OldSet.empty.intersection(OldSet.empty)(randomString) shouldBe false

  }

  test("**OLD intersection on two non empty Sets should yield their intersection") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = OldSet.empty.add(a).add(b).add(c)
    val right = OldSet.empty.add(c).add(d)

    val intersection = left.intersection(right)

    intersection(a) shouldBe false
    intersection(b) shouldBe false
    intersection(c) shouldBe true
    intersection(d) shouldBe false

  }

  test("**OLD difference on empty Set should yield empty Set") {

    OldSet.empty.difference(OldSet.empty)(randomString) shouldBe false

  }

  test("**OLD difference on a non empty Sets with empty should yield an empty Set") {

    val a = randomString
    val b = randomString

    val nonEmptySet = OldSet.empty.add(a).add(b)
    val emptySet = OldSet.empty

    emptySet.difference(nonEmptySet)(a) shouldBe false
    emptySet.difference(nonEmptySet)(b) shouldBe false

    nonEmptySet.difference(emptySet)(a) shouldBe true
    nonEmptySet.difference(emptySet)(b) shouldBe true

  }

  test("**OLD difference on two non empty Sets should yield their intersection") {

    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = OldSet.empty.add(a).add(b).add(c).add(d)
    val right = OldSet.empty.add(c).add(d)

    val difference = left.difference(right)

    difference(a) shouldBe true
    difference(b) shouldBe true
    difference(c) shouldBe false
    difference(d) shouldBe false

  }

  test("**OLD isSubSetOf on an empty Set should yierd true") {

    OldSet.empty.isSubSetOf(OldSet.empty) shouldBe true
    OldSet.empty.isSubSetOf(OldSet.empty.add(randomString)) shouldBe true

  }

  private def randomString: String =
    scala.util.Random.alphanumeric.take(5).mkString

}
