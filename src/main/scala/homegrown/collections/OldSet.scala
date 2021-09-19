package homegrown.collections

trait OldSet extends (String => Boolean) {
  final def add(input: String): OldSet = element =>
    element == input || this(element)

  final def remove(input: String): OldSet = element =>
    input != element && this(input) // input and element is the same thing because input != element

  final def union(that: OldSet): OldSet = element =>
    this(element) || that(element)

  final def intersection(that: OldSet): OldSet = element =>
    this(element) && that(element)

  final def difference(that: OldSet): OldSet = element =>
    this(element) && !that(element)

  final def isSubSetOf(that: OldSet): OldSet = element =>
    ???

}

object OldSet {

  //val empty: OldSet = input => false // ==>  explain below

  // because OldSet extends this function (String => Boolean)
  // it has one function "apply" so this thing [val empty: OldSet = input => false]
  // become
  val empty: OldSet = new OldSet {
    override final def apply(input: String) =
      false
  }

}
