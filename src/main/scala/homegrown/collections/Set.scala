package homegrown.collections

trait Set extends (String => Boolean) {
  def add(input: String): Set
  def remove(input: String): Set
  def union(that: Set): Set
  def intersection(that: Set): Set
  def difference(that: Set): Set
  def isSubSetOf(that: Set): Boolean
  /*






  final def isSubSetOf(that: Set): Set = element =>
    ???
*/
}

object Set {

  //val empty: Set = input => false // ==>  explain below

  // because Set extends this function (String => Boolean)
  // it has one function "apply" so this thing [val empty: Set = input => false]
  // become
  private final case class NonEmpty(element: String, otherElements: Set) extends Set {
    override final def apply(input: String) =
      input == element || otherElements(input)

    final override def add(input: String): Set =
      if (input == element)
        this
      else
        NonEmpty(element, otherElements.add(input))

    final override def remove(input: String): Set =
      if (input == element)
        otherElements
      else
        NonEmpty(element, otherElements.remove(input))

    final override def union(that: Set): Set = {
      val newSet = that.add(element)
      otherElements.union(newSet)
    }

    final override def intersection(that: Set): Set = {

      if (that(element))
        otherElements.intersection(that).add(element)
      else
        otherElements.intersection(that)
    }

    final override def difference(that: Set): Set =
      {
        if (that(element))
          otherElements.difference(that)
        else
          otherElements.difference(that).add(element)
      }

    def isSubSetOf(that: Set): Boolean =
      that(element) && otherElements.isSubSetOf(that)

  }

  private object Empty extends Set {
    override final def apply(input: String) =
      false

    final override def add(input: String): Set =
      NonEmpty(input, Empty)

    final override def remove(input: String): Set =
      this

    final override def union(that: Set): Set =
      that

    final override def intersection(that: Set): Set =
      this

    final override def difference(that: Set): Set =
      this

    def isSubSetOf(that: Set): Boolean = true

  }

  val empty: Set = Empty
  /*
  val empty: Set = new Set {
    override final def apply(input: String) =
      false
  }
  */

}
