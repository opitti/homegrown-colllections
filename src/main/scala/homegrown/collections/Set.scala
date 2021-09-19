package homegrown.collections

trait Set extends (String => Boolean) {
  import Set._ // beacause we need NonEmpty(...)
  final def apply(input: String): Boolean = {
    var result = false

    foreach { current =>
      result = result || current == input
    }

    result

  }

  final def add(input: String): Set = {
    var result = NonEmpty(input, empty) // do what we do for the Empty cas

    foreach { current =>
      // always the logic here
      if (current == input) {}
      else
        // always result =
        result = NonEmpty(current, result)
    }

    result // always the result

  }

  final def remove(input: String): Set = {
    var result = empty

    foreach { current =>
      // always the logic here
      if (current == input) {}
      else
        // always result =
        result = NonEmpty(current, result)
    }
    result
  }

  final def union(that: Set): Set = {
    //val newSet = that.add(element)
    //otherElements.union(newSet)
    var result = that

    foreach {
      current =>
        result = result.add(current)
    }

    result

  }

  final def intersection(that: Set): Set = {

    /*
      if (that(element))
        otherElements.intersection(that).add(element)
      else
        otherElements.intersection(that)
*/
    var result = empty
    foreach { current =>
      if (that(current))
        result = result.add(current)
    }

    result
  }

  final def difference(that: Set): Set =
    {
      /*
        if (that(element))
          otherElements.difference(that)
        else
          otherElements.difference(that).add(element)
      */

      var result = empty

      foreach { current =>
        if (!that(current))
          result = result.add(current)

      }

      result
    }

  final def isSubSetOf(that: Set): Boolean =
    {
      // that(element) && otherElements.isSubSetOf(that)
      var result = true

      foreach { current =>
        result = result && that(current)
      }

      result

    }
  //def add(input: String): Set
  //def remove(input: String): Set
  //def union(that: Set): Set
  //def intersection(that: Set): Set
  //def difference(that: Set): Set
  //def isSubSetOf(that: Set): Boolean
  def nonEmpty(): Boolean
  def isSupersetOf(that: Set): Boolean
  def size: Int

  final override def equals(other: Any): Boolean = other match {

    case that: Set => this.isSubSetOf(that) && that.isSupersetOf(this)
    case _         => false
  }

  final def foreach(function: String => Unit): Unit = {

    if (nonEmpty) {
      // works cause pattern mathing but it's expensive because of the equality
      //val NonEmpty(element, otherElements) = this

      // les 3 lignes du dessousremplacent le pattern matching du dessus
      val nonEmptySet = this.asInstanceOf[NonEmpty]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      function(element)
      otherElements.foreach(function)
    }
  }

} // end of the trait

object Set {
  def apply(element: String, otherElements: String*): Set = {
    var result: Set = empty.add(element)

    // this is not our foreach !!!!!!!
    otherElements.foreach { current =>
      result = result.add(current)
    }
    result
  }

  //val empty: Set = input => false // ==>  explain below

  // because Set extends this function (String => Boolean)
  // it has one function "apply" so this thing [val empty: Set = input => false]
  // become
  private final case class NonEmpty(element: String, otherElements: Set) extends Set {

    /* first implementation
    final override def add(input: String): Set =
      if (input == element)
        this
      else
        NonEmpty(element, otherElements.add(input))
    */

    // always the same things

    /* it can go to the trait
    final override def add(input: String): Set = {
      var result = NonEmpty(input, empty) // do what we do for the Empty cas

      foreach { current =>
        // always the logic here
        if (current == input) {}
        else
          // always result =
          result = NonEmpty(current, result)
      }

      result // always the result
    }
    */

    /*
    final override def remove(input: String): Set =
      if (input == element)
        otherElements
      else
        NonEmpty(element, otherElements.remove(input))
    */

    final override def nonEmpty(): Boolean = true

    def isSupersetOf(that: Set): Boolean =
      that.isSubSetOf(this)

    final override def hashCode: Int =
      element.hashCode + otherElements.hashCode

    final override def size: Int =
      1 + otherElements.size

    /*
    final override def foreach(function: String => Unit): Unit = {

      function(element)
      otherElements.foreach(function)

    }
*/

  } // fin de NonEmpty

  private object NonEmpty { // pattern Matching need to be and Object not a case class ....
    private[this] def unapply(any: Any): Option[(String, Set)] =
      patternMatchingNotSupported
  }

  private object Empty extends Set {

    /* it's in the trait no more needed here
    final override def add(input: String): Set =
      NonEmpty(input, Empty)
  */
    //final override def remove(input: String): Set =
    //  this

    // final override def union(that: Set): Set =
    //   that

    //final override def intersection(that: Set): Set =
    //   this

    //final override def difference(that: Set): Set =
    //  this

    // def isSubSetOf(that: Set): Boolean = true
    def isSupersetOf(that: Set): Boolean = true

    final override def nonEmpty(): Boolean = false

    final override def size: Int = 0

    //final override def foreach(function: String => Unit): Unit = ()
    private[this] def unapply(any: Any): Option[(String, Set)] =
      patternMatchingNotSupported
  } // end Empty

  val empty: Set = Empty
  /*
  val empty: Set = new Set {
    override final def apply(input: String) =
      false
  }
  */

  private[this] def unapply(any: Any): Option[(String, Set)] =
    patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("pattern matching on Sets is expensive and therefore not supported")

} // Set Object
