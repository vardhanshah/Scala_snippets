trait BTree {
  var occurrences: Int
  var height: Int
  protected def getHeight: Int
  protected def setHeight: Unit
  var size: Int
  protected def getSize: Int
  protected def setSize: Unit
  def search(x: Int): Boolean
  def searchOccurrences(x: Int): Int
  def insert(x: Int): BTree
  def index(ind: Int): Int
  def leftRotate(): BTree
  def rightRotate(): BTree
}

object Empty extends BTree {
  var occurrences: Int = 0
  var height: Int = 0
  var size: Int = 0
  def getHeight: Int = 0
  def setHeight: Unit = Unit
  def getSize: Int = 0
  def setSize: Unit = Unit
  def search(x: Int): Boolean = false
  def searchOccurrences(x: Int): Int = 0
  def insert(x: Int): BTree = Node(x,Empty,Empty)
  override def toString: String = "."
  def index(ind: Int): Int = throw new Exception("Index for Empty Node")
  def leftRotate(): BTree = Empty
  def rightRotate(): BTree = Empty
}
case class Node(element: Int, _left: BTree, _right: BTree) extends BTree {
  var left = _left
  var right = _right
  var height: Int = 1
  def getHeight: Int = 1 + (left.height max right.height)
  def setHeight: Unit = height = getHeight
  var occurrences: Int = 1
  var size: Int = _left.size + _right.size + occurrences
  def getSize: Int = left.size + right.size + occurrences
  def setSize: Unit = size = getSize
  def search(x: Int): Boolean =
    if (x == element)true
    else if (x < element) left.search(x)
    else right.search(x)
  def searchOccurrences(x: Int): Int =
    if (x == element)occurrences
    else if (x < element) left.searchOccurrences(x)
    else right.searchOccurrences(x)
  def leftRotate(): BTree = {
    val tmp: Node = right match {
      case n: Node => n
      case _ => throw new Exception("error in left rotate")
    }
    right = tmp.left
    tmp.left = this
    setHeight
    setSize
    tmp.setHeight
    tmp.setSize
    tmp
  }
  def rightRotate(): BTree = {
    val tmp = left match {
      case n: Node => n
      case _ => throw new Exception("error in right rotate")
    }
    left = tmp.right
    tmp.right = this
    setHeight
    setSize
    tmp.setHeight
    tmp.setSize
    tmp
  }
  def insert(x: Int): BTree = {
    if (element < x) {
      right = right.insert(x)
    }
    else if (element > x) {
      left = left.insert(x)
    }
    else {
      occurrences += 1
    }
    setSize
    setHeight
    //    println()
    val balancef: Int = left.height - right.height
//    println(balancef)
    if (balancef >= -1 && balancef <= 1)
      this
    else if (balancef > 1)
      left match {
        case n: Node =>
          if (n.element > x)
            rightRotate()
          else {
            left = n.leftRotate()
            rightRotate()
          }
        case _ => throw new Exception("error in balancef left")
      }
    else
      right match {
        case n: Node =>
          if (n.element < x)
            leftRotate()
          else {
            right = n.rightRotate()
            leftRotate()
          }
        case _ => throw new Exception("error in balancef right")
      }
  }
  def index(ind: Int): Int = {
    val lsize = left.size
    if (ind>lsize && ind < lsize+occurrences+1 ) element
    else if (lsize >= ind) left.index(ind)
    else right.index(ind-lsize-occurrences)
  }
  override def toString: String =
    "{" + left + " " + element + " " + right + "}"

}


