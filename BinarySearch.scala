object BinarySearch {
  def lower_bound(_l: Int, _h: Int,f: Int => Boolean): Int = {
    var (l,h) = (_l, _h)
    var mid = 0
    while (l<h) {
      mid = l + (h-l)/2
      if (f(mid))
        h = mid
      else
        l = mid + 1
    }
    if (!f(l))
      -1
    else
      l
  }
  def upper_bound(_l: Int, _h: Int, f: Int => Boolean): Int = {
    var (l,h) = (_l,_h)
    var mid: Int = 0
    while (l<h) {
      mid = l + (h-l+1)/2
      if (f(mid))
        h = mid - 1
      else
        l = mid
    }
    if (f(l))
      -1
    else
      l
  }
  def binary_search(nums: Array[Int], _l: Int, _h: Int, target: Int): Int = {
    var (l,h) = (_l,_h)
    var mid: Int = 0
    while (l<=h) {
      mid = l + (h-l)/2
      if (nums(mid) == target)
        return mid
      else if (nums(mid) > target)
        h = mid - 1
      else
        l = mid + 1
    }
    -1
  }
}
