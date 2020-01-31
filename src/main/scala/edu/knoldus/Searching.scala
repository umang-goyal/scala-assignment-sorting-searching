package edu.knoldus

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    def bs(list: Array[Int], lowerBound: Int, upperBound: Int, elem: Int): Boolean = {

      if (lowerBound > upperBound) {
        return false
      }

      val mid: Int = (lowerBound + upperBound) / 2

      if (elem == list(mid)) {
        return true
      }

      else if (elem < list(mid)) {
        return bs(list, lowerBound, mid - 1, elem)
      }

      else if (elem > list(mid)) {
        return bs(list, mid + 1, upperBound, elem)
      }

      false
    }

    return bs(array, lowerBound = 0, upperBound = array.length - 1, elem)
    false
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    for (x <- array) {
      if (x == elem) {
        return true
      }
    }
    false
  }


}
