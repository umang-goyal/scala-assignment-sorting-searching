package edu.knoldus

import org.scalatest.FunSuite

class SortingTest extends FunSuite {

  val sorting = new Sorting

  test("Test Insertion Sort") {
    val array: Array[Int] = Array(3, 4, 5, 3, 2, 1)
    assert(sorting.insertionSort(array) === array.sortBy(a => a))
  }

  test("Test Selection Sort") {
    val array: Array[Int] = Array(3, 4, 5, 3, 2, 1)
    assert(sorting.selectionSort(array) === array.sortBy(a => a))
  }

  test("Test Bubble Sort") {
    val array: Array[Int] = Array(3, 4, 5, 3, 2, 1)
    assert(sorting.bubbleSort(array) === array.sortBy(a => a))
  }

}
