package edu.knoldus

class Sorting {

 def selectionSort(array: Array[Int]): Array[Int] = {
  @scala.annotation.tailrec
  def sSort(array: Array[Int], intialIndex: Int): Array[Int] = {
    def findMin(array: Array[Int], startIndex: Int): Int= {
      for (i <- startIndex until array.length ) {
        if (array(i) < array(startIndex)) {
          return findMin(array, i)
        }
      }
      startIndex
    }
    def swap(array: Array[Int], i: Int, j: Int) {
      val tmp = array(i)
      array(i) = array(j)
      array(j) = tmp
    }


    if (intialIndex == array.length){
      return array
    }
    val min = findMin(array, intialIndex)
    swap(array,intialIndex, min)

    sSort(array, intialIndex+1)
  }
  sSort(array,0)
}

  def bubbleSort(array: Array[Int]): Array[Int] = {
  val n: Int = array.length
  @scala.annotation.tailrec
  def bs(array: Array[Int], length: Int): Array[Int]= {
    def swap(array: Array[Int], index: Int) {
      val tmp = array(index)
      array(index) = array(index - 1)
      array(index - 1) = tmp
    }
    for (i <- 1 until length) {
      if (array(i - 1) >= array(i)) {
        swap(array,i)
      }
    }
    if (length==1) {
      array
    }
    else {
      bs(array, length - 1)
    }
  }
  bs(array, n-1)
  array
}

  def insertionSort(array: Array[Int]): Array[Int] = {
    for(i<-1 until array.length-1)
    {
      for(j<- i to 1 by -1)
      {
        if(array(j-1)>array(j))
        {
          val temp=array(j)
          array(j)=array(j-1)
          array(j-1)=temp
        }
      }
    }
    array
  }
