package main.scala.obj

import scala.collection.mutable.ArrayBuffer

class KnowledgeSource(var knowledge: Array[(String, Array[(String, Int)])], var K: Int, var delta: Array[Array[(Int, Int)]], var deltaPow: Array[Array[Double]], var deltaPowSum: Array[Double]) {

  def initDelta(dict: Dictionary): Unit = {
    delta = new Array[Array[(Int, Int)]](knowledge.length)
    for (topic <- 0 until knowledge.length) {
      var rowBuff = new ArrayBuffer[(Int, Int)]
      knowledge(topic)._2.foreach {
        case (word: String, freq: Int) => {
          rowBuff.append((dict.getId(word), freq))
        }
      }
      delta(topic) = rowBuff.toArray
    }
  }

  def initDeltaPow(lamda: Double): Unit = {
    deltaPow = Array.ofDim[Array[Double]](knowledge.length)
    deltaPowSum = Array.ofDim[Double](knowledge.length)

    for (topic <- 0 until knowledge.length) {
      val deltaTopic = getDeltaJ(topic)
      var rowBuff = new ArrayBuffer[Double]
      var sumBuff = 0.0
      for (word <- 0 until deltaTopic.length) {
        rowBuff.append(math.pow(deltaTopic(word), lamda))
        sumBuff += rowBuff(word)
      }
      deltaPow(topic) = rowBuff.toArray
      deltaPowSum(topic) = sumBuff
    }
  }

  def this(lamda: Double, dict: Dictionary, knowledge: Array[(String, Array[(String, Int)])]) = {
    this(knowledge, knowledge.length, null, null, null)
    initDelta(dict)
    initDeltaPow(lamda)
  }

  def getDeltaIJ(jTopic: Int, iWord: Int): Int = {
    return delta(jTopic)(iWord)._2
  }

  def getDeltaIJ(jTopic: Int, iWord: String): Int = {
    knowledge(jTopic)._2.foreach {
      case (word: String, freq: Int) => {
        if (word.equalsIgnoreCase(iWord)) return freq
      }
    }
    return 0
  }

  def getDeltaJ(jTopic: Int): Array[Int] = {
    knowledge(jTopic)._2.map {
      case (word: String, freq: Int) => {
        freq
      }
    }
  }

  def getNumberOfTopic(): Int = {
    K
  }

  /*def getDeltaPow(jTopic: Int, iWord: Int): Double = {
    if(deltaPow(jTopic).contains(iWord))
      return deltaPow(jTopic)(iWord)._2
    return 0
  }*/
}