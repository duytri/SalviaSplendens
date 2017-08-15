package main.scala.helper

import scala.collection.mutable.ArrayBuffer
import main.scala.obj.LDADataset

object Utils {
  def printTopWords(topic: Int, vocabSize: Int, phi: Array[Array[Double]], data: LDADataset, top: Int): Unit = {
    var wordsProbsList = new ArrayBuffer[(Int, Double)]
    for (w <- 0 until vocabSize) {
      wordsProbsList.append((w, phi(topic)(w)))
    } //end foreach word
    wordsProbsList = wordsProbsList.sortWith(_._2 > _._2)

    for (i <- 0 until top) {
      if (data.localDict.contains(wordsProbsList(i)._1)) {
        println("\t" + data.localDict.getWord(wordsProbsList(i)._1) + " " + wordsProbsList(i)._2);
      }
    }
  }

  /**
   * Calculate Kullback–Leibler divergence from Q to P
   * @param phiP discrete probability distributions P [T x V]
   * @param phiQ discrete probability distributions Q [T x V]
   * @return Kullback–Leibler divergence from Q to P
   */
  def calcKullbackLeiblerDivergence(phiP: Array[Array[Double]], phiQ: Array[Array[Double]]): Double = {
    var T = phiP.length // number of topic
    var V = phiP(0).length // number of vocabulary
    var KLDEachTopic = Array.ofDim[Double](T)
    for (t <- 0 until T) {
      for (v <- 0 until V) {
        KLDEachTopic(t) += phiP(t)(v) * Math.log(phiP(t)(v) / phiQ(t)(v))
      }
    }
    val sumKL = KLDEachTopic.reduce(_ + _)
    return sumKL / T
  }
}