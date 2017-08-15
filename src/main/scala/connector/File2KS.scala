package main.scala.connector

import main.scala.obj.KnowledgeSource
import java.io.File
import java.io.BufferedReader
import java.io.FileReader
import main.scala.obj.KnowledgeSource
import java.util.StringTokenizer
import scala.collection.mutable.ArrayBuffer
import main.scala.obj.Dictionary

object File2KS {
  def readKnowledgeSrc(lamda: Double, dict: Dictionary, filename: String): KnowledgeSource = {
    val file = new File(filename)
    val reader = new BufferedReader(new FileReader(file))
    var line = reader.readLine()
    var resBuffer = new ArrayBuffer[(String, Array[(String, Int)])]
    while (line != null) {
      val tknr = new StringTokenizer(line, " ")
      val str_frq = new ArrayBuffer[(String, Int)]((tknr.countTokens() - 1) / 2)
      val topic = tknr.nextToken()
      while (tknr.hasMoreTokens()) {
        str_frq.append((tknr.nextToken(), tknr.nextToken().toInt))
      }
      resBuffer.append((topic, str_frq.toArray))
      line = reader.readLine()
    }
    new KnowledgeSource(lamda, dict, resBuffer.toArray)
  }
}