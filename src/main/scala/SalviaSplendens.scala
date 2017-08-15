package main.scala

import main.scala.obj.Dictionary
import main.scala.obj.Document
import main.scala.helper.LDACmdOption
import main.java.commons.cli.MissingOptionException
import main.java.commons.cli.MissingArgumentException
import main.java.commons.cli.CommandLine
import main.java.commons.cli.Option
import main.java.commons.cli.UnrecognizedOptionException
import main.scala.obj.Parameter
import main.scala.helper.Utils
import main.scala.obj.Model
import main.scala.connector.File2LDADataset
import java.io.File
import main.scala.connector.Dictionary2File

object SalviaSplendens {
  def main(args: Array[String]): Unit = {
    println("#################### LDA vs. Source LDA ####################")
    try {
      var cmd = LDACmdOption.getArguments(args)
      if (cmd.hasOption("help")) {
        LDACmdOption.showHelp()
      } else {
        // set user parameters
        var params = new Parameter
        params.getParams(cmd)
        if (!params.checkRequirement) {
          println("ERROR!!! Phai nhap day du cac tham so: alpha, beta, directory, datafile, ksfile, ntopics, niters")
          LDACmdOption.showHelp()
          return
        } else {

          //~~~~~~~~~~~ Timer ~~~~~~~~~~~
          val startTime = System.currentTimeMillis()

          //~~~~~~~~~~~ Body ~~~~~~~~~~~
          //println("#################### DAY LA PHAN THAN CUA CHUONG TRINH ####################")

          // Create LDADataset
          var data = File2LDADataset.readDataSet(params.directory + File.separator + params.datafile)
          // Write dictionary to file
          Dictionary2File.writeWordMap(params.directory + File.separator + "output" + File.separator + params.wordMapFileName, data.localDict.word2id)

          println("PURE LDA")
          var estimator = new Estimator
          println("LDA preparing...")
          estimator.init(params, data)
          println("LDA estimating...")
          estimator.estimate(params.savestep)
          println("LDA finish successfully!")

          println("SOURCE LDA")
          var srcEstimate = new SrcEstimator
          println("SourceLDA preparing...")
          srcEstimate.init(params, data)
          println("SourceLDA estimating...")
          srcEstimate.estimate(params.savestep)
          println("SourceLDA finish successfully!")

          println(s"Kullback–Leibler divergence from Source-LDA to LDA: ${Utils.calcKullbackLeiblerDivergence(estimator.trnModel.phi, srcEstimate.trnModel.phi)}")

          println(s"Perplexity of LDA model: ${estimator.computePerplexity}")
          println(s"Perplexity of Source-LDA model: ${srcEstimate.computePerplexity}")

          //~~~~~~~~~~~ Timer ~~~~~~~~~~~
          val duration = System.currentTimeMillis() - startTime
          val millis = (duration % 1000).toInt
          val seconds = ((duration / 1000) % 60).toInt
          val minutes = ((duration / (1000 * 60)) % 60).toInt
          val hours = ((duration / (1000 * 60 * 60)) % 24).toInt
          println("#################### Finished in " + hours + " hour(s) " + minutes + " minute(s) " + seconds + " second(s) and " + millis + " millisecond(s) ####################")
        }
      }
    } catch {
      case moe: MissingOptionException => {
        println("ERROR!!! Phai nhap day du cac tham so: alpha, beta, directory, datafile, ksfile, ntopics, niters")
        LDACmdOption.showHelp()
      }
      case mae: MissingArgumentException => {
        mae.printStackTrace()
        println("ERROR!!! Thieu gia tri cua cac tham so.")
        LDACmdOption.showHelp()
      }
      case uoe: UnrecognizedOptionException => {
        uoe.printStackTrace()
        println("ERROR!!! Chuong trinh khong ho tro tham so ban da nhap.")
        LDACmdOption.showHelp()
      }
      case e: Throwable => e.printStackTrace()
    }
  }
}
