package HelpFunc

import java.io.File

import org.deeplearning4j.nn.graph.ComputationGraph
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Jack on 2017/11/8.
  */
object HelpFunc {

  def scoreModel(model: MultiLayerNetwork, setIter: DataSetIterator): Double = {
    if (setIter.resetSupported) setIter.reset()
    var score: Double = 0
    var count: Double = 0
    while (setIter.hasNext) {
      val ds: DataSet = setIter.next
      val nrow = ds.getFeatures.rows.toDouble
      score += nrow * model.score(ds, false)
      count += nrow
    }
    setIter.reset()
    score / count
  }

  def scoreModel(model: MultiLayerNetwork, data: Array[DataSet]): Double = {
    var score: Double = 0
    var count: Double = 0
    for (ds <- data) {
      val nrow = ds.getFeatures.rows
      score += nrow * model.score(ds, false)
      count += nrow
    }
    score / count
  }

  def scoreModel(model: MultiLayerNetwork, data: Array[DataSet], index: IndexedSeq[Int]): Double = {
    var score: Double = 0
    var count: Double = 0
    for (i <- index) {
      val ds = data(i)
      val nrow = ds.getFeatures.rows
      score += nrow * model.score(ds, false)
      count += nrow
    }
    score / count
  }

  def scoreModel(model: MultiLayerNetwork, data: ArrayBuffer[DataSet], index: IndexedSeq[Int]): Double = {
    var score: Double = 0
    var count: Double = 0
    for (i <- index) {
      val ds = data(i)
      val nrow = ds.getFeatures.rows
      score += nrow * model.score(ds, false)
      count += nrow
    }
    score / count
  }

  def scoreModel(model: ComputationGraph, data: Array[DataSet], index: IndexedSeq[Int]): Double = {
    var score: Double = 0
    var count: Double = 0
    for (i <- index) {
      val ds = data(i)
      val nrow = ds.getFeatures.rows
      score += nrow * model.score(ds, false)
      count += nrow
    }
    score / count
  }

  /**
    * a simple wrapper
    *
    * @param dsi     a dataset iterator
    * @param fileLoc location to save the datasets
    */
  def preSaveData(dsi: DataSetIterator, fileLoc: String): Unit = {
    val dir = new File(fileLoc)
    if (!dir.exists) dir.mkdirs
    var dataSaved = 0
    if (dsi.resetSupported) dsi.reset()
    while (dsi.hasNext) {
      val ds: DataSet = dsi.next
      ds.save(new File(fileLoc, dataSaved + ".bin"))
      dataSaved += 1
    }
    dsi.reset()
  }
}
