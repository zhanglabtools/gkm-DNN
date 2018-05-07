package HelpFunc

import java.util.Random

import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.{MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.conf.layers.{ActivationLayer, BatchNormalization, DenseLayer, OutputLayer}
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.lossfunctions.LossFunctions

/**
  * Created by Jack on 2017/11/8.
  */
class BN(var shape: Array[Int]) {

  var learningRate = 0.005D
  var seed: Int = -1
  var dropRate = 0.8D

  def randomSeed(): Unit = {
    this.seed = new Random().nextInt(200)
  }

  def build(): MultiLayerConfiguration = {

    // random seeds
    if (this.seed < 0) {
      val random = new Random
      this.seed = random.nextInt(200)
    }

    // common part
    var config = new NeuralNetConfiguration.Builder()
      .seed(this.seed)
      .learningRate(this.learningRate)
      .iterations(1)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .updater(Updater.NESTEROVS).momentum(0.9D)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.IDENTITY) 
      .list

    // hidden layer
    var layerCount = 0
    for (i <- 0 until shape.length - 2) {
      config = config.layer(layerCount, new DenseLayer.Builder()
          .nIn(shape(i))
          .nOut(shape(i + 1))
          .build)
      layerCount += 1
      config = config.layer(layerCount, new BatchNormalization())
      layerCount += 1
      config = config.layer(layerCount, new ActivationLayer.Builder().activation(Activation.RELU).build)
      layerCount += 1
    }

    // output layer
    config = config.layer(layerCount,
      new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
        .activation(Activation.SOFTMAX)
        .nOut(shape.last)
        .dropOut(dropRate)
        .build)


    config.pretrain(false).backprop(true).build
  }
}
