package HelpFunc

import java.util.Random

import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.layers.{DenseLayer, OutputLayer}
import org.deeplearning4j.nn.conf.{MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.lossfunctions.LossFunctions

/**
  * Created by Jack on 2017/11/6.
  */
class MLP(var shape: Array[Int]) {

  var learningRate = 0.005D
  var seed: Int = -1
  var activation = Activation.RELU
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
      .iterations(1)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .updater(Updater.NESTEROVS).momentum(0.9D)
      .weightInit(WeightInit.XAVIER)
      .activation(this.activation)
      .learningRate(this.learningRate)
      .list

    // hidden layer
    for (i <- 0 until shape.length - 2) {
      config = config.layer(i,
        new DenseLayer.Builder().nIn(shape(i)).nOut(shape(i + 1)).build)
    }

    // output layer
    config = config.layer(shape.length-2,
      new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
        .activation(Activation.SOFTMAX)
        .nOut(shape.last)
        .dropOut(dropRate)
        .build)
      .pretrain(false).backprop(true)

    config.build
  }
}