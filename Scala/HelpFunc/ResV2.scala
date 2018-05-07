package HelpFunc

import java.util.Random

import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.{ComputationGraphConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.conf.graph.ElementWiseVertex
import org.deeplearning4j.nn.conf.inputs.InputType
import org.deeplearning4j.nn.conf.layers.{ActivationLayer, BatchNormalization, DenseLayer, OutputLayer}
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.lossfunctions.LossFunctions

/**
  * Created by Jack on 2017/11/8.
  */
class ResV2(var numIN: Int, var width: Int, var numRes: Int) {


  var learningRate = 0.005D
  var seed: Int = -1
  var dropRate = 0.8D

  def randomSeed(): Unit = {
    this.seed = new Random().nextInt(200)
  }

  override def toString: String = width + "_" + numRes + "_" + learningRate

  private def identityBlock(graph: ComputationGraphConfiguration.GraphBuilder,
                            stage: String, input: String):Unit = {
    val layers = Array("BN1", "ReLU1", "FC1", "BN2", "ReLU2", "FC2", "out").map(stage + "-" + _)

    graph
      .addLayer(layers(0), new BatchNormalization(), input)
      .addLayer(layers(1), new ActivationLayer.Builder().activation(Activation.RELU).build(), layers(0))
      .addLayer(layers(2), new DenseLayer.Builder().nOut(this.width).build(), layers(1))
      .addLayer(layers(3), new BatchNormalization(), layers(2))
      .addLayer(layers(4), new ActivationLayer.Builder().activation(Activation.RELU).build(), layers(3))
      .addLayer(layers(5), new DenseLayer.Builder().nOut(this.width).build(), layers(4))
      .addVertex(layers(6), new ElementWiseVertex(ElementWiseVertex.Op.Add), input, layers(5))
  }

  def resBuilder():ComputationGraphConfiguration = {

    if (this.seed < 0) {
      val random = new Random
      this.seed = random.nextInt(200)
    }

    val graph: ComputationGraphConfiguration.GraphBuilder = new NeuralNetConfiguration.Builder()
      .seed(seed)
      .iterations(1)
      .activation(Activation.IDENTITY)
      .learningRate(learningRate)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
      .updater(Updater.NESTEROVS).momentum(0.9)
      .weightInit(WeightInit.XAVIER)
      .graphBuilder()


    graph
      .addInputs("input").setInputTypes(InputType.feedForward(numIN))
      .addLayer("0-out", new DenseLayer.Builder().nIn(numIN).nOut(this.width).build(), "input")

    for (i <- 1 to numRes) identityBlock(graph, i+"", (i-1) + "-out")

    graph
      .addLayer("post-BN", new BatchNormalization(), numRes + "-out")
      .addLayer("post-ReLU", new ActivationLayer.Builder().activation(Activation.RELU).build(), "post-BN")
      .addLayer("output",
        new OutputLayer.Builder(LossFunctions.LossFunction.NEGATIVELOGLIKELIHOOD)
          .dropOut(dropRate).nOut(2).activation(Activation.SOFTMAX).build(),"post-ReLU")
      .setOutputs("output").backprop(true).pretrain(false)

    graph.build()
  }
}
