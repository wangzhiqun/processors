package edu.arizona.sista.learning

import java.io.File

import org.scalatest._

/**
 *
 * User: mihais
 * Date: 12/15/13
 */
class TestPerceptronClassifier extends FlatSpec with Matchers {
  "PerceptronClassifier" should "have an accuracy > .97 on this dataset" in {
    val classifier = new PerceptronClassifier[Int, Int](
      epochs = 10,
      marginRatio = 1.0)
    val dataset = Datasets.loadDatasetFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_train.txt.gz")
    classifier.train(dataset)

    val datums = Datasets.loadDatumsFromSvmLightFormat("src/test/resources/edu/arizona/sista/learning/classification_test.txt.gz")
    val acc = computeAcc(datums, classifier)
    //println("Accuracy: " + acc)
    acc should be > 0.97

    // make sure scores are the same after saving/loading
    val file = File.createTempFile("model", "dat")
    //println(s"Saving classifier to $file")
    file.deleteOnExit()
    classifier.saveTo(file.getAbsolutePath)
    val loadedClassifier = PerceptronClassifier.loadFrom[Int, Int](file.getAbsolutePath)
    val newAcc = computeAcc(datums, loadedClassifier)
    acc should be (newAcc)
  }

  def computeAcc(datums:Iterable[Datum[Int, Int]], classifier:Classifier[Int, Int]) = {
    var total = 0
    var correct = 0
    for(datum <- datums) {
      val l = classifier.classOf(datum)
      //println(s"prediction: $l")
      if(l == datum.label) correct += 1
      total += 1
    }
    val acc = correct.toDouble / total.toDouble
    acc
  }
}
