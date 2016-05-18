package edu.arizona.sista.learning

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.struct.Lexicon
import org.slf4j.LoggerFactory
import Dataset._

/**
 * Parent class for classification datasets
 * User: mihais
 * Date: 4/23/13
 */
abstract class Dataset[L, F](
  val labelLexicon:Lexicon[L],
  val featureLexicon:Lexicon[F],
  val labels:ArrayBuffer[Int]) extends Serializable {

  def this() = this(new Lexicon[L], new Lexicon[F], new ArrayBuffer[Int])

  def += (datum:Datum[L, F])

  def numFeatures = featureLexicon.size
  def numLabels = labelLexicon.size

  /** number of training examples */
  def size = labels.size

  def indices = 0 until size

  def featuresCounter(datumOffset:Int):Counter[Int]

  /** Returns the Datum for given row */
  def mkDatum(row:Int): Datum[L, F]

  /** Removes features that appear less than threshold times in this dataset. */
  def removeFeaturesByFrequency(threshold:Int):Dataset[L, F]

  /** Removes features by information gain. */
  def removeFeaturesByInformationGain(pctToKeep:Double):Dataset[L, F]

  /** Creates a new dataset keeping only the features in the given set */
  def keepOnly(featuresToKeep:Set[Int]):Dataset[L, F]

  /** Convert this dataset to a CounterDataset */
  def toCounterDataset:CounterDataset[L, F]
}


object Dataset {
  val logger = LoggerFactory.getLogger(classOf[Dataset[String, String]])
}


/**
 * Dataset containing only BVFDatums
 * Important note: to encode feature values > 1, simply store the same feature multiple times (equal to feature value)!
  *
  * @tparam L Type of labels
 * @tparam F Type of features
 */
class BVFDataset[L, F] (
  ll:Lexicon[L],
  fl:Lexicon[F],
  ls:ArrayBuffer[Int],
  val features:ArrayBuffer[Array[Int]]) extends Dataset[L, F](ll, fl, ls) {

  def this() = this(
    new Lexicon[L], new Lexicon[F], new ArrayBuffer[Int],
    new ArrayBuffer[Array[Int]])

  def += (datum:Datum[L, F]) {
    datum match {
      case bd:BVFDatum[L, F] =>
        labels += labelLexicon.add(bd.label)
        features += featuresToArray(bd.features)
      case _ => throw new RuntimeException("ERROR: you cannot add a non BVFDatum to a BVFDataset!")
    }
  }

  private def featuresToArray(fs:Iterable[F]):Array[Int] = {
    val fb = new ListBuffer[Int]
    for(f <- fs) fb += featureLexicon.add(f)
    fb.toList.sorted.toArray
  }

  override def mkDatum(row:Int): Datum[L, F] = {
    val feats = for (f <- features(row)) yield featureLexicon.get(f)
    new BVFDatum[L, F](labelLexicon.get(labels(row)), feats)
  }

  override def featuresCounter(datumOffset:Int):Counter[Int] = {
    val c = new Counter[Int]
    features(datumOffset).foreach(f => c.incrementCount(f))
    c
  }

  def countFeatures(fs:ArrayBuffer[Array[Int]], threshold:Int):Set[Int] = {
    val counts = new Counter[Int]
    for(d <- fs) {
      for(f <- d) {
        counts.incrementCount(f)
      }
    }
    logger.debug("Total unique features before filtering: " + counts.size)

    val passed = new mutable.HashSet[Int]()
    for(f <- counts.keySet) {
      if(counts.getCount(f) >= threshold)
        passed += f
    }
    logger.debug(s"Total unique features after filtering with threshold $threshold: ${passed.size}")

    passed.toSet
  }

  override def removeFeaturesByInformationGain(pctToKeep:Double):Dataset[L, F] = {
    logger.debug("Computing information gain for all features in dataset...")

    // compute information gain per feature
    val (total, igs) = computeInformationGains(features, labels)
    logger.debug("Total unique features before filtering: " + igs.size)

    // sort all features in descending order of their IG
    val fb = new ListBuffer[(Int, Double)]
    for(f <- igs.keySet) fb += new Tuple2(f, igs.get(f).get.ig(total))
    val sortedFeats = fb.sortBy(- _._2).toArray

    // keep the top pctToKeep
    val maxLen = (pctToKeep * sortedFeats.length.toDouble).ceil.toInt
    assert(maxLen > 0 && maxLen <= sortedFeats.length)
    logger.debug(s"Will keep $maxLen features after filtering by IG.")

    // these are the features to keep
    val featsToKeep = new mutable.HashSet[Int]()
    for(i <- 0 until maxLen) featsToKeep += sortedFeats(i)._1

    // keep only these features in the dataset
    keepOnly(featsToKeep.toSet)
  }

  def computeInformationGains(fs:ArrayBuffer[Array[Int]], ls:ArrayBuffer[Int]):(InformationGain, Map[Int, InformationGain]) = {
    val igs = new mutable.HashMap[Int, InformationGain]()
    val total = new InformationGain()

    // count occurrence of f with label l
    for(i <- fs.indices) {
      val d = fs(i)
      val l = ls(i)

      total.datumCount += 1
      total.datumsByClass.incrementCount(l)

      for(f <- d) {
        val ig = igs.getOrElseUpdate(f, new InformationGain)
        ig.datumCount += 1
        ig.datumsByClass.incrementCount(l)
      }
    }

    (total, igs.toMap)
  }


  override def removeFeaturesByFrequency(threshold:Int):Dataset[L, F] = {
    // compute feature frequencies and keep the ones above threshold
    val counts = countFeatures(features, threshold)

    keepOnly(counts)
  }

  override def keepOnly(featuresToKeep:Set[Int]):Dataset[L, F] = {
    // map old feature ids to new ids, over the filtered set
    val featureIndexMap = new mutable.HashMap[Int, Int]()
    var newId = 0
    for(f <- 0 until featureLexicon.size) {
      if(featuresToKeep.contains(f)) {
        featureIndexMap += f -> newId
        newId += 1
      }
    }

    // construct the new dataset with the filtered features
    val newFeatures = new ArrayBuffer[Array[Int]]
    for(row <- features.indices) {
      val nfs = keepOnlyRow(features(row), featureIndexMap)
      newFeatures += nfs
    }

    new BVFDataset[L, F](labelLexicon, featureLexicon.mapIndicesTo(featureIndexMap.toMap), labels, newFeatures)
  }

  def keepOnlyRow(feats:Array[Int], featureIndexMap:mutable.HashMap[Int, Int]):Array[Int] = {
    val newFeats = new ArrayBuffer[Int]()

    for(i <- feats.indices) {
      val f = feats(i)
      if(featureIndexMap.contains(f)) {
        newFeats += featureIndexMap.get(f).get
      }
    }

    newFeats.toArray
  }

  override def toCounterDataset:CounterDataset[L, F] = {
    val cfs = new ArrayBuffer[Counter[Int]]()
    for(d <- features.indices) {
      val cf = new Counter[Int]
      for(i <- features(d).indices) {
        cf.incrementCount(features(d)(i))
      }
      cfs += cf
    }
    new CounterDataset[L, F](labelLexicon, featureLexicon, labels, cfs)
  }
}

/**
  * Dataset containing only RVFDatums
  * @tparam L Type of labels
  * @tparam F Type of features
  */
class RVFDataset[L, F] (
  ll:Lexicon[L],
  fl:Lexicon[F],
  ls:ArrayBuffer[Int],
  fs:ArrayBuffer[Array[Int]],
  val values:ArrayBuffer[Array[Double]]) extends BVFDataset[L, F](ll, fl, ls, fs) with FeatureTraversable[F, Double]{

  def this() = this(
    new Lexicon[L], new Lexicon[F], new ArrayBuffer[Int],
    new ArrayBuffer[Array[Int]],
    new ArrayBuffer[Array[Double]])

  override def += (datum:Datum[L, F]) {
    datum match {
      case d:RVFDatum[L, F] =>
        labels += labelLexicon.add(d.label)
        val fvs = featuresCounterToArray(d.featuresCounter)
        features += fvs.map(fv => fv._1)
        values += fvs.map(fv => fv._2)
      case _ => throw new RuntimeException("ERROR: you cannot add a non RVFDatum to a RVFDataset!")
    }
  }

  private def featuresCounterToArray(fs:Counter[F]):Array[(Int, Double)] = {
    val fb = new ListBuffer[(Int, Double)]
    for(f <- fs.keySet) {
      fb += new Tuple2[Int, Double](featureLexicon.add(f), fs.getCount(f))
    }
    fb.sortBy(_._1).toArray
  }

  override def featuresCounter(datumOffset:Int):Counter[Int] = {
    val c = new Counter[Int]
    val fs = features(datumOffset)
    val vs = values(datumOffset)
    for(i <- fs.indices) {
      c.incrementCount(fs(i), vs(i))
    }
    c
  }

  override def mkDatum(row:Int): Datum[L, F] = {
    val intFeats = featuresCounter(row)
    val feats = new Counter[F]
    for (f <- intFeats.keySet) {
      feats.setCount(featureLexicon.get(f), intFeats.getCount(f))
    }
    new RVFDatum[L, F](labelLexicon.get(labels(row)), feats)
  }

  def featureUpdater: FeatureUpdater[F, Double] = new FeatureUpdater[F, Double] {
    def foreach[U](fn: ((F, Double)) => U): Unit = {
      for(i <- 0 until RVFDataset.this.size) {
        for(j <- features(i).indices) {
          val fi = features(i)(j)
          val v = values(i)(j)
          val f = featureLexicon.get(fi)
          fn((f, v))
        }
      }
    }

    def updateAll(fn: ((F, Double)) => Double): Unit = {
      for(i <- 0 until RVFDataset.this.size) {
        for(j <- features(i).indices) {
          val fi = features(i)(j)
          val v = values(i)(j)
          val f = featureLexicon.get(fi)
          values(i)(j) = fn((f, v))
        }
      }
    }
  }

  override def keepOnly(featuresToKeep:Set[Int]):Dataset[L, F] = {
    // map old feature ids to new ids, over the filtered set
    val featureIndexMap = new mutable.HashMap[Int, Int]()
    var newId = 0
    for(f <- 0 until featureLexicon.size) {
      if(featuresToKeep.contains(f)) {
        featureIndexMap += f -> newId
        newId += 1
      }
    }

    // construct the new dataset with the filtered features
    val newFeatures = new ArrayBuffer[Array[Int]]
    val newValues = new ArrayBuffer[Array[Double]]
    for(row <- features.indices) {
      val (nfs, nvs) = keepOnlyRow(features(row), values(row), featureIndexMap)
      newFeatures += nfs
      newValues += nvs
    }

    new RVFDataset[L, F](labelLexicon, featureLexicon.mapIndicesTo(featureIndexMap.toMap), labels, newFeatures, newValues)
  }

  def keepOnlyRow(feats:Array[Int], vals:Array[Double], featureIndexMap:mutable.HashMap[Int, Int]):(Array[Int], Array[Double]) = {
    val newFeats = new ArrayBuffer[Int]()
    val newVals = new ArrayBuffer[Double]()

    for(i <- feats.indices) {
      val f = feats(i)
      val v = vals(i)
      if(featureIndexMap.contains(f)) {
        newFeats += featureIndexMap.get(f).get
        newVals += v
      }
    }

    (newFeats.toArray, newVals.toArray)
  }

  override def toCounterDataset:CounterDataset[L, F] = {
    val cfs = new ArrayBuffer[Counter[Int]]()
    for(d <- features.indices) {
      val cf = new Counter[Int]
      for(i <- features(d).indices) {
        cf.incrementCount(features(d)(i), values(d)(i))
      }
      cfs += cf
    }
    new CounterDataset[L, F](labelLexicon, featureLexicon, labels, cfs)
  }
}

class InformationGain( var datumCount:Int = 0,
                       val datumsByClass:Counter[Int] = new Counter[Int]) {
  def ig(total:InformationGain):Double = {
    var pos = 0.0
    var neg = 0.0
    if(pWith(total) != 0) {
      for (c <- datumsByClass.keySet) {
        val p = datumsByClass.getCount(c) / datumCount.toDouble
        pos += p * math.log(p)
      }
      pos *= pWith(total)
    }
    if(pWithout(total) != 0) {
      for(c <- total.datumsByClass.keySet) {
        val p = (total.datumsByClass.getCount(c) - datumsByClass.getCount(c)) / (total.datumCount - datumCount).toDouble
        neg += p * math.log(p)
      }
      neg *= pWithout(total)
    }
    pos + neg
  }

  def pWith(total:InformationGain) = datumCount.toDouble / total.datumCount.toDouble
  def pWithout(total:InformationGain) = (total.datumCount - datumCount).toDouble / total.datumCount.toDouble
}


/**
  * Dataset that represents datums as explicit counters
  * This is more efficient for the training of various algorithms such as random forests
  */
class CounterDataset[L, F](ll:Lexicon[L],
                           fl:Lexicon[F],
                           ls:ArrayBuffer[Int],
                           val features:ArrayBuffer[Counter[Int]]) extends Dataset[L, F](ll, fl, ls) {
  override def +=(datum: Datum[L, F]): Unit = {
    labels += labelLexicon.add(datum.label)
    val fvs = toIndexCounter(datum.featuresCounter)
    features += fvs
  }

  private def toIndexCounter(feats:Counter[F]):Counter[Int] = {
    val c = new Counter[Int]
    for(f <- feats.keySet) {
      assert(featureLexicon.contains(f))
      c.incrementCount(featureLexicon.get(f).get, feats.getCount(f))
    }
    c
  }

  private def toFeatureCounter(c:Counter[Int]):Counter[F] = {
    val feats = new Counter[F]
    for(f <- c.keySet) {
      assert(f < featureLexicon.size)
      feats.incrementCount(featureLexicon.get(f), c.getCount(f))
    }
    feats
  }

  /**
    * Returns the Datum for given row
    * These datums are always represented as RVFDatums
    */
  override def mkDatum(row: Int): Datum[L, F] = {
    new RVFDatum[L, F](labelLexicon.get(labels(row)), toFeatureCounter(features(row)))
  }

  /** Creates a new dataset keeping only the features in the given set */
  override def keepOnly(featuresToKeep: Set[Int]): Dataset[L, F] = {
    throw new RuntimeException("ERROR: keepOnly not supported yet!")
  }

  /** Removes features that appear less than threshold times in this dataset. */
  override def removeFeaturesByFrequency(threshold: Int): Dataset[L, F] = {
    throw new RuntimeException("ERROR: removeFeaturesByFrequency not supported in CounterDataset yet!")
  }

  override def removeFeaturesByInformationGain(pctToKeep:Double):Dataset[L, F] = {
    throw new RuntimeException("removeFeaturesByInformationGain not supported in CounterDataset yet!")
  }

  override def featuresCounter(datumOffset: Int): Counter[Int] = features(datumOffset)

  def toCounterDataset:CounterDataset[L, F] = this
}