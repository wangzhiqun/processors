package edu.arizona.sista.swirl2

import edu.arizona.sista.embeddings.word2vec.Word2Vec
import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.{Counter, DirectedGraph}

import ArgumentFeatureExtractor._

import scala.StringBuilder
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Creates features for the binary classification of tokens as potential arguments
 * User: mihais
 * Date: 7/14/15
 */
class ArgumentFeatureExtractor(word2vecFile:String) {

  lazy val w2v = new Word2Vec(word2vecFile)

  def addEmbeddingsFeatures(features:Counter[String],
                            prefix:String,
                            sent:Sentence,
                            position:Int): Unit = {
    val embeddings = w2v.getWordVector(sent.words(position))
    if(embeddings.isDefined)
    for(i <- embeddings.get.indices) {
      val fn = prefix + ":" + i.toString
      val fv = embeddings.get(i)
      features.setCount(fn, fv)
    }
  }

  var lemmaCounts:Option[Counter[String]] = None

  def addDepFeatures(features:Counter[String],
                     prefix:String,
                     sent:Sentence,
                     deps:DirectedGraph[String],
                     arg:Int,
                     pred:Int): Unit = {
    val predTag = tagAt(sent, pred, MAX_TAG_SIZE)
    val argTag = tagAt(sent, arg, MAX_TAG_SIZE)
    val argLemma = lemmaAt(sent, arg)
    val predLemma = lemmaAt(sent, pred)
    val before = if(arg < pred) "T" else "F"
    val paths = deps.shortestPathEdges(pred, arg, ignoreDirection = true)
    paths.foreach(path => {
      // path including POS tags
      val pst = pathToString(path, sent, useTags = true, useLemmas = false)

      //println(s"""Sentence: ${sent.words.mkString(" ")}""")
      //println(s"Dependencies:\n$deps")
      //println(s"Path between $pred and $arg: $pst")

      features.incrementCount(s"path$prefix$before-TTAG:$predTag-$pst-$argTag")
      features.incrementCount(s"path$prefix$before-TLEMMA:$predLemma-$pst-$argLemma")
      features.incrementCount(s"path$prefix-TTAG:$predTag-$pst-$argTag")
      features.incrementCount(s"path$prefix-TLEMMA:$predLemma-$pst-$argLemma")


      // path including lemmas along the way
      val psl = pathToString(path, sent, useTags = false, useLemmas = true)
      features.incrementCount(s"path$prefix$before-LTAG:$predTag-$psl-$argTag")
      features.incrementCount(s"path$prefix$before-LLEMMA:$predLemma-$psl-$argLemma")
      features.incrementCount(s"path$prefix-LTAG:$predTag-$psl-$argTag")
      features.incrementCount(s"path$prefix-LLEMMA:$predLemma-$psl-$argLemma")

      // no tags, no lemmas
      val ps = pathToString(path, sent, useTags = false, useLemmas = false)
      features.incrementCount(s"path$prefix$before-TAG:$predTag-$ps-$argTag")
      features.incrementCount(s"path$prefix$before-LEMMA:$predLemma-$ps-$argLemma")
      features.incrementCount(s"path$prefix-TAG:$predTag-$ps-$argTag")
      features.incrementCount(s"path$prefix-LEMMA:$predLemma-$ps-$argLemma")
    })

    for(path <- paths) {
      val dir = path.map(d => s"${d._4}").mkString("")

      if(dir == ">") {
        val outgoing = deps.outgoingEdges(pred).sortBy(_._1)
        val b = new StringBuilder()
        b.append("predchildren:")
        var first = true
        var foundArg = false
        for(o <- outgoing) {
          if(! first) b.append("-")
          b.append(o._2)
          if(o._1 == arg) {
            foundArg = true
            b.append("(A)")
          }
          first = false
        }
        assert(foundArg == true)
        //println(b.toString())
        features.incrementCount(b.toString())
      }

      else if(dir == "<>" || dir == "<") {
        val parent = path.head._1
        val outgoing = deps.outgoingEdges(parent).sortBy(_._1)
        val b = new StringBuilder()
        b.append("parentchildren:")
        var first = true
        var foundArg = false
        var foundPred = false
        if(parent == arg) {
          b.append("(A)")
          foundArg = true
        }
        for(o <- outgoing) {
          if(! first) b.append("-")
          b.append(o._2)
          if(o._1 == arg) {
            foundArg = true
            b.append("(A)")
          }
          if(o._1 == pred) {
            foundPred = true
            b.append("(P)")
          }
          first = false
        }
        assert(foundArg == true)
        assert(foundPred == true)
        //println(b.toString())
        features.incrementCount(b.toString())
      }

      else if(dir == "<<>") {
        val grandParent = path(1)._1
        val parent = path.head._1
        val outgoing = deps.outgoingEdges(grandParent).sortBy(_._1)
        val b = new StringBuilder()
        b.append("grandparentchildren:")
        var first = true
        var foundArg = false
        var foundPred = false
        for(o <- outgoing) {
          if(! first) b.append("-")
          b.append(o._2)
          if(o._1 == arg) {
            foundArg = true
            b.append("(A)")
          }
          if(o._1 == parent) {
            foundPred = true
            b.append("(PP)")
          }
          first = false
        }
        assert(foundArg == true)
        assert(foundPred == true)
        //println(b.toString())
        features.incrementCount(b.toString())
      }
    }
  }

  def pathToString(path:Seq[(Int, Int, String, String)], sent:Sentence, useTags:Boolean, useLemmas:Boolean):String = {
    if(useTags)
      path.map(d => s"${d._3}${d._4}${tagAt(sent, endPoint(d), MAX_TAG_SIZE)}").mkString("-")
    else if(useLemmas)
      path.map(d => s"${d._3}${d._4}${lemmaAt(sent, endPoint(d))}").mkString("-")
    else
      path.map(d => s"${d._3}${d._4}").mkString("-")
  }

  def endPoint(dep:(Int, Int, String, String)):Int = {
    dep._4 match {
      case ">" => dep._2
      case _ => dep._1
    }
  }

  def mkFeatures(sent:Sentence, position:Int, pred:Int, history:ArrayBuffer[(Int, String)]):Counter[String] = {
    val features = new Counter[String]

    val predLemma = lemmaAt(sent, pred)
    val predTag = tagAt(sent, pred, MAX_TAG_SIZE)

    if(position == pred) {
      features.incrementCount(s"same:$predLemma:$predTag")
      features.incrementCount(s"same-lemma:$predLemma")
      return features
    }

    val before: Boolean = position < pred

    if(sent.stanfordBasicDependencies.isDefined)
      addDepFeatures(features, "B", sent, sent.stanfordBasicDependencies.get, position, pred)
    if(sent.stanfordCollapsedDependencies.isDefined)
      addDepFeatures(features, "C", sent, sent.stanfordCollapsedDependencies.get, position, pred)

    // unigrams
    for (i <- Range(-1, 2)) {
      val lemma = lemmaAt(sent, position + i)
      val tag = tagAt(sent, position + i)

      // of lemmas
      features.incrementCount(s"lemma$i-LEMMA:$lemma")
      features.incrementCount(s"lemma$i$before-LEMMA:$lemma")
      features.incrementCount(s"lemma$i-PL-LEMMA:$lemma:$predLemma")
      features.incrementCount(s"lemma$i$before-PL-LEMMA:$lemma:$predLemma")

      // hybrid
      features.incrementCount(s"lemma$i-PT-LEMMA:$lemma:$predTag")
      features.incrementCount(s"lemma$i$before-PT-LEMMA:$lemma:$predTag")
      features.incrementCount(s"lemma$i-PL-TAG:$tag:$predLemma")
      features.incrementCount(s"lemma$i$before-PL-TAG:$tag:$predLemma")

      // of POS tags
      features.incrementCount(s"tag$i-TAG:$tag")
      features.incrementCount(s"tag$i$before-TAG:$tag")
      features.incrementCount(s"tag$i-PT-TAG:$tag:$predTag")
      features.incrementCount(s"tag$i$before-PT-TAG:$tag:$predTag")
    }

    val argTag = tagAt(sent, position)
    if((argTag == "IN" || argTag == "TO") && sent.stanfordBasicDependencies.isDefined) {
      val deps = sent.stanfordBasicDependencies.get
      val outgoing = deps.getOutgoingEdges(position)

      //println(s"Found PREP at $position")
      //println(s"""Words: ${sent.words.mkString(" ")}""")
      //println(s"Deps:\n$deps")

      for(p <- outgoing) {
        val pPos = p._1
        val pDep = p._2

        val pLemma = lemmaAt(sent, pPos)
        val pTag = tagAt(sent, pPos)

        // TODO: add pDep

        // lemmas
        features.incrementCount(s"plemma-LEMMA:$pLemma")
        features.incrementCount(s"plemma$before-LEMMA:$pLemma")
        features.incrementCount(s"plemma-PL-LEMMA:$pLemma:$predLemma")
        features.incrementCount(s"plemma$before-PL-LEMMA:$pLemma:$predLemma")

        // tags
        features.incrementCount(s"ptag-TAG:$pTag")
        features.incrementCount(s"ptag$before-TAG:$pTag")
        features.incrementCount(s"ptag-PT-TAG:$pTag:$predTag")
        features.incrementCount(s"ptag$before-PT-TAG:$pTag:$predTag")

        // hybrid
        features.incrementCount(s"pLemma-PT-LEMMA:$pLemma:$predTag")
        features.incrementCount(s"pLemma$before-PT-LEMMA:$pLemma:$predTag")
        features.incrementCount(s"pLemma-PL-TAG:$pTag:$predLemma")
        features.incrementCount(s"pLemma$before-PL-TAG:$pTag:$predLemma")
      }
    }

    //
    // history features
    // history stores the positive predictions seen to the left of this candidate
    //
    // features.incrementCount(s"history:${mkHistorySeq(history, pred, position)}")

    // addEmbeddingsFeatures(features, "AE", sent, position)

    /*
    val deps = sent.stanfordBasicDependencies.get
    //if("IN|TO".r.findFirstMatchIn(lemmaAt(sent, position)).isDefined) {
      for(dep <- deps.outgoingEdges(position)) {
        val mlemma = lemmaAt(sent, dep._1)
        val mtag = tagAt(sent, dep._1)
        features += s"mlemma:$mlemma"
        features += s"mtag:$mtag"
      }
    //}

    val paths = deps.shortestPathEdges(pred, position, ignoreDirection = true)
    if(paths.nonEmpty) {
      val path = paths.head.toArray
      features += s"path-length:${path.length}"
      //val pathLabels = path.map(_._3).mkString("-")
      //features += s"path-labels:$pathLabels"
      //features += s"path-labels:$predLemma-$pathLabels-${lemmaAt(sent, position)}"
      // println(s"$position\t$pred\t${path.toList}\t$pathLabels")

      val dirPathLabels = path.map(d => s"${d._3}${d._4}").mkString("-")
      features += s"path-labels:$dirPathLabels"
      features += s"path-labels:$predLemma-$dirPathLabels-${lemmaAt(sent, position)}"
      features += s"path-labels:$predTag-$dirPathLabels-${tagAt(sent, position)}"
    } else {
      features += "no-path"
    }

    if (pred == position) {
      features += s"same-token:${tagAt(sent, position)}"
    } else {
      features += s"token-dist:${math.abs(pred - position)}:${pred < position}"
      if(pred < position) features += "before"
      else features += "after"
    }
    */

    filter(features)
  }

  def filter(feats:Counter[String]):Counter[String] = {
    val selectedPrefixes = Set(
      "pathB-TAG",
      "lemma0-PT-LEMMA",
      "same",
      "lemma0-PL-TAG",
      "pathB-LEMMA",
      "pathBT-TTAG",
      "tag0-PT-TAG",
      "lemma1true-PT-LEMMA",
      "pathB-LTAG",
      "pathB-TTAG",
      "grandparentchildren",
      "lemma1true-PL-LEMMA",
      "pathBT-TAG",
      "lemma-1false-PL-LEMMA",
      "lemma0-PL-LEMMA",
      "predchildren",
      "pathBF-TAG",
      "parentchildren",
      "ptag-PT-TAG",
      "tag1true-PT-TAG",
      "same-lemma"
    )
    val features = new Counter[String]()
    for(f <- feats.keySet) {
      if(selectedPrefixes.contains(ArgumentClassifier.prefix(f, ":"))) {
        features.setCount(f, feats.getCount(f))
      }
    }
    features
  }

  def mkHistorySeq(history:ArrayBuffer[(Int, String)], pred:Int, position:Int):String = {
    val f = new StringBuilder

    var first = true
    var predIncluded = false
    for(argIndexAndLabel <- history) {
      if(! first) f.append('+')
      if(! predIncluded && argIndexAndLabel._1 > pred) {
        f.append('P')
        f.append('+')
        predIncluded = true
      }
      f.append(argIndexAndLabel._2) // the label of the preceding arg, represented as an index in the label lexicon
      if(argIndexAndLabel._1 == pred) f.append("=P")
      first = false
    }

    if(! first) f.append('+')
    if(position < pred) {
      f.append("*+P")
    } else if(position == pred) {
      f.append("*=P")
    } else {
      if(! predIncluded) {
        f.append("P+*")
      } else {
        f.append("*")
      }
    }

    // println(s"History: ${f.toString()}")
    f.toString()
  }

  def wordAt(sent:Sentence, position:Int):String = {
    if(position >= 0 && position < sent.size) {
      val w = sent.words(position)
      val l = sent.lemmas.get(position)
      if(lemmaCounts.isDefined) {
        if(lemmaCounts.get.getCount(l) > UNKNOWN_THRESHOLD) {
          w
        } else {
          UNKNOWN_TOKEN
        }
      } else {
        w
      }
    }
    else PADDING
  }
  def lemmaAt(sent:Sentence, position:Int):String = {
    if(position >= 0 && position < sent.size) {
      val l = sent.lemmas.get(position)
      if(lemmaCounts.isDefined) {
        if(lemmaCounts.get.getCount(l) > UNKNOWN_THRESHOLD) {
          l
        } else {
          UNKNOWN_TOKEN
        }
      } else {
        l
      }
    }
    else PADDING
  }
  def tagAt(sent:Sentence, position:Int, maxSize:Int = 0):String = {
    if(position >= 0 && position < sent.size) {
      val t = sent.tags.get(position)
      if(maxSize > 0 && maxSize < t.length) t.substring(0, maxSize)
      else t
    }
    else PADDING
  }
}

object ArgumentFeatureExtractor {
  val PADDING = "##"

  val MAX_TAG_SIZE = 2

  val UNKNOWN_THRESHOLD = 1
  val UNKNOWN_TOKEN = "*u*"
}
