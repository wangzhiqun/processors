package org.clulab.processors.clu.bio

import java.util.regex.Pattern

import org.clulab.processors.Sentence
import org.clulab.processors.clu.SentencePostProcessor
import org.clulab.sequences.LexiconNER
import org.clulab.struct.MutableNumber
import BioNERPostProcessor._
import org.clulab.utils.Files.loadStreamFromClasspath

import scala.collection.mutable

/**
  * Fixes some common NER labeling mistakes in the bio domain (in place)
  * User: mihais
  * Date: 10/11/17
  */
class BioNERPostProcessor(val stopWordFile:String) extends SentencePostProcessor {
  val stopWords: Set[String] = loadEntityStopList(stopWordFile)

  override def process(sent: Sentence): Unit = {
    val seq = sent.entities.get
    val tags = sent.tags.get
    val lemmas = sent.lemmas.get
    val words = sent.words

    //
    // This code *transforms* entity labels, e.g., by shortening entity spans
    //

    //
    // IF: "A ,(optional) and B complex" THEN: make sure "complex" is labeled "O"
    // This pattern is considered a Binding event, and will be modeled by REACH
    //
    var i = 0
    while(i < seq.length) {
      val offset = new MutableNumber[Int](i - 1)
      if(lemmas(i) == "complex" &&
        isEntity(offset, seq) &&
        isCC(offset, tags) &&
        isEntity(offset, seq)) {
        seq(i) = LexiconNER.OUTSIDE_LABEL
        seq(findEntityStart(i - 1, seq) - 1) = LexiconNER.OUTSIDE_LABEL
      }
      i += 1
    }

    //
    // This code *validates* entire entity spans as valid/invalid entity names
    //
    
    i = 0
    while(i < seq.length) {
      if(isEntityStart(i, seq)) {
        val end = findEntityEnd(i, seq)
        if(! validMatch(sent, i, end)) {
          for(j <- i until end) {
            // if no bueno, reset the entire span labels to O
            seq(j) = LexiconNER.OUTSIDE_LABEL
          }
        }
        i = end
      } else {
        i += 1
      }
    }
  }

  private def validMatch(sentence: Sentence, start: Int, end: Int):Boolean = {
    assert(end > start)
    val words = sentence.words
    val lemmas = sentence.lemmas.get
    var verbose = true

    if(verbose) {
      println(s"validMatch for span [$start, $end) with label ${sentence.entities.get(start)}")
      println(s"Tags: ${sentence.tags.get.mkString(", ")}")
    }

    //
    // must contain at least one NN*
    //
    var nouns = 0
    for(i <- start until end)
      if(sentence.tags.get(i).startsWith("NN"))
        nouns += 1
    if(nouns == 0) {
      if(verbose) println("\tFailed noun test")
      return false
    }

    //
    // some entities end with -ing verbs (e.g., "binding")
    // do not accept them when followed by "to"
    //
    if(end < words.length) {
      val last = words(end - 1)
      val to = words(end)
      if(last.length > 3 && last.toLowerCase.endsWith("ing") && to.toLowerCase == "to") {
        if(verbose) println("\tFailed -ing to test")
        return false
      }
    }

    //
    // stop words should not be labeled when in lower case, or upper initial
    //
    if(end - start == 1 &&
       (isLowerCase(words(start)) || isUpperInitial(words(start))) &&
       stopWords.contains(words(start).toLowerCase)) {
      if(verbose) println("\tFailed stop-word test")
      return false
    }

    //
    // XML tag leftovers should not be labeled
    //
    for(i <- start until end) {
      if(words(i).startsWith("XREF_")) {
        if(verbose) println("\tFailed XREF test")
        return false
      }
    }

    //
    // figure references, e.g., "Figure S2", should not be labeled
    //
    for(i <- start until end) {
      if(i > 0 && isFigRef(lemmas, i)) {
        if(verbose) println("\tFailed figure reference test")
        return false
      }
    }

    // TODO: the next two statements repeat from LexiconNER.validEntitySpan(). Can we avoid this repetition?

    //
    // the text must contain at least one letter AND (the letter must be upper case OR the text contains at least 1 digit)
    //
    val (characters, letters, digits, upperCaseLetters, spaces) = LexiconNER.scanText(words, start, end)
    if(letters > 0 && (digits > 0 || upperCaseLetters > 0 || spaces > 0)) {
      if(verbose) println("\tPassed scanText test 1")
      return true
    }

    //
    // if at least 1 letter and length > 3 accept (e.g., "rapamycin")
    //
    if(letters > 0 && characters > LexiconNER.KNOWN_CASE_INSENSITIVE_LENGTH) {
      if(verbose) println("\tPassed scanText test 2")
      return true
    }

    if(verbose) println("\tFailed validMatch default")
    false
  }

  private def isLowerCase(s:String):Boolean = {
    for(i <- 0 until s.length) {
      val c = s.charAt(i)
      if(Character.isLetter(c) && ! Character.isLowerCase(c))
        return false
    }
    true
  }

  private def isUpperInitial(s:String):Boolean = {
    if(s.length < 1) return false
    if(Character.isLetter(s.charAt(0)) && ! Character.isUpperCase(s.charAt(0)))
      return false

    for(i <- 1 until s.length) {
      val c = s.charAt(i)
      if(Character.isLetter(c) && ! Character.isLowerCase(c))
        return false
    }
    true
  }

  private def isFigRef(lemmas:Array[String], offset:Int):Boolean = {
    assert(offset > 0)
    val m1 = POTENTIAL_FIGURE_TEXT.matcher(lemmas(offset - 1))
    val m2 = POTENTIAL_FIGURE_NUMBER.matcher(lemmas(offset))
    if(m1.matches() && m2.matches())
      return true
    false
  }

  private def isEntity(offset:MutableNumber[Int], seq:Array[String]):Boolean = {
    if(offset.value >= 0 && (seq(offset.value).startsWith("B-") || seq(offset.value).startsWith("I-"))) {
      offset.value = findEntityStart(offset.value, seq) - 1
      return true
    }
    false
  }

  private def findEntityStart(offset:Int, seq:Array[String]):Int = {
    var i = offset
    while(i > 0 && seq(i).startsWith("I-"))
      i -= 1
    i
  }

  private def isEntityStart(offset:Int, seq:Array[String]):Boolean = {
    if(seq(offset).startsWith("B-")) return true
    // allow entities to start with "I-", in case the sequence tagger screwed up
    if(seq(offset).startsWith("I-")) return true
    false
  }

  private def findEntityEnd(offset:Int, seq:Array[String]):Int = {
    var i = offset
    if(seq(i).startsWith("B-"))
      i += 1
    while(i < seq.length && seq(i).startsWith("I-"))
      i += 1
    i
  }

  private def isCC(offset:MutableNumber[Int], tags:Array[String]):Boolean = {
    if(offset.value >= 0 && tags(offset.value) == "CC") {
      offset.value -= 1
      if(offset.value >= 0 && tags(offset.value) == ",")
        offset.value -= 1
      return true
    }
    false
  }
}

object BioNERPostProcessor {
  private val POTENTIAL_FIGURE_NUMBER = Pattern.compile("[a-z]*\\d+", Pattern.CASE_INSENSITIVE)
  private val POTENTIAL_FIGURE_TEXT = Pattern.compile("(figure|figures|fig\\.?|figs\\.?)", Pattern.CASE_INSENSITIVE)

  def loadEntityStopList(kb:String):Set[String] = {
    val stops = new mutable.HashSet[String]()
    val reader = loadStreamFromClasspath(kb)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        val l = line.trim
        if(! l.isEmpty && ! l.startsWith("#")) {
          stops += l
        }
      }
    }
    reader.close()
    stops.toSet
  }
}
