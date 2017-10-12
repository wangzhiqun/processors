package org.clulab.sequences

import java.io.BufferedReader

import org.clulab.struct.HashTrie

import scala.collection.mutable.ArrayBuffer
import LexiconNER._
import org.clulab.processors.Sentence
import org.clulab.utils.Files.loadStreamFromClasspath
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable

/**
  * Lexicon-based NER, which efficiently recognizes entities from large dictionaries
  * Note: This is a cleaned-up version of the old RuleNER.
  * Create a LexiconNER object using LexiconNER.apply() (NOT the c'tor, which is private).
  * Use it by calling the find() method on a single sentence.
  * See org.clulab.processors.TextLexiconNER for usage examples.
  *
  * @param matchers A map of tries to be matched for each given category label
  *                 The order of the matchers is important: it indicates priority during ties (first has higher priority)
  * @param knownCaseInsensitives Set of single-token entity names that can be spelled using lower case, according to the KB(s)
  * @param useLemmasForMatching If true, tokens are matched using lemmas, otherwise using words
  * @param caseInsensitiveMatching If true, tokens are matched case insensitively
  * @param verifySingleTokenLowerCaseEntities If true, allow single-token, lower-case entities in case-insensitive matching only if seen as such in the KBs
  *
  * Author: mihais
  * Created: 5/11/15
  * Modified: 9/27/17 - Clean up from RuleNER into LexiconNER
  */
class LexiconNER private (
  val matchers:Array[(String, HashTrie)],
  val knownCaseInsensitives:Set[String],
  val useLemmasForMatching:Boolean,
  val caseInsensitiveMatching:Boolean,
  val verifySingleTokenLowerCaseEntities:Boolean) {

  /**
    * Matches the lexicons against this sentence
    * @param sentence The input sentence
    * @return An array of BIO notations the store the outcome of the matches
    */
  def find(sentence:Sentence):Array[String] = {
    val seq = findLongestMatch(sentence)
    seq
  }

  protected def getTokens(sentence:Sentence):Array[String] = {
    if (useLemmasForMatching) {
      sentence.lemmas.get
    } else {
      sentence.words
    }
  }

  /**
    * Finds the longest match across all matchers.
    * This means that the longest match is always chosen, even if coming from a matcher with lower priority
    * Only ties are disambiguated according to the order provided in the constructor
    */
  protected def findLongestMatch(sentence:Sentence):Array[String] = {
    val tokens = getTokens(sentence)
    val caseInsensitiveWords = tokens.map(_.toLowerCase)

    var offset = 0
    val labels = new ArrayBuffer[String]()
    while(offset < tokens.length) {
      // stores the spans found by all matchers
      val spans = new Array[Int](matchers.length)

      // attempt to match each category at this offset
      for (i <- matchers.indices) {
        spans(i) = findAt(tokens, caseInsensitiveWords, matchers(i)._2, offset)
        // if(spans(i) > 0) println(s"Offset $offset: Matched span ${spans(i)} for matcher ${matchers(i)._1}")
      }

      // pick the longest match
      // solve ties by preferring earlier (higher priority) matchers
      var bestSpanOffset: Int = -1
      var bestSpan: Int = -1
      for(i <- matchers.indices) {
        if(spans(i) > bestSpan) {
          bestSpanOffset = i
          bestSpan = spans(i)
        }
      }

      // found something!
      // if single-token span in case-insensitive matching, this is a known single-token, lower-case entity
      if(bestSpanOffset != -1 &&
        ( bestSpan > 1 ||
          ! caseInsensitiveMatching ||
          ! verifySingleTokenLowerCaseEntities ||
          knownCaseInsensitives.contains(sentence.words(offset)))) {
        
        assert(bestSpan > 0)
        val label = matchers(bestSpanOffset)._1
        //println(s"MATCHED LABEL $label from $offset to ${offset + bestSpan} (exclusive)!")
        labels += "B-" + label
        for(i <- 1 until bestSpan) {
          labels += "I-" + label
        }
        offset += bestSpan
        //println(s"Will continue matching starting at $offset")
      } else {
        labels += OUTSIDE_LABEL
        offset += 1
      }
    }

    labels.toArray
  }

  protected def findAt(seq:Array[String],
                       caseInsensitiveSeq:Array[String],
                       matcher:HashTrie,
                       offset:Int):Int = {
    val span = if (matcher.caseInsensitive) {
      matcher.findAt(caseInsensitiveSeq, offset)
    } else {
      matcher.findAt(seq, offset)
    }
    span
  }

}

object LexiconNER {
  val logger: Logger = LoggerFactory.getLogger(classOf[LexiconNER])
  val OUTSIDE_LABEL: String = "O"
  val INTERN_STRINGS:Boolean = false

  /**
    * Creates a LexiconNER from a list of KBs
    * Note that file name (minus the extension) for each KB becomes the name of the corresponding category.
    *   For example, /Some/Path/SomeCategory.tsv.gz yields the category name SomeCategory.
    * Each of the KBs must contain one entity name per line
    *
    * @param kbs KBs containing known entity names
    * @param overrideKBs KBs containing override labels for entity names from kbs (necessary for the bio domain)
    * @param lexicalVariationEngine Generates alternative spellings of an entity name (necessary for the bio domain)
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param caseInsensitiveMatching If true, tokens are matched case insensitively
    * @param verifySingleTokenLowerCaseEntities If true, allow single-token, lower-case entities in case-insensitive matching only if seen as such in the KBs
    * @return The new LexiconNER
    */
  def apply(kbs:Seq[String],
            overrideKBs:Option[List[String]],
            lexicalVariationEngine:LexicalVariations,
            useLemmasForMatching:Boolean,
            caseInsensitiveMatching:Boolean,
            verifySingleTokenLowerCaseEntities:Boolean): LexiconNER = {
    logger.info("Beginning to load the KBs for the rule-based bio NER...")
    val matchers = new ArrayBuffer[(String, HashTrie)]
    val knownCaseInsensitives = new mutable.HashSet[String]()

    // load the override KBs first, so they take priority during matching
    // in these KBs, the name of the entity must be the first token, and the label must be the last
    //   (tokenization is performed around TABs here)
    if (overrideKBs.isDefined) {
      overrideKBs.get.foreach(okb => {
        val reader = loadStreamFromClasspath(okb)
        val overrideMatchers = loadOverrideKB(reader, lexicalVariationEngine, caseInsensitiveMatching, knownCaseInsensitives)
        for(name <- overrideMatchers.keySet.toList.sorted) {
          val matcher = overrideMatchers(name)
          logger.info(s"Loaded OVERRIDE matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
          matchers += Tuple2(name, matcher)
        }
        reader.close()
      })
    }

    // load the standard KBs
    for(kb <- kbs) {
      val name = extractKBName(kb)
      val reader = loadStreamFromClasspath(kb)
      val matcher = loadKB(reader, lexicalVariationEngine, caseInsensitiveMatching, knownCaseInsensitives)
      logger.info(s"Loaded matcher for label $name. This matcher contains ${matcher.uniqueStrings.size} unique strings; the size of the first layer is ${matcher.entries.size}.")
      matchers += Tuple2(name, matcher)
      reader.close()
    }

    logger.info("KB loading completed.")
    new LexiconNER(matchers.toArray, knownCaseInsensitives.toSet, useLemmasForMatching, caseInsensitiveMatching, verifySingleTokenLowerCaseEntities)
  }

  /**
    * Creates a LexiconNER from a list of KBs
    * Note that file name (minus the extension) for each KB becomes the name of the corresponding category.
    *   For example, /Some/Path/SomeCategory.tsv.gz yields the category name SomeCategory.
    * Each of the KBs must contain one entity name per line
    *   
    * @param kbs KBs containing known entity names
    * @param useLemmasForMatching If true, we use Sentence.lemmas instead of Sentence.words during matching
    * @param caseInsensitiveMatching If true, tokens are matched case insensitively
    * @param verifySingleTokenLowerCaseEntities If true, allow single-token, lower-case entities in case-insensitive matching only if seen as such in the KBs
    * @return The new LexiconNER
    */
  def apply(kbs:Seq[String],
            useLemmasForMatching:Boolean = false,
            caseInsensitiveMatching:Boolean = false,
            verifySingleTokenLowerCaseEntities:Boolean = true): LexiconNER = {
    apply(kbs, None, new NoLexicalVariations,
      useLemmasForMatching, caseInsensitiveMatching, verifySingleTokenLowerCaseEntities)
  }

  private def loadKB(
    reader:BufferedReader,
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): HashTrie = {
    val matcher = new HashTrie(caseInsensitive = caseInsensitive, internStrings = INTERN_STRINGS)
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addLine(line, matcher, lexicalVariationEngine, caseInsensitive, knownCaseInsensitives)
      }
    }
    matcher
  }

  private def addLine(
    inputLine:String,
    matcher:HashTrie,
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean, 
    knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    
    val line = inputLine.trim
    if(! line.startsWith("#")) {
      val tokens = line.split("\\s+")
      addWithLexicalVariations(tokens, lexicalVariationEngine, matcher)

      // keep track of all lower case ents that are single token (necessary for case-insensitive matching)
      if(tokens.length == 1 && line.toLowerCase == line && caseInsensitive) { 
        knownCaseInsensitives.add(line)
      }
    }
  }

  private def loadOverrideKB(
    reader:BufferedReader,
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Map[String, HashTrie] = {
    val matchers = new mutable.HashMap[String, HashTrie]()
    var done = false
    while(! done) {
      val line = reader.readLine()
      if(line == null) {
        done = true
      } else {
        addOverrideLine(line, matchers, lexicalVariationEngine, caseInsensitive, knownCaseInsensitives)
      }
    }
    matchers.toMap
  }

  private def addOverrideLine(
    inputLine:String,
    matchers:mutable.HashMap[String, HashTrie],
    lexicalVariationEngine:LexicalVariations,
    caseInsensitive:Boolean,
    knownCaseInsensitives:mutable.HashSet[String]): Unit = {
    val line = inputLine.trim
    if(! line.startsWith("#")) { // skip comments starting with #
      val blocks = line.split("\t")
      if (blocks.size >= 2) {
        val entity = blocks.head   // grab the text of the named entity
        val label = blocks.last    // grab the label of the named entity

        val tokens = entity.split("\\s+")
        // keep track of all lower case ents that are single token (necessary for case-insensitive matching)
        if (tokens.length == 1 && line.toLowerCase == line && caseInsensitive) {
          knownCaseInsensitives.add(line)
        }
        val matcher = matchers.getOrElseUpdate(label,
          new HashTrie(caseInsensitive = caseInsensitive, internStrings = INTERN_STRINGS))

        addWithLexicalVariations(tokens, lexicalVariationEngine, matcher)
      }
    }
  }

  private def addWithLexicalVariations(
    tokens:Array[String],
    lexicalVariationEngine:LexicalVariations,
    matcher:HashTrie): Unit = {
    // add the original form
    matcher.add(tokens)

    // add the lexical variations
    for(ts <- lexicalVariationEngine.lexicalVariations(tokens)) {
      matcher.add(ts)
    }
  }

  private def extractKBName(kb:String):String = {
    val slash = kb.lastIndexOf("/")
    val dot = kb.indexOf('.')
    val name = kb.substring(slash + 1, dot)
    name
  }

  /** Merges labels from src into dst, without overlapping any existing labels in dst */
  def mergeLabels(dst:Array[String], src:Array[String]) {
    assert(dst.length == src.length)

    var offset = 0
    while(offset < dst.length) {
      if(src(offset) != OUTSIDE_LABEL) {
        // no overlap allowed
        // if overlap is detected, the corresponding labels in src are discarded
        if(! overlap(dst, src, offset)) {
          dst(offset) = src(offset)
          offset += 1
          while(offset < src.length && src(offset).startsWith("I-")) {
            dst(offset) = src(offset)
            offset += 1
          }
        } else {
          // overlap found; just ignore the labels in src
          offset += 1
          while(offset < src.length && src(offset).startsWith("I-")) {
            offset += 1
          }
        }
      } else {
        // nothing to merge
        offset += 1
      }
    }
  }

  // Used by mergeLabels above
  private def overlap(dst:Array[String], src:Array[String], offset:Int):Boolean = {
    var position = offset
    if(dst(position) != OUTSIDE_LABEL) return true
    position += 1
    while(position < src.length && src(position).startsWith("I-")) {
      if(dst(position) != OUTSIDE_LABEL) return true
      position += 1
    }
    false
  }
}

