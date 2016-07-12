package org.clulab.processors

import org.clulab.processors.bionlp.BioNLPProcessor
import org.scalatest.{Matchers, FlatSpec}

/**
  * Tests for proper tokenization in the Bio domain
  * User: mihais
  * Date: 7/6/16
  */
class TestBioNLPTokenizer extends FlatSpec with Matchers {
  val proc:Processor = new BioNLPProcessor()
  val s1 = "Cells were additionally stimulated with 10 ng/ml NRG and cell extracts analyzed for ErbB3 tyrosine phosphorylation"

  "BioNLPProcessor" should "NOT tokenize slashes if they are part of measurement units" in {
    val text = "Cells were additionally stimulated with 10 ng/ml NRG and cell extracts analyzed for ErbB3 tyrosine phosphorylation"
    val doc = proc.mkDocument(text, keepText = false)
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(6) should be ("ng/ml")
  }

  it should """tokenize ERK/MAPK-signaling into "ERK and MAPK signaling"""" in {
    val text = "Ras activates SAF-1 and MAZ activity through YYY/ZZZ-signaling"
    val doc = proc.mkDocument(text, keepText = false)
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(7) should be ("YYY")
    s.words(8) should be ("and")
    s.words(9) should be ("ZZZ")
    s.words(10) should be ("signaling")
  }

  it should "reattach / and tokenize it properly" in {
    var doc = proc.mkDocument("ZZZ-1/YYY-1", keepText = false)
    proc.annotate(doc)

    var s = doc.sentences(0)
    s.words(0) should be ("ZZZ-1")
    s.words(1) should be ("and")
    s.words(2) should be ("YYY-1")

    doc = proc.mkDocument("ERK-1/-2", keepText = false)
    s = doc.sentences(0)
    s.words(0) should be ("ERK-1/-2")

    doc = proc.mkDocument("ERK-1/2", keepText = false)
    s = doc.sentences(0)
    s.words(0) should be ("ERK-1/2")
  }

  it should "NOT tokenize names of protein families around slash" in {
    val doc = proc.mkDocument("EGFR/ERBB or Erk1/3 or MAPK1/MAPK3", keepText = false)
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(0) should be ("EGFR/ERBB")
    s.words(2) should be ("Erk1/3")
    s.words(4) should be ("MAPK1/MAPK3")
  }

  it should "tokenize complex names around slash" in {
    val doc = proc.mkDocument("Highly purified DNA-PKcs, Ku70/Ku80 heterodimer and the two documented XRCC1 binding partners LigIII and DNA polbeta were dot-blotted.")
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(4) should be ("Ku70")
    s.words(5) should be ("and")
    s.words(6) should be ("Ku80")
  }

  it should "tokenize complex names around dash" in {
    val doc = proc.mkDocument("Highly purified DNA-PKcs, Ku70-Ku80 heterodimer and the two documented XRCC1 binding partners LigIII and DNA polbeta were dot-blotted.")
    proc.annotate(doc)

    val s = doc.sentences(0)
    s.words(4) should be ("Ku70")
    s.words(5) should be ("and")
    s.words(6) should be ("Ku80")
  }

  val mutantTest1a = "Hsp90/S595A mutant"
  it should "tokenize protein/family names from substitution mutations" in {
    val doc = proc.mkDocument(mutantTest1a)
    val s = doc.sentences(0)
    s.words(0) should be ("Hsp90")
    s.words(1) should be ("S595A")
  }

  val mutantTest1b = "Hsp90/414delCys mutant"
  it should "tokenize protein/family names from deletion mutations" in {
    val doc = proc.mkDocument(mutantTest1b)
    val s = doc.sentences(0)
    s.words(0) should be ("Hsp90")
    s.words(1) should be ("414delCys")
  }

  val mutantTest1c = "Hsp90/Leu81fs mutant"
  it should "tokenize protein/family names from frameshift mutations" in {
    val doc = proc.mkDocument(mutantTest1c)
    val s = doc.sentences(0)
    s.words(0) should be ("Hsp90")
    s.words(1) should be ("Leu81fs")
  }

  val mutantTest1d = "Hsp90/Ser43Val mutant"
  it should "tokenize protein/family names from long substitution mutations" in {
    val doc = proc.mkDocument(mutantTest1d)
    val s = doc.sentences(0)
    s.words(0) should be ("Hsp90")
    s.words(1) should be ("Ser43Val")
  }

  val mutantTest1e = "Hsp90/W34_A36del mutant"
  it should "tokenize protein/family names from range deletion mutations" in {
    val doc = proc.mkDocument(mutantTest1e)
    val s = doc.sentences(0)
    s.words(0) should be ("Hsp90")
    s.words(1) should be ("W34_A36del")
  }

  val mutantTest2a = "p110-S205D/K224A double mutant"
  it should "NOT tokenize two substitutions separated by slashes" in {
    val doc = proc.mkDocument(mutantTest2a)
    val s = doc.sentences(0)
    s.words(0) should be ("p110")
    s.words(1) should be ("S205D/K224A")
  }

  val mutantTest2b = "p110-S205D/K224A/Y243M triple mutant"
  it should "NOT tokenize three substitutions separated by slashes" in {
    val doc = proc.mkDocument(mutantTest2b)
    val s = doc.sentences(0)
    s.words(0) should be ("p110")
    s.words(1) should be ("S205D/K224A/Y243M")
  }

  val mutantTest3 = "PKD3.S731E.S735E"
  it should "NOT tokenize multiple substitutions separated by periods" in {
    val doc = proc.mkDocument(mutantTest3)
    val s = doc.sentences(0)
    s.words(0) should be ("PKD3")
    s.words(1) should be ("S731E.S735E")
  }

  val mutantTest4 = "H69Y-RNF2 mutant"
  it should "tokenize mutations from following proteins" in {
    val doc = proc.mkDocument(mutantTest4)
    val s = doc.sentences(0)
    s.words(0) should be ("H69Y")
    s.words(1) should be ("RNF2")
  }

  val mutantTest5 = "RNF2 91T>Y mutant"
  it should "NOT tokenize angle-bracket substitution mutations" in {
    val doc = proc.mkDocument(mutantTest5)
    val s = doc.sentences(0)
    s.words(0) should be ("H69Y")
    s.words(1) should be ("91T>Y")
  }
}
