package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val lw = w.toLowerCase()
    lw.toList.distinct.sortWith((a, b) => a < b).map(c => (c, lw.count(_ == c)))
  }
  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    def sum(occ: Option[(Char, Int)]): Int = occ.head._2 + occ.last._2

    def merge(a: Occurrences, b: Occurrences): Occurrences =
      if (b.isEmpty) a
      else {
        val c = a.union(b)
        val overlap = c.filter(e => c.count(_._1 == e._1) > 1)
        if (overlap.isEmpty) c
        else c.diff(overlap) ++ overlap.map(e => e._1).distinct.map(e => (e, sum(overlap.find(_._1 == e))))
      }

    if (s.isEmpty) Nil
    else merge(wordOccurrences(s.head), sentenceOccurrences(s.tail)).sortWith((a, b) => a._1 < b._1)
  }

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(e => wordOccurrences(e))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val words = dictionaryByOccurrences.get(wordOccurrences(word))
    if (words.isEmpty) Nil
    else words.head
  }

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def subOcc(occ: (Char, Int)): List[Occurrences] = (for(i <- 1 to occ._2) yield List((occ._1, i))).toList

    def subMerge(occ1: Occurrences, occ2: List[Occurrences]): List[Occurrences] =
      if (occ2.isEmpty) List(occ1)
      else subMerge(occ2.head, occ2.tail).::(occ1)

    def mergeList(occ1: List[Occurrences], occ2: List[Occurrences]): List[Occurrences] =
      if (occ2.isEmpty || occ2.forall(_.isEmpty)) occ1 ++ occ2
      else {
        val locc = (for(a <- occ1; b <- occ2  if a.nonEmpty) yield a ++ b) ++ occ2
        subMerge(locc.head, locc.tail)
      }

    if (occurrences.isEmpty) List(Nil)
    else mergeList(subOcc(occurrences.head), combinations(occurrences.tail))
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    if (combinations(x).contains(y)) {
      val res = (for (a <- x; b <- y if a._1 == b._1) yield (a._1, a._2 - b._2)).filter(_._2 > 0)
      (x.filterNot(occ => y.exists(_._1 == occ._1)) ++ res).sortWith((a, b) => a._1 < b._1)
    } else Nil

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def findMatchedWords(source: List[Word], senocc: Occurrences): List[Word] =
      if (source.isEmpty || senocc.isEmpty) Nil
      else source.filter(w => {
          val lw = w.toLowerCase()
          lw.toList.distinct.forall(c => senocc.exists(o => c == o._1 && lw.count(_ == c) <= o._2))
          })

    def mergeWords(word: Word, words: List[Word], senocc: Occurrences): List[Sentence] = {
      val sen = subtract(senocc, wordOccurrences(word))
      if (sen.nonEmpty) {
        val possibleWords = findMatchedWords(words.diff(word), sen)
        val res = for (b <- possibleWords) yield mergeWords(b, possibleWords, sen)
        if(res.nonEmpty && res.exists(_.nonEmpty)) {
          val ress = res.filter(_.nonEmpty)
          mergeSentences(ress.head, ress.tail).map(_.::(word))
        } else Nil
      } else List(List(word))
    }
    
    def mergeSentences(sentences1: List[Sentence], sentences2: List[List[Sentence]]): List[Sentence] =
      if(sentences2.isEmpty) sentences1
      else sentences1 ++ mergeSentences(sentences2.head, sentences2.tail)

    if (sentence.isEmpty) List(Nil)
    else {
      val stOccs = sentenceOccurrences(sentence)
      val allPossibleWords = findMatchedWords(dictionary, stOccs)
      val res = for (a <- allPossibleWords) yield mergeWords(a, allPossibleWords, stOccs)
      if(res.nonEmpty && res.exists(_.nonEmpty)) {
        val ress = res.filter(_.nonEmpty)
        mergeSentences(ress.head, ress.tail)
      } else Nil
    }
  }
}
