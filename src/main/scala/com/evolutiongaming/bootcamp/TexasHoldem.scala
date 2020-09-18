package com.evolutiongaming.bootcamp

import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ListBuffer



object TexasHoldem {
  class rankedHand(val cards: String, val rank: Int, val strength: Double, val combo: Array[String]) {
    override def toString = {
      def joinme(combo: Array[String]): String = {
        combo.mkString("")
      }
      val heya = joinme(combo)
      s"($cards, $rank, $strength, $heya)"
    }
  }

  var omaha = false
  var handSize = 2

  val Ratings = Map(
    "2" -> 1,
    "3" -> 2,
    "4" -> 3,
    "5" -> 4,
    "6" -> 5,
    "7" -> 6,
    "8" -> 7,
    "9" -> 8,
    "T" -> 9,
    "J" -> 10,
    "Q" -> 11,
    "K" -> 12,
    "A" -> 13
  )

  def createRatingSet(Combination: Array[String]): Set[Char] = {
    val ratingList = new ListBuffer[Char]()
    for (card <- Combination)
      ratingList += card.charAt(0)
    ratingList.toSet
  }

  def suitKindsCheck(Combination: Array[String], neededOccurrences: List[Int]): Boolean = {
    val ratingSet = createRatingSet(Combination)
    if (ratingSet.size == neededOccurrences.size) {
      val AllCards = Combination.mkString("")
      val ratingOccurrences = new ListBuffer[Int]()
      ratingSet.foreach(c => ratingOccurrences += AllCards.count(_ == c))
      ratingOccurrences.sorted == neededOccurrences.sorted
    }
    else false
  }

  def isFlush(Combination: Array[String]): Boolean = {
    val AllCards = Combination.mkString("")
    val Hearts = AllCards.count(_ == 'h')
    val Spades = AllCards.count(_ == 's')
    val Diamonds = AllCards.count(_ == 'd')
    val Clubs = AllCards.count(_ == 'c')
    Array(Hearts, Spades, Diamonds, Clubs) contains 5
  }

  def isStraight(Combination: Array[String]): Boolean = {
    val ratingSet = scala.collection.mutable.Set[Int]()
    for (card <- Combination) ratingSet += Ratings(card.charAt(0).toString)
    ratingSet.max - ratingSet.min == 4 && ratingSet.size == 5
  }

  def isStraightFlush(Combination: Array[String]): Boolean = {
    isStraight(Combination) && isFlush(Combination)
  }

  def isFourOfKind(Combination: Array[String]): Boolean = {
    suitKindsCheck(Combination, List(4, 1))
  }

  def isFullHouse(Combination: Array[String]): Boolean = {
    suitKindsCheck(Combination, List(3, 2))
  }

  def isThreeOfKind(Combination: Array[String]): Boolean = {
    suitKindsCheck(Combination, List(3, 1, 1))
  }
  def isTwoPair(Combination: Array[String]): Boolean = {
    suitKindsCheck(Combination, List(2, 2, 1))
  }
  def isOnePair(Combination: Array[String]): Boolean = {
    suitKindsCheck(Combination, List(2, 1, 1, 1))
  }

  def handStrength(Combination: Array[String]): Double = {
    val ratingSet = createRatingSet(Combination)
    val AllCards = Combination.mkString("")
    var ratingOccurrences = 0
    val values = Array.fill(ratingSet.size)(0.0)
    for (rating <- ratingSet) {
      if (AllCards.count(_ == rating) >= ratingOccurrences && Ratings(rating.toString) > values(0) || (ratingSet.size == 5 && Ratings(rating.toString) > values(0))) {
        values(0) = Ratings(rating.toString)
        ratingOccurrences = AllCards.count(_ == rating)
      }
    }
    if (ratingSet.size > 1)
      for (i <- 1 until values.length) {
        ratingOccurrences = 0
        for (rating <- ratingSet)
          if (Ratings(rating.toString) < values(i - 1) && values(i) < Ratings(rating.toString) && AllCards.count(_ == rating) >= ratingOccurrences) {
            ratingOccurrences = AllCards.count(_ == rating)
            values(i) = Ratings(rating.toString) * ratingOccurrences
          }
      }
    var handValue: Double = 0
    var multiplier: Double = 1
    for (i <- values.indices) {
      handValue += values(i) * multiplier
      multiplier *= 0.01
    }
    handValue
  }

  def evaluate(BoardCards: Array[String], Hand: String): rankedHand = {
    val HandCards = splitCards(Hand)
    var Combined = Array[String]()
    var allCombinations = Array[Array[String]]()
    if (omaha) {
      val combinedOmahaHand = HandCards.combinations(2).toArray
      for (i <- combinedOmahaHand.indices) {
        for (j <- BoardCards.indices) {
          for (k <- j to BoardCards.length - 2) {
            val tempBoard = BoardCards.clone()
            tempBoard(j) = combinedOmahaHand(i)(0)
            tempBoard(k + 1) = combinedOmahaHand(i)(1)
            allCombinations ++= Array(tempBoard)
          }
        }
      }
    } else {
      Combined = BoardCards ++ HandCards
      allCombinations = Combined.combinations(5).toArray
    }

    val output = new ListBuffer[rankedHand]()
    for (i <- allCombinations.indices) {
      if ((allCombinations(i): ArraySeq[String]) != (BoardCards: ArraySeq[String])) {
        if (isStraightFlush(allCombinations(i)))
          output += new rankedHand(Hand, 9, handStrength(allCombinations(i)), allCombinations(i))
        else if (isFourOfKind(allCombinations(i)))
          output += new rankedHand(Hand, 8, handStrength(allCombinations(i)), allCombinations(i))
        else if (isFullHouse(allCombinations(i)))
          output += new rankedHand(Hand, 7, handStrength(allCombinations(i)), allCombinations(i))
        else if (isFlush(allCombinations(i)))
          output += new rankedHand(Hand, 6, handStrength(allCombinations(i)), allCombinations(i))
        else if (isStraight(allCombinations(i)))
          output += new rankedHand(Hand, 5, handStrength(allCombinations(i)), allCombinations(i))
        else if (isThreeOfKind(allCombinations(i)))
          output += new rankedHand(Hand, 4, handStrength(allCombinations(i)), allCombinations(i))
        else if (isTwoPair(allCombinations(i)))
          output += new rankedHand(Hand, 3, handStrength(allCombinations(i)), allCombinations(i))
        else if (isOnePair(allCombinations(i)))
          output += new rankedHand(Hand, 2, handStrength(allCombinations(i)), allCombinations(i))
        else
          output += new rankedHand(Hand, 1, handStrength(allCombinations(i)), allCombinations(i))
      }
    }
    output.maxBy(el => (el.rank, el.strength))
  }

  def splitCards(Cards: String): Array[String] = {
    Cards.split("(?<=\\G..)")
  }

  def holdem(Table: String): Unit = {
    val Board = Table.split(" ").head
    val Hands = Table.split(" ").drop(1)
    val BoardCards = splitCards(Board)
    var bestHands = new ListBuffer[rankedHand]
    for (hand <- Hands)
      bestHands += evaluate(BoardCards: Array[String], hand: String)
    bestHands = bestHands.sortBy(hand => (hand.rank, hand.strength, hand.cards))
    var output = ""
    for (i <- bestHands.indices) {
      if (i != bestHands.length-1) {
        if (bestHands(i).rank == bestHands(i + 1).rank && bestHands(i).strength == bestHands(i + 1).strength)
          output += bestHands(i).cards + "="
        else output += bestHands(i).cards + " "
      }
      else output += bestHands(i).cards
    }
    println(output)
  }

  def isValid(Table: String): Boolean = {
    val Board = Table.split(" ").head
    val Hands = Table.split(" ").drop(1)
    val regex = "^([23456789TJQKA][hsdc])*$"
    if (Table.contains("  ")) {
      println("Table contains 2 or more whitespaces")
      return false
    }
    if (Board.length > 10) {
      println("Board is invalid, it has more than 5 cards")
      return false
    }
    if (Board.length < 10) {
      println("Board is invalid, it has less than 5 cards")
      return false
    }
    if (!Board.matches(regex)) {
      println("Board is invalid, it has invalid cards")
      return false
    }
    val validHands = Hands
      .withFilter(x => x.length == handSize * 2 && x.matches(regex))
      .map(x => x).toSet
    if (!Hands.forall(validHands)) {
      print("Following hands are invalid: ")
      for (hand <- Hands.filterNot(validHands))
        if (!hand.matches(regex)) print(hand + " (has invalid cards) ")
        else if (hand.length < handSize * 2) print(hand + s" (has less than $handSize cards) ")
        else if (hand.length > handSize * 2) print(hand + s" (has more than $handSize cards) ")
      return false
    }
    var allCards = splitCards(Board)
    Hands.foreach(hand => allCards = allCards ++ splitCards(hand))
    if (allCards.toSet.size != allCards.length) {
      print("There are duplicates of the cards on the table: ")
      allCards.diff(allCards.distinct).distinct.foreach(hand => print(hand + " "))
      return false
    }
    true
  }

  def main(args: Array[String]) {
    if (args.contains("--omaha")) {
      omaha = true
      handSize = 4
    }
//    val Tables = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != "").mkString("\n").split("\n")
    for (table <- io.Source.stdin.getLines)
      if (isValid(table))
        holdem(table)
  }
}
