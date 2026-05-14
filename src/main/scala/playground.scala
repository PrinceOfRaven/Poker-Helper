import scala.annotation.tailrec

object playground:

  private var usedCards: List[card] = List()
  private var notUsedCards: List[card] = List()
  private var enemyCards: List[card] = List()

  def generateFullDeck(): List[card] = {
    val suits = List("ч", "т", "б", "п")
    val ranks = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)

    for {
      suit <- suits
      rank <- ranks
    } yield card(suit, rank)
  }

//region Работа с картами
  def evaluateHand(cards: List[card]): (String, Int, List[Int]) = {
    val digits = cards.map(_.getDigit)
    val uniqueDigits = digits.groupBy(identity).view.mapValues(_.size).toMap
    val frequencies = uniqueDigits.values.toList.sorted.reverse

    val flushCards = getFlushCards(cards)
    val isFlush = flushCards.nonEmpty

    if (isFlush && hasStraight(cards)) {
      val straightHigh = getStraightHigh(cards)
      if (straightHigh == 14) return ("Флеш-рояль", 10, List())
      return ("Стрит-флеш", 9, List(straightHigh))
    }

    frequencies match {
      case 4 :: _ => return ("Каре",8, uniqueDigits.keys.toList.sorted.reverse)
      case 3 :: 2 :: _ =>
        val trioRank = uniqueDigits.find(_._2 == 3).get._1
        val pairRank = uniqueDigits.find(_._2 == 2).get._1
        return ("Фулл-хаус", 7, List(trioRank, pairRank))
      case _ =>
    }

    if (isFlush) return ("Флеш",6, digits.sorted.reverse)
    if (hasStraight(cards)) return ("Стрит",5, List(getStraightHigh(cards)))

    frequencies match {
      case 3 :: _ =>
        val trioRank = uniqueDigits.find(_._2 == 3).get._1
        val kickers = uniqueDigits.collect {case (card,count) if count != 3 => card}.toList.sorted.reverse
        val allCards = trioRank :: kickers
        ("Сет", 4, allCards)
      case 2 :: 2 :: _ =>
        val pairs = uniqueDigits.filter(_._2 == 2).keys.toList.sorted.reverse
        val kicker = uniqueDigits.find(_._2 == 1).get._1
        val allCards = pairs :+ kicker
        ("Две пары", 3, allCards)
      case 2 :: _ =>
        val pairRank = uniqueDigits.find(_._2 == 2).get._1
        val kickers = uniqueDigits.filter(_._2 != 2).keys.toList.sorted.reverse
        val allCards = pairRank :: kickers
        ("Одна пара", 2, allCards)
      case _ => ("Старшая карта",1,digits)
    }
  }

  private def hasStraight(cards: List[card]): Boolean = {
    val sorted = cards.map(_.getDigit).distinct.sorted
    if (sorted.size < 5) return false

    val wheel = List(2, 3, 4, 5, 14).forall(sorted.contains)
    val normalStraight = sorted.sliding(5).exists(window => window.last - window.head == 4)

    wheel || normalStraight
  }

  private def getFlushCards(cards: List[card]): List[card] = {
    cards.groupBy(_.getSuit).values.find(_.size >= 5).getOrElse(List())
  }

  private def getStraightHigh(cards: List[card]): Int = {
    val ranks = cards.map(_.getDigit).distinct.sorted
    if (ranks == List(2, 3, 4, 5, 14)) return 5
    ranks.sliding(5).find(window => window.last - window.head == 4).map(_.last).getOrElse(0)
  }

  @tailrec
  private def compareKickers(l: List[Int], r: List[Int]): String = (l, r) match {
    case (headL :: tailL, headR :: tailR) =>
      if (headL > headR) "Игрок1 выиграл"
      else if (headL < headR) "Игрок2 выиграл"
      else compareKickers(tailL, tailR)
    case _ => "Ничья"
  }

  private def compareHandles(
      handleLeft: (String, Int, List[Int]),
      handleRight: (String, Int, List[Int])): String =
  {
    if (handleLeft._2 > handleRight._2) {
       "Игрок1 выиграл"
    }
    else if ( handleLeft._2 < handleRight._2) {
      "Игрок2 выиграл"
    }
    else {
      compareKickers(handleLeft._3, handleRight._3)
    }
  }

  private def bestHandScoreFrom7(cards: List[card]): (String, Int, List[Int]) = {
    cards.combinations(5).map(evaluateHand).maxBy {
      case (_, weight, kickers) => (weight, kickers)
    }(using Ordering.Tuple2(using Ordering.Int, Ordering.Implicits.seqOrdering[List, Int]))
  }

  private def isBetterKickers(k1: List[Int], k2: List[Int]): Boolean = {
    k1.zip(k2).collectFirst {
      case (a, b) if a != b => a > b
    }.getOrElse(k1.length > k2.length)
  }

//endregion

//region Вероятности и обёртки
  private def getProbability(
                            myCards: List[card],
                            boardCards: List[card],
                            deadCards: List[card],
                            handSize: Int,
                            communityToDraw: Int
                          ): Double = {

    val allCards = generateFullDeck()
    val known = myCards ++ boardCards ++ deadCards
    val remaining = allCards.filterNot(known.contains)

    val needed = handSize + communityToDraw
    if (remaining.size < needed) return 0.0

    var wins = 0.0
    var total = 0

    for (chosen <- combinations.getCombinations(remaining, needed)) {
      val opponentHand = chosen.take(handSize)
      val drawnCommunity = chosen.drop(handSize)

      val fullBoard = boardCards ++ drawnCommunity

      val myBest = bestHandScoreFrom7(myCards ++ fullBoard)
      val oppBest = bestHandScoreFrom7(opponentHand ++ fullBoard)

      compareHandles(myBest, oppBest) match {
        case "Игрок1 выиграл" => wins += 1.0
        case "Ничья" => wins += 0.5
        case _ =>
      }
      total += 1
    }

    if (total == 0) 0.0 else wins / total.toDouble
  }


  def probabilityClassic(my5cards: List[card], deadCards: List[card]): Double = {
    getProbability(my5cards, List(), deadCards, 5, 0)
  }

  def probabilityTexasFlop(my2cards: List[card], flop3: List[card], deadCards: List[card]): Double = {
    getProbability(my2cards, flop3, deadCards, 2, 2)
  }

  def probabilityTexasTurn(my2cards: List[card], board4: List[card], deadCards: List[card]): Double = {
    getProbability(my2cards, board4, deadCards, 2, 1)
  }

  def probabilityTexasRiver(my2cards: List[card], board5: List[card], deadCards: List[card]): Double = {
    getProbability(my2cards, board5, deadCards, 2, 0)
  }
//endregion

//region Угадывание карт соперника

  def guessOpponentCombination(
                                myCards: List[card],
                                boardCards: List[card],
                                deadCards: List[card],
                                handSize: Int,
                                communityToDraw: Int
                              ): String = {

    val allCards = generateFullDeck()
    val known = myCards ++ boardCards ++ deadCards
    val remaining = allCards.filterNot(known.contains)
    val needed = handSize + communityToDraw

    val comboCounts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)

    for (chosen <- combinations.getCombinations(remaining, needed)) {
      val opponentHand = chosen.take(handSize)
      val drawnCommunity = chosen.drop(handSize)
      val fullBoard = boardCards ++ drawnCommunity
      val oppBest = bestHandScoreFrom7(opponentHand ++ fullBoard)
      val (comboName, _, _) = oppBest
      comboCounts(comboName) += 1
    }

    comboCounts.maxBy(_._2)._1
  }
//endregion
end playground



