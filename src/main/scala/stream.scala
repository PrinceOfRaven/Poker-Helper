import scala.io.StdIn.readLine

object stream {

  private val rankNames = Map(
    2->"2", 3->"3", 4->"4", 5->"5", 6->"6", 7->"7", 8->"8", 9->"9",
    10->"10", 11->"В", 12->"Д", 13->"К", 14->"Т"
  )

  private def cardStr(c: card): String = s"${c.getSuit}${rankNames(c.getDigit)}"

  private def handStr(cards: List[card]): String =
    cards.zipWithIndex.map { case (c, i) => s"[${i+1}]${cardStr(c)}" }.mkString(" ")

  // ─── парсер карты ──────────────────────────────────────────────────────────
  // Формат: <масть><ранг>
  // Масть: ч/червы/черва  п/пики/пика  б/бубны/бубна  т/трефы/трефа
  // Ранг:  2..10, В/вальт/валет, Д/дама, К/король, Т/туз  (или числа 11–14)
  // Примеры: ч14  чТ  чТуз  пК  б10  тВ  п7

  private val suitMap: Map[String, String] = Map(
    "ч" -> "ч", "червы" -> "ч", "черва" -> "ч", "червь" -> "ч",
    "п" -> "п", "пики"  -> "п", "пика"  -> "п", "пик"   -> "п",
    "б" -> "б", "бубны" -> "б", "бубна" -> "б", "буби"  -> "б",
    "т" -> "т", "трефы" -> "т", "трефа" -> "т", "треф"  -> "т"
  )

  private val rankMap: Map[String, Int] = Map(
    "2"->2,  "3"->3,  "4"->4,  "5"->5,  "6"->6,
    "7"->7,  "8"->8,  "9"->9,  "10"->10,
    "в"->11, "вальт"->11, "валет"->11,
    "д"->12, "дама"->12,
    "к"->13, "кор"->13, "король"->13,
    "т"->14, "туз"->14,
    "11"->11, "12"->12, "13"->13, "14"->14
  )

  private def parseCard(raw: String): Option[card] = {
    val s = raw.trim.toLowerCase
    if s.length < 2 then return None
    List(5, 4, 3, 2, 1).flatMap { len =>
      if s.length <= len then None
      else
        for {
          suit <- suitMap.get(s.take(len))
          rank <- rankMap.get(s.drop(len))
        } yield card(suit, rank)
    }.headOption
  }

  // ─── ввод ─────────────────────────────────────────────────────────────────

  private def prompt(msg: String): String = { print(msg); readLine().trim }

  private def pause(): Unit = { print("\n  [Enter для продолжения...]"); readLine() }

  private def separator(title: String = ""): Unit =
    if title.isEmpty then println("\n" + "─" * 60)
    else println(s"\n${"─" * 20}  $title  ${"─" * 20}")

  private def readCard(promptStr: String, used: List[card]): card = {
    while true do {
      val raw = prompt(promptStr)
      parseCard(raw) match {
        case None =>
          println("  ✗ Не распознал карту.")
          println("    Формат: <масть><ранг>  Примеры: ч14  чТ  чТуз  пК  б10  тВ  п7")
          println("    Масти: ч=червы  п=пики  б=бубны  т=трефы")
          println("    Ранги: 2..10, В/вальт, Д/дама, К/король, Т/туз  (или 11–14)")
        case Some(c) if used.contains(c) =>
          println(s"  ✗ Карта ${cardStr(c)} уже использована!")
        case Some(c) =>
          return c
      }
    }
    throw new RuntimeException("unreachable")
  }

  private def readCards(n: Int, label: String, used: List[card]): List[card] = {
    println(s"\n  Введите $n карт — $label")
    println("  Формат: <масть><ранг>, по одной. Примеры: ч14  пК  б10  тВ")
    var acc     = List.empty[card]
    var usedAcc = used
    for i <- 1 to n do {
      val c = readCard(s"  Карта $i: ", usedAcc)
      acc     = acc :+ c
      usedAcc = usedAcc :+ c
    }
    acc
  }


  private def pct(d: Double): String = f"${d * 100}%.1f%%"

  private val comboRank = Map(
    "Флеш-рояль"->10, "Стрит-флеш"->9, "Каре"->8,
    "Фулл-хаус"->7,   "Флеш"->6,       "Стрит"->5,
    "Сет"->4,         "Две пары"->3,    "Одна пара"->2, "Старшая карта"->1
  )

  private def compareResultStr(myCombo: String, oppCombo: String,
                               myCards: List[card], oppCards: List[card]): String = {
    val myR  = comboRank.getOrElse(myCombo, 0)
    val oppR = comboRank.getOrElse(oppCombo, 0)
    if myR > oppR then "Команда 1"
    else if myR < oppR then "Команда 2"
    else {
      val diff = myCards.map(_.getDigit).sorted.reverse
        .zip(oppCards.map(_.getDigit).sorted.reverse)
        .dropWhile { case (a, b) => a == b }
      if diff.isEmpty then "Ничья"
      else if diff.head._1 > diff.head._2 then "Команда 1"
      else "Команда 2"
    }
  }

  // ─── КЛАССИЧЕСКИЙ ПОКЕР ───────────────────────────────────────────────────

  private def classicRound(roundNum: Int, firstTeam: Int, usedCards: List[card]): (String, List[card]) = {
    separator(s"Классика — Раунд $roundNum")

    println("\n  📤 Введите карты обеих команд:")
    val myCards  = readCards(5, "Ваша рука (Команда 1)", usedCards)
    val oppCards = readCards(5, "Рука соперника (Команда 2)", usedCards ++ myCards)
    val allUsed  = usedCards ++ myCards ++ oppCards

    println(s"\n  🃏 Ваша рука:  ${handStr(myCards)}  → ${playground.evaluateHand(myCards)._1}")
    println(s"\n  Команда $firstTeam меняет карты ПЕРВОЙ в этом раунде.")

    val (myCardsAfter,  allUsed2) = exchangeCards("Команда 1 (вы)",       myCards,  firstTeam == 1, allUsed)
    val (oppCardsAfter, allUsed3) = exchangeCards("Команда 2 (соперник)", oppCards,  firstTeam == 2, allUsed2)

    separator("Финальный подсчёт")
    val myFinalCombo  = playground.evaluateHand(myCardsAfter)._1
    val oppFinalCombo = playground.evaluateHand(oppCardsAfter)._1
    println(s"\n  Ваша рука:      ${handStr(myCardsAfter)}  → $myFinalCombo")

    val dead    = allUsed3.filterNot(myCardsAfter.contains).filterNot(oppCardsAfter.contains)
    val myProb  = playground.probabilityClassic(myCardsAfter, dead)
    val oppProb = playground.probabilityClassic(oppCardsAfter, dead)
    println(s"  Ваша вероятность победы:      ${pct(myProb)}")
    println(s"  Вероятность победы соперника: ${pct(oppProb)}")

    val guessedCombo = playground.guessOpponentCombination(myCardsAfter, List(), dead, 5, 0)
    println(s"\n  🔮 Предполагаемая комбинация соперника: $guessedCombo")
    println(s"  📢 Озвучьте вашу вероятность (${pct(myProb)}) и предположение: $guessedCombo")
    pause()

    separator("Вскрытие карт")
    println(s"  Ваши карты:      ${handStr(myCardsAfter)}  → $myFinalCombo")
    println(s"  Карты соперника: ${handStr(oppCardsAfter)}  → $oppFinalCombo")

    val result = compareResultStr(myFinalCombo, oppFinalCombo, myCardsAfter, oppCardsAfter)
    println(s"\n  🏆 Результат раунда: $result")

    if guessedCombo == oppFinalCombo       then println("  ⭐ Вы угадали комбинацию соперника! +0.1 балла")
    if result == "Команда 1" && myProb > oppProb then println("  ⭐ Победа + ваша вероятность была выше! +0.1 балла")

    (result, allUsed3)
  }

  private def exchangeCards(team: String, hand: List[card],
                            isFirst: Boolean, usedCards: List[card]): (List[card], List[card]) = {
    separator(s"Замена карт — $team")
    val combo = playground.evaluateHand(hand)._1
    println(s"\n  Рука: ${handStr(hand)}  → $combo")

    val dead = usedCards.filterNot(hand.contains)
    val prob = playground.probabilityClassic(hand, dead)
    println(s"  Вероятность победы с текущей рукой: ${pct(prob)}")

    val count = prompt(s"\n  Сколько карт меняет $team? (0–3): ")
      .toIntOption.getOrElse(0).max(0).min(3)

    if count == 0 then {
      println(s"  $team не меняет карты.")
      return (hand, usedCards)
    }

    val indicesStr = prompt("  Номера карт для замены через пробел (например: 1 3): ")
    val indices    = indicesStr.split("\\s+").flatMap(_.toIntOption)
      .map(_ - 1).filter(i => i >= 0 && i < 5)
      .distinct.take(count).toList

    val kept    = hand.zipWithIndex.filterNot { case (_, i) => indices.contains(i) }.map(_._1)
    var usedAcc = usedCards
    var newCards = List.empty[card]

    println(s"  Введите ${indices.size} новые карты:")
    for j <- 1 to indices.size do {
      val c = readCard(s"  Новая карта $j: ", usedAcc)
      newCards = newCards :+ c
      usedAcc  = usedAcc :+ c
    }

    val newHand = kept ++ newCards
    println(s"\n  Новая рука: ${handStr(newHand)}  → ${playground.evaluateHand(newHand)._1}")
    (newHand, usedAcc)
  }

  // ─── ТЕХАССКИЙ ХОЛДЕМ ─────────────────────────────────────────────────────

  private def texasRound(roundNum: Int, usedCards: List[card]): (String, List[card]) = {
    separator(s"Техасский Холдем — Раунд $roundNum")

    println("\n  📤 Введите карты:")
    val myCards  = readCards(2, "Ваши 2 карты (Команда 1)", usedCards)
    val oppCards = readCards(2, "Карты соперника (Команда 2)", usedCards ++ myCards)
    val flop     = readCards(3, "Флоп — 3 общих карты", usedCards ++ myCards ++ oppCards)

    var board   = flop
    var allUsed = usedCards ++ myCards ++ oppCards ++ flop

    // ── Флоп ──
    separator("Флоп")
    println(s"  Ваши карты: ${handStr(myCards)}")
    println(s"  Борд:       ${handStr(board)}")
    println(s"  Ваша текущая комбинация: ${playground.evaluateHand(myCards ++ board)._1}")

    val deadFlop    = allUsed.filterNot(myCards.contains).filterNot(board.contains)
    val myProbFlop  = playground.probabilityTexasFlop(myCards,  board, deadFlop.filterNot(oppCards.contains))
    val oppProbFlop = playground.probabilityTexasFlop(oppCards, board, deadFlop.filterNot(myCards.contains))
    println(s"\n  Вероятность победы (ваша):      ${pct(myProbFlop)}")
    println(s"  Вероятность победы (соперника): ${pct(oppProbFlop)}")
    println(s"  📢 Озвучьте вашу вероятность: ${pct(myProbFlop)}")
    pause()

    // ── Тёрн ──
    separator("Тёрн")
    val turn = readCards(1, "Тёрн — 4-я карта", allUsed)
    board   = board ++ turn
    allUsed = allUsed ++ turn

    println(s"  Ваши карты: ${handStr(myCards)}")
    println(s"  Борд:       ${handStr(board)}")
    println(s"  Ваша текущая комбинация: ${playground.evaluateHand(myCards ++ board)._1}")

    val deadTurn    = allUsed.filterNot(myCards.contains).filterNot(board.contains)
    val myProbTurn  = playground.probabilityTexasTurn(myCards,  board, deadTurn.filterNot(oppCards.contains))
    val oppProbTurn = playground.probabilityTexasTurn(oppCards, board, deadTurn.filterNot(myCards.contains))
    println(s"\n  Вероятность победы (ваша):      ${pct(myProbTurn)}")
    println(s"  Вероятность победы (соперника): ${pct(oppProbTurn)}")
    println(s"  📢 Озвучьте вашу вероятность: ${pct(myProbTurn)}")
    pause()

    // ── Ривер ──
    separator("Ривер")
    val river = readCards(1, "Ривер — 5-я карта", allUsed)
    board   = board ++ river
    allUsed = allUsed ++ river

    println(s"  Ваши карты: ${handStr(myCards)}")
    println(s"  Борд:       ${handStr(board)}")
    println(s"  Ваша текущая комбинация: ${playground.evaluateHand(myCards ++ board)._1}")

    val deadRiver    = allUsed.filterNot(myCards.contains).filterNot(board.contains)
    val myProbRiver  = playground.probabilityTexasRiver(myCards,  board, deadRiver.filterNot(oppCards.contains))
    val oppProbRiver = playground.probabilityTexasRiver(oppCards, board, deadRiver.filterNot(myCards.contains))
    val guessedCombo = playground.guessOpponentCombination(myCards, board, deadRiver.filterNot(myCards.contains), 2, 0)

    println(s"\n  Вероятность победы (ваша):      ${pct(myProbRiver)}")
    println(s"  Вероятность победы (соперника): ${pct(oppProbRiver)}")
    println(s"  🔮 Предполагаемая комбинация соперника: $guessedCombo")
    println(s"  📢 Озвучьте вашу вероятность (${pct(myProbRiver)}) и предположение: $guessedCombo")
    pause()

    // ── Вскрытие ──
    separator("Вскрытие")
    val myFinalCombo  = playground.evaluateHand(myCards  ++ board)._1
    val oppFinalCombo = playground.evaluateHand(oppCards ++ board)._1
    println(s"  Ваши карты:      ${handStr(myCards)}  + борд ${handStr(board)}  → $myFinalCombo")
    println(s"  Карты соперника: ${handStr(oppCards)}  + борд ${handStr(board)}  → $oppFinalCombo")

    val result = compareResultStr(myFinalCombo, oppFinalCombo, myCards ++ board, oppCards ++ board)
    println(s"\n  🏆 Результат раунда: $result")

    if guessedCombo == oppFinalCombo            then println("  ⭐ Вы угадали комбинацию соперника! +0.1 балла")
    if result == "Команда 1" && myProbRiver > oppProbRiver then println("  ⭐ Победа + ваша вероятность была выше! +0.1 балла")

    (result, allUsed)
  }

  // ─── ИГРА (3 раунда) ──────────────────────────────────────────────────────

  private def playGame(gameType: String): Unit = {
    var score1    = 0
    var score2    = 0
    var usedCards = List.empty[card]
    var firstTeam = 1

    if gameType == "classic" then {
      println("\n  ✊ Сыграйте «Камень-ножницы-бумага» — победитель меняет карты первым в раунде 1.")
      firstTeam = prompt("  Какая команда выиграла К-Н-Б? (1/2): ").toIntOption.getOrElse(1)
    }

    for roundNum <- 1 to 3 do {
      if score1 >= 2 || score2 >= 2 then {
        separator(s"Раунд $roundNum — Раунд чести")
        println("  Победитель уже определён. Раунд чести — по желанию команд.")
        if prompt("  Сыграть? (y/n): ").toLowerCase != "y" then {
          println("  Пропускаем.")
          return
        }
      }

      val (result, newUsed) = gameType match {
        case "classic" =>
          val res = classicRound(roundNum, firstTeam, usedCards)
          firstTeam = if firstTeam == 1 then 2 else 1
          res
        case "texas" => texasRound(roundNum, usedCards)
        case _       => ("Ничья", usedCards)
      }

      usedCards = newUsed
      result match {
        case "Команда 1" => score1 += 1
        case "Команда 2" => score2 += 1
        case _           =>
      }
      println(s"\n  Счёт: Команда 1 — $score1  |  Команда 2 — $score2")
      pause()
    }

    separator("Итог игры")
    if      score1 > score2 then println("  🏆 КОМАНДА 1 ПОБЕДИЛА В ИГРЕ!")
    else if score2 > score1 then println("  🏆 КОМАНДА 2 ПОБЕДИЛА В ИГРЕ!")
    else                         println("  🤝 Ничья в игре!")
  }

  // ─── выбор типа игры ──────────────────────────────────────────────────────

  private def chooseGameType(label: String): String = {
    println(s"\n  Выберите режим для $label:")
    println("    1 — Классический покер")
    println("    2 — Техасский Холдем")
    var choice = ""
    while choice != "1" && choice != "2" do
      choice = prompt("  Ваш выбор (1/2): ")
    if choice == "1" then "classic" else "texas"
  }

  private def gameTypeName(t: String): String =
    if t == "classic" then "Классический покер" else "Техасский Холдем"

  private def printRules(gameType: String): Unit = gameType match {
    case "classic" => println(
      """|
         |  Правила Классики:
         |  • Каждой команде 5 карт
         |  • Можно заменить 1–3 карты (или не менять)
         |  • Кто меняет первым — чередуется каждый раунд (К-Н-Б в начале)
         |  • Перед заменой — озвучить вероятность победы
         |  • После обеих замен — финальная вероятность + угадывание комбо соперника
         |""".stripMargin)
    case "texas" => println(
      """|
         |  Правила Техасского Холдема:
         |  • Каждой команде 2 закрытые карты, 3 общих на флопе
         |  • После флопа — тёрн (+1 карта), затем ривер (+1 карта)
         |  • После каждой карты — пересчёт вероятности
         |  • На ривере — озвучить вероятность + угадать комбо соперника
         |""".stripMargin)
    case _ =>
  }

  def startRound(): Unit = {
    println(
      """|
         |╔══════════════════════════════════════════════════════════╗
         |║                        ДУХ МАШИНЫ                        ║
         |╚══════════════════════════════════════════════════════════╝
         |
         |  Формат: 2 игры по 3 раунда. Тип каждой игры выбирается вручную.
         |  Победитель игры — у кого больше раундов из 3.
         |
         |  Ввод карт: <масть><ранг>
         |    Масти: ч=червы  п=пики  б=бубны  т=трефы
         |    Ранги: 2..10, В/вальт, Д/дама, К/король, Т/туз  (или числа 11–14)
         |    Примеры: ч14  чТ  чТуз  п13  пК  б10  тВ  п7
         |""".stripMargin
    )

    var matchScore1 = 0
    var matchScore2 = 0

    for gameNum <- 1 to 2 do {
      separator(s"ИГРА $gameNum")
      val gType = chooseGameType(s"Игры $gameNum")
      println(s"\n  Играем: ${gameTypeName(gType)}")
      printRules(gType)
      prompt("  Нажмите Enter чтобы начать...")
      playGame(gType)

      val winner = prompt(s"\n  Кто победил в Игре $gameNum — ${gameTypeName(gType)}? (1/2/0=ничья): ")
        .toIntOption.getOrElse(0)
      if winner == 1 then { matchScore1 += 1; println("  +1 победа Команде 1") }
      else if winner == 2 then { matchScore2 += 1; println("  +1 победа Команде 2") }
      println(s"  Счёт матча: Команда 1 — $matchScore1  |  Команда 2 — $matchScore2")
    }

    separator("ИТОГ МАТЧА")
    println(s"  Команда 1: $matchScore1 побед(ы)")
    println(s"  Команда 2: $matchScore2 побед(ы)")

    if matchScore1 > matchScore2 then {
      println("\n  🏆 КОМАНДА 1 ПОБЕДИЛА В МАТЧЕ! (+1 балл за задачу)")
      println("     Команда 2: +0.25 балла")
    } else if matchScore2 > matchScore1 then {
      println("\n  🏆 КОМАНДА 2 ПОБЕДИЛА В МАТЧЕ! (+1 балл за задачу)")
      println("     Команда 1: +0.25 балла")
    } else {
      println("\n  🎯 СЧЁТ 1:1 — Играется дополнительная игра!")
      println("  Преподаватель/ассистент выбирает тип игры.")
      val gTypeExtra = chooseGameType("дополнительной игры")
      println(s"\n  Играем: ${gameTypeName(gTypeExtra)}")
      printRules(gTypeExtra)
      prompt("  Нажмите Enter чтобы начать...")
      playGame(gTypeExtra)
      val extraWinner = prompt("\n  Победитель дополнительной игры? (1/2): ").toIntOption.getOrElse(1)
      if extraWinner == 1 then println("\n  🏆 Команда 1 победила! (+1 балл)  Команда 2: +0.5 балла")
      else                     println("\n  🏆 Команда 2 победила! (+1 балл)  Команда 1: +0.5 балла")
    }

    println("\n  Спасибо за игру! 🃏\n")
  }
}