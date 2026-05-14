


object combinations {

  def getCombinations(deck: List[card], k: Int): Iterator[List[card]] = {
    if (k < 0 || k > deck.size) return Iterator.empty
    if (k == 0) return Iterator(Nil)


    new Iterator[List[card]] {
      private val indices = Array.range(0, k)
      private var hasNextCombo = true
      private val n = deck.size

      def hasNext: Boolean = hasNextCombo

      def next(): List[card] = {
        if (!hasNext) throw new NoSuchElementException

        val result = indices.map(deck).toList
        var i = k - 1

        while (i >= 0 && indices(i) == i + n - k) i -= 1

        if (i < 0) hasNextCombo = false
        else {
          indices(i) += 1
          for (j <- i + 1 until k) indices(j) = indices(j - 1) + 1
        }
        result
      }
    }
  }
}


