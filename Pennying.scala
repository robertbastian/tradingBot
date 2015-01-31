import scala.collection.mutable._

class Pennying extends Strategy {

	val maxBuy = Map[Symbol,Int](
	  "BAR" -> 0,
	  "BAZ" -> 0,
	  "FOO" -> 0,
	  "QUUX" -> 0,
	  "CORGE" -> 0
	)
	val minSell = Map[Symbol,Int](
		"BAR" -> Int.MaxValue,
	  "BAZ" -> Int.MaxValue,
	  "FOO" -> Int.MaxValue,
	  "QUUX" -> Int.MaxValue,
	  "CORGE" -> Int.MaxValue
	 )
	val pennyingVal = .25

  def handleBook(book: Book): Unit = {
  	  for ((symbol,entry) <- book){
  	  	maxBuy(symbol) = entry("BUY").foldLeft(0)((a: Int, b: (Int, Int)) => Math.max(a,b._1))
  	  	minSell(symbol) = entry("SELL").foldLeft(Int.MaxValue)((a: Int, b: (Int, Int)) => Math.min(a,b._1))
  	  }
  }

	def handleTrade(sym: Symbol, price: Int, size: Int, book: Book): Unit = {
	}

	var running = true

	def startRunning(delegate: TradingBot) {
		val t = new Thread{
			override def run {
				while (running){
					Thread.sleep(5000)
					for ((symbol,_) <- minSell){
						val min = minSell(symbol); val max = maxBuy(symbol)
						if (min != Int.MaxValue && max != 0){
							val diff = min - max
							var buyPrice = (max + diff*pennyingVal/100).toInt
							var sellPrice = (min - diff*pennyingVal/100).toInt
							delegate.sell(symbol,sellPrice,15)
							delegate.buy(symbol,buyPrice,15)
						}
					}
				}
			}
		}
		t.start
	}

	def stopRunning: Unit = running = false

  def orderTimeLimit: Int = 5

}