import scala.collection.mutable._

class ETF extends Pennying {

	val AMOUNT = 50
	val DELTA = 0
	val WIN = 10
	val THRESH = 100

	override def startRunning(delegate: TradingBot) {
		val t = new Thread{
			override def run {
				while (running){
					Thread.sleep(100)

					var buy_corge = minSell("CORGE")-DELTA
					var buy_foo = minSell("FOO")-DELTA
					var buy_bar = minSell("BAR")-DELTA

					var sell_corge = maxBuy("CORGE")+DELTA
					var sell_foo = maxBuy("FOO")+DELTA
					var sell_bar = maxBuy("BAR")+DELTA

					if ((0.3*sell_foo + 0.8*sell_bar)*AMOUNT - (buy_corge*AMOUNT+100) > WIN) {
          	delegate.convert("CORGE","SELL", AMOUNT)

	          delegate.buy("CORGE", buy_corge, AMOUNT)
	          delegate.sell("FOO", sell_foo, (0.3*AMOUNT).toInt)
	          delegate.sell("BAR", sell_bar, (0.8*AMOUNT).toInt)
	        }
	        if (sell_corge*AMOUNT - ((0.3*buy_foo + 0.8*buy_bar)*AMOUNT+100) > WIN) {
	          delegate.convert("CORGE","BUY", AMOUNT)

	          delegate.sell("CORGE", sell_corge, AMOUNT)
	          delegate.buy("FOO", buy_foo, (0.3*AMOUNT).toInt)
	          delegate.buy("BAR", buy_bar, (0.8*AMOUNT).toInt)
	        }
	        for ((id,(order,time)) <- delegate.openOrders){
	        	order match {
	        		case delegate.Trade(symbol,"BUY",price,size) => {
		            if (price + THRESH < minSell(symbol)) {
		              delegate.cancel(id)
		              delegate.buy(symbol, minSell(symbol) + DELTA, size);
		            }
		          }
			        case delegate.Trade(symbol,"SELL",price,size) => {
		            if (price - THRESH > maxBuy(symbol)) {
		              delegate.cancel(id)
		              delegate.sell(symbol, maxBuy(symbol) - DELTA, size);
		            }
		          }
		          case _ => {}
			      }
	        }
				}
			}
		}
		t.start
	}

  override def orderTimeLimit: Int = 0

}
