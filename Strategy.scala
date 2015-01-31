import scala.collection.mutable._

trait Strategy {
  type Symbol = String
  type Direction = String
  type Book = Map[Symbol,Map[Direction,ListBuffer[(Int,Int)]]]
	
  def handleBook(book: Book): Unit
  def handleTrade(sym: Symbol,price: Int, size: Int, book: Book): Unit
  def startRunning(delegate: TradingBot): Unit
  def orderTimeLimit: Int
}