import scala.collection.mutable._
import java.net._
import java.io._
import scala.io._

class TradingBot{
  type Symbol = String
  type Direction = String
  type Book = Map[Symbol,Map[Direction,ListBuffer[(Int,Int)]]]
  
  class Order(symbol: Symbol, dir: Direction, size: Int)
  case class Trade(symbol: Symbol, dir: Direction, size: Int, price: Int) extends Order(symbol, dir, size)
  case class Conversion(symbol: Symbol, dir: Direction, size: Int) extends Order(symbol, dir, size)

  var nextOrderId = 0
  var openOrders = Map[Int,(Order,Long)]()
  var queuedOrders = Map[Int,(Order,Long)]()
  var positions = Map[Symbol,Int](
    "BAR" -> 0,
    "BAZ" -> 0,
    "FOO" -> 0,
    "QUUX" -> 0,
    "CORGE" -> 0
  )
  var book: Book = Map(
    "BAR" -> Map("BUY" -> ListBuffer(), "SELL" ->ListBuffer()),
    "BAZ" -> Map("BUY" -> ListBuffer(), "SELL" ->ListBuffer()),
    "FOO" -> Map("BUY" -> ListBuffer(), "SELL" ->ListBuffer()),
    "QUUX" -> Map("BUY" -> ListBuffer(), "SELL" ->ListBuffer()),
    "CORGE" -> Map("BUY" -> ListBuffer(), "SELL" ->ListBuffer())
  )
  var canTrade: Boolean = false
  var cash: Int = 0

  def buy(sym: Symbol, price: Int, size: Int) = add(sym, "BUY",price,size)
  def sell(sym: Symbol, price: Int, size: Int) = add(sym, "SELL",price,size)
  def add(sym: Symbol, dir: Direction, price: Int, size: Int) = {
    out.println("ADD "+nextOrderId+" "+sym+" "+dir+" "+price+" "+size)
    queuedOrders(nextOrderId) = (Trade(sym,dir,price,size),System.currentTimeMillis)
    nextOrderId += 1
  }
  def convert(sym: Symbol, dir: Direction, size: Int) = {
    out.println("CONVERT "+nextOrderId+" "+sym+" "+dir+" "+size)
    queuedOrders(nextOrderId) = (Conversion(sym,dir,size),System.currentTimeMillis)
  }
  def cancel(id: Int){
    out.println("CANCEL "+id)
    queuedOrders.remove(id)
    openOrders.remove(id)
  }

  val host = "10.0.85.231"
  val port = 20000
  val strategy: Strategy = new Pennying()

  val s = new Socket(host, port)
  lazy val in = new BufferedSource(s.getInputStream()).getLines()
  val out = new PrintStream(s.getOutputStream())

  out.println("HELLO SCALA")
  out.flush()

  strategy.startRunning(this)

  var i = 0

  while (in.hasNext){
    var l = in.next
    var line = l.split(" ")

    if (line(0) == "HELLO"){
      println("Let's go!")
      canTrade = line(1) == "OPEN"
      cash = Integer.parseInt(line(2))
      for (i <- 3 until line.size){
        var entry = line(i).split(":")
        positions(entry(0)) = Integer.parseInt(entry(1))
      }
    }
    else if (line(0) == "BOOK"){
      val symbol = line(1)
      book(symbol)("BUY") = ListBuffer()
      book(symbol)("SELL") = ListBuffer()
      var i = 3
      while (line(i) != "SELL"){
        val entry = line(i).split(":")
        val tuple: (Int,Int) = (Integer.parseInt(entry(0)),Integer.parseInt(entry(1)))
        book(symbol)("BUY") += tuple
        i += 1
      }
      i += 1
      while (i < line.size){
        val entry = line(i).split(":")
        val tuple: (Int,Int) = (Integer.parseInt(entry(0)),Integer.parseInt(entry(1)))
        book(symbol)("SELL") += tuple
        i += 1
      }
      strategy.handleBook(book)
    } 
    else if (line(0) == "MARKET_OPEN")
      canTrade = true
    else if (line(0) == "MARKET_CLOSED")
      canTrade = false
    else if (line(0) == "ERROR")
      println(line(1))
    else if (line(0) == "TRADE")
      strategy.handleTrade(line(1),Integer.parseInt(line(2)),Integer.parseInt(line(3)), book)
    else if (line(0) == "ACK"){
      val id = Integer.parseInt(line(1))
      openOrders(id) = queuedOrders.remove(id).get
    }
    else if (line(0) == "REJECT"){
      openOrders.remove(Integer.parseInt(line(1)))
      println("Rejected order %d: %s",line(1),line(2))
    }
    else if (line(0) == "FILL"){
      if (line(3) == "BUY"){
        cash -= Integer.parseInt(line(4))
        positions(line(2)) += Integer.parseInt(line(5))
      }
      else {
        cash += Integer.parseInt(line(4))
        positions(line(2)) -= Integer.parseInt(line(5))
      }
    }
    else if (line(0) == "OUT")
      openOrders.remove(Integer.parseInt(line(1)))
    else 
      println("Unknown line: "+l)

    // On every 30th line
    if (i > 30){
      for ((id,(order,time)) <- openOrders)
        if (time + strategy.orderTimeLimit*1000 < System.currentTimeMillis)
          cancel(id)
      i = 0
    }
    i += 1
  }

  s.close()
}
  
object TradingBot {
  def main(args : Array[String]) : Unit = {
    new TradingBot()
  }
}