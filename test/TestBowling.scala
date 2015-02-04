import collection.mutable.Stack
import org.scalatest._

class Bowling {
  var pins = List[Int]()
  var index = 0;

  def result(rolls:String):Int = {
    var begin = 0
    var end = 2
    var value = 0
    while( begin < rolls.length()){
      if (end > rolls.length()){
        end = begin + 1
      }
      value += calculateTurnValue(rolls.substring(begin,end))
      println(pins, value)
      begin += 2
      end += 2
      index += 1
    }
    value
  }
  
  def calculateTurnValue(turnRolls:String):Int = {
    if (turnRolls.length() > 1){
      var prev = rollValue(turnRolls.charAt(0),0)
      var turnValue = rollValue(turnRolls.charAt(1),prev)
      if (turnValue > 10){
    	  pins = pins :+ 10
      }else{
        pins = pins :+ turnValue
      }
      turnValue
    }else{
      rollValue(turnRolls.charAt(0),0)
    }
  } 
  
  def rollValue(representation:Char, previous:Int):Int = {
    if (representation == '-') {
      0 + previous
    }else if ( representation == 'X') {
      sumValueAt(sumValueAt(10,index-2),index-1)
    }else if ( representation == '\\') {
      sumValueAt(10,index-1)
    }else{
      representation.toString.toInt + previous
    }
  }
  
  def sumValueAt(value:Int, index:Int):Int = {
    if (index >= 0){
      value + pins(index)
    }else{
      value
    }
  }
}

class TestBowling extends FlatSpec with Matchers {

  "Bowling" should "single throw is equal throw value" in {
    
     new Bowling().result("1") should be (1)
     new Bowling().result("2") should be (2)
  }
  
  "Bowling" should "single strike is equal 10" in {
     new Bowling().result("X") should be (10)
  }
  
  "Bowling" should "single miss is equal zero" in {
     new Bowling().result("-") should be (0)
  }
  
  "Bowling" should "two trhows sums results" in {
     new Bowling().result("23") should be (5)
     new Bowling().result("X-") should be (10)
   }
  
  "Bowling" should "first spare is 10" in {
     new Bowling().result("3\\") should be (10)
  }
  
  "Bowling" should "sums the results from rolls with no memory" in {
	  new Bowling().result("9-9-") should be (18)
    new Bowling().result("9-9-9-9-9-9-9-9-9-9-") should be (90)
  }
  
  "Bowling" should "spare sums last roll value" in {
     new Bowling().result("5-3\\") should be (20)
     new Bowling().result("235-3\\") should be (25)
  }
  
  "Bowling" should "strike sums two last roll value" in {
     new Bowling().result("5-3X") should be (20)
     new Bowling().result("235-3X") should be (30)
  }
  
}
