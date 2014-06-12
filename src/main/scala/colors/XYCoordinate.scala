package main.scala.colors

import java.awt.Color

class XYCoordinate(val x : Int , val y : Int) {
  
  private var colorSquareSum = 0 
  def getColorSquareSum = colorSquareSum
	
  private var redSum = 0  
  private var greenSum = 0  
  private var blueSum = 0
  
  def getRedSum = redSum  
  def getGreenSum = greenSum  
  def getBlueSum = blueSum
  
  var color : Color = Color.BLACK
  
  @Override
  def equals (that : XYCoordinate) : Boolean = {
    this.x == that.x && this.y == that.y
  }
  
  def getColor : Color = color
  
  def setColor(color : Color, newColor :Boolean) {
    if(!this.color.equals(Color.BLACK) && !newColor){
      throw new IllegalStateException("tried to modify color of pixel already assigned a color")
    }
    this.color = color
  }
  
  def adjustColorCache(newColorNeighbour : Color) {
    val r = newColorNeighbour.getRed()
    val g = newColorNeighbour.getGreen()
    val b = newColorNeighbour.getBlue()
    
    redSum += r
    colorSquareSum += r*r
    
    greenSum += g
    colorSquareSum += g*g
    
    blueSum += b
    colorSquareSum += b*b
  }

}