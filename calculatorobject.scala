class Calc (n:Int, d:Int){
  require(d!=0)
  private val g = gcd(n.abs,d.abs)
  val numerator = n/g
  val denominator = d/g
  private def gcd(x:Int, y:Int) :Int = {
    if(x==0) y
    else if (x<0) gcd(-x,y)
    else if (y<0) gcd(x,-y)
    else gcd(y%x,x)
  }
  def this(n: Int) = this(n, 1)
  def add (r:Calc): Calc =
    new Calc(numerator * r.denominator + r.numerator*denominator ,
      denominator*r.denominator)
  def add (i: Int): Calc = new Calc(numerator + i * denominator, denominator)
  def subtract (r:Calc) = new Calc(numerator*r.denominator -r.numerator*denominator,denominator*r.denominator)
  def subtract (i: Int): Calc = new Calc(numerator - i * denominator, denominator)
  def multiply (r:Calc) = new Calc(numerator*r.numerator,denominator*r.denominator)
  def multiply (i: Int): Calc = new Calc(numerator * i , denominator)
  def divide (r:Calc) = new Calc(numerator*r.denominator,denominator*r.numerator)
  def divide (i: Int): Calc = new Calc(numerator , denominator * i)
  override def toString = numerator + "/" + denominator
}

object CalculatorObject {
  def main(args: Array[String]): Unit = {
    val a_calc = new Calc(15,14)
    val a_int = new Calc(18)
    val b_calc = new Calc(10,9)
    val b_int = new Calc(17)
    val c_calc = new Calc(12,24)
    val c_int = new Calc(12)
    val d_calc = new Calc(12,24)
    val d_int = new Calc(11)
    val p = a_calc add 5
    println(p)
    val t = a_int add new Calc(5,7)
    println(t)
    val u = b_calc multiply 8
    println(u)
    val q = b_int multiply new Calc(11,9)
    println(q)
    val v = c_calc subtract 10
    println(v)
    val r = c_int subtract new Calc(14,2)
    println(r)
    val s = d_calc divide 53
    println(s)
    val x = d_int divide new Calc(14,2)
    println(x)
  }
}
