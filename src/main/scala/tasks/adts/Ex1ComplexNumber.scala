package tasks.adts

package u04lab

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    case class Complex(re: Double, im: Double)
    def complex(re: Double, im: Double): Complex = Complex(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re  
      def im(): Double = complex.im
      def sum(other: Complex): Complex = other match
        case Complex(re, im) => Complex(complex.re + re, complex.im + im)

      def subtract(other: Complex): Complex = other match
        case Complex(re, im) => Complex(complex.re - re, complex.im - im)
      
      def asString(): String = complex match
        case Complex(re, im) if re != 0 && im > 0 
        => re.toString().concat(" + ".concat(im.toString().concat("i")))
        case Complex(re, im) if re != 0 && im < 0 
        => complex.re.toString().concat(" - ".concat(Math.abs(im).toString().concat("i")))
        

      
