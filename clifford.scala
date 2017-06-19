import scala.language.postfixOps


// HEAD


sealed trait Algebra {
  def baseDimension : Int
  def withNewDimension(n : Int) : Algebra
}

case class Real(baseDimension: Int) extends Algebra {
  override def withNewDimension(n: Int) = Real(n)
  override def toString: String = s"\u211D($baseDimension)"
}

case class Quaternion(baseDimension: Int) extends Algebra {
  override def withNewDimension(n: Int) = Quaternion(n)
  override def toString: String = s"\u210D($baseDimension)"
}

case class Complex(baseDimension: Int) extends Algebra {
  override def withNewDimension(n: Int) = Complex(n)
  override def toString: String = s"\u2102($baseDimension)"
}

case class SplitComplex(baseDimension: Int) extends Algebra {
  override def withNewDimension(n: Int) = SplitComplex(n)
  override def toString: String = s"(\u211D($baseDimension)\u2295\u211D($baseDimension))"
}


case class TensorProduct(left : Algebra, right : Algebra) extends Algebra {
  override def toString: String = s"${left.toString}\u2297${right.toString}"
  val baseDimension: Int = left.baseDimension * right.baseDimension
  override def withNewDimension(n: Int) = throw new IllegalStateException("PORCODDDUEEE")
}

case class DirectSum(left : Algebra, right : Algebra) extends Algebra {
  override def toString: String = s"${left.toString}\u2295${right.toString}"
  override def baseDimension: Int = left.baseDimension + right.baseDimension
  override def withNewDimension(n: Int): Algebra = throw new IllegalStateException("PORCODDDUEEE")
}


// BODY


object Clifford {


  def reduceProduct(product : TensorProduct) : Algebra = {
    println(s"reducing $product")
    (product.left,product.right) match {
      case (other, prod : TensorProduct) =>
        reduceProduct(TensorProduct(other,reduceProduct(prod)))
      case (prod : TensorProduct,other) =>
        println("PORCO DIO STRANO")
        reduceProduct(TensorProduct(reduceProduct(prod),other))
      case (Quaternion(m),Quaternion(n))=>
        Real(m*n*4)
      case (Quaternion(m),Complex(n))=>
        Complex(m*n*2)
      case (Complex(m),Quaternion(n))=>
        Complex(m*n*2)
      case (SplitComplex(1), other) =>
        DirectSum(other, other)
      case (l : Real, r) =>
        r.withNewDimension(l.baseDimension*r.baseDimension)
      case (l, r : Real) =>
        l.withNewDimension(l.baseDimension*r.baseDimension)
      case other =>
        product
    }
  }


  def Cl( s : Int , t : Int , acc : Algebra = Real(1)) : Algebra = {
    println(s"Cl($s,$t)\u2297$acc")
    require( s >= 0 && t >= 0, s" s or t are negative: ($s,$t)" )
    (s,t) match{
      case (1,0) =>
        reduceProduct(TensorProduct(Complex(1),acc))
      case (0,1) =>
        reduceProduct(TensorProduct(SplitComplex(1),acc))
      case (2,0) =>
        reduceProduct(TensorProduct(Quaternion(1),acc))
      case (0,2) =>
        reduceProduct(TensorProduct(Real(2),acc))
      case (1,1) =>
        reduceProduct(TensorProduct(Real(2),acc))
      case (0,0) =>
        reduceProduct(TensorProduct(Real(1),acc))
      case (_,0) =>
        Cl(0,s-2,reduceProduct(TensorProduct(Quaternion(1),acc)))
      case (0,_) =>
        Cl(t-2,0,reduceProduct(TensorProduct(Real(2),acc)))
      case (_,_) =>
        Cl(s-1,t-1,reduceProduct(TensorProduct(Real(2),acc)))
    }
  }


  def ClEven(s : Int, t : Int) : Algebra = {
    if (s > 0)
      Cl(s-1,t)
    else if (t > 0)
      Cl(t-1,s)
    else
     throw new IllegalStateException("PORCO DIO TUTTO ZERO O NEGATIVO!")
  }

  def main(args: Array[String]) {
    ClEven(6,6)
  }


}
