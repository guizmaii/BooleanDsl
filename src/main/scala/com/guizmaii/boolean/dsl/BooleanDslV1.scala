package com.guizmaii.boolean.dsl

sealed trait BooleanDslV1 {
  def unary_! : BooleanDslV1

  final def &&(that: BooleanDslV1): BooleanDslV1 = BooleanDslV1.And(this, that)
  final def ||(that: BooleanDslV1): BooleanDslV1 = BooleanDslV1.Or(this, that)
  final def not: BooleanDslV1                    = !this
}
object BooleanDslV1       {

  sealed trait Unary                                   extends BooleanDslV1
  private[dsl] final case class Pure(x: () => Boolean) extends Unary {
    override def unary_! : BooleanDslV1 = Not(x)
  }
  private[dsl] final case class Not(x: () => Boolean)  extends Unary {
    override def unary_! : BooleanDslV1 = Pure(x)
  }

  sealed trait Binary                                                  extends BooleanDslV1
  private[dsl] final case class And(x: BooleanDslV1, y: BooleanDslV1)  extends Binary {
    override def unary_! : BooleanDslV1 = Nand(x, y)
  }
  private[dsl] final case class Nand(x: BooleanDslV1, y: BooleanDslV1) extends Binary {
    override def unary_! : BooleanDslV1 = And(x, y)
  }
  private[dsl] final case class Or(x: BooleanDslV1, y: BooleanDslV1)   extends Binary {
    override def unary_! : BooleanDslV1 = Nor(x, y)
  }
  private[dsl] final case class Nor(x: BooleanDslV1, y: BooleanDslV1)  extends Binary {
    override def unary_! : BooleanDslV1 = Or(x, y)
  }

  def pure(x: => Boolean): BooleanDslV1 = Pure(() => x)

  def interpret(exp: BooleanDslV1): Boolean =
    exp match {
      case Pure(x) => x()
      case Not(x)  => !x()

      case And(Pure(x), Pure(y)) => x() && y()
      case And(Pure(x), Not(y))  => x() && !y()
      case And(Not(x), Pure(y))  => !x() && y()
      case And(Not(x), Not(y))   => !x() && !y()

      case And(Pure(x), And(y, z))  => x() && interpret(y) && interpret(z)
      case And(Pure(x), Nand(y, z)) => x() && !(interpret(y) && interpret(z))
      case And(Pure(x), Or(y, z))   => x() && (interpret(y) || interpret(z))
      case And(Pure(x), Nor(y, z))  => x() && !(interpret(y) || interpret(z))

      case And(Not(x), And(y, z))  => !x() && (interpret(y) && interpret(z))
      case And(Not(x), Nand(y, z)) => !x() && !(interpret(y) && interpret(z))
      case And(Not(x), Or(y, z))   => !x() && (interpret(y) || interpret(z))
      case And(Not(x), Nor(y, z))  => !x() && !(interpret(y) || interpret(z))

      case Nand(Pure(x), Pure(y)) => !(x() && y())
      case Nand(Pure(x), Not(y))  => !(x() && !y())
      case Nand(Not(x), Pure(y))  => !(!x() && y())
      case Nand(Not(x), Not(y))   => x() || y()

      case Nand(Pure(x), And(y, z))  => !(x() && (interpret(y) && interpret(z)))
      case Nand(Pure(x), Nand(y, z)) => !(x() && !(interpret(y) && interpret(z)))
      case Nand(Pure(x), Or(y, z))   => !(x() && (interpret(y) || interpret(z)))
      case Nand(Pure(x), Nor(y, z))  => !(x() && !(interpret(y) || interpret(z)))

      case Nand(Not(x), And(y, z))  => !(!x() && (interpret(y) && interpret(z)))
      case Nand(Not(x), Nand(y, z)) => x() || (interpret(y) && interpret(z))
      case Nand(Not(x), Or(y, z))   => !(!x() && (interpret(y) || interpret(z)))
      case Nand(Not(x), Nor(y, z))  => x() || interpret(y) || interpret(z)

      case Or(Pure(x), Pure(y)) => x() || y()
      case Or(Pure(x), Not(y))  => x() || !y()
      case Or(Not(x), Pure(y))  => !x() || y()
      case Or(Not(x), Not(y))   => !x() || !y()

      case Or(Pure(x), And(y, z))  => x() || (interpret(y) && interpret(z))
      case Or(Pure(x), Nand(y, z)) => x() || !(interpret(y) && interpret(z))
      case Or(Pure(x), Or(y, z))   => x() || (interpret(y) || interpret(z))
      case Or(Pure(x), Nor(y, z))  => x() || !(interpret(y) || interpret(z))

      case Or(Not(x), And(y, z))  => !x() || (interpret(y) && interpret(z))
      case Or(Not(x), Nand(y, z)) => !x() || !(interpret(y) && interpret(z))
      case Or(Not(x), Or(y, z))   => !x() || (interpret(y) || interpret(z))
      case Or(Not(x), Nor(y, z))  => !x() || !(interpret(y) || interpret(z))

      case Nor(Pure(x), Pure(y)) => !(x() || y())
      case Nor(Pure(x), Not(y))  => !(x() || !y())
      case Nor(Not(x), Pure(y))  => !(!x() || y())
      case Nor(Not(x), Not(y))   => x() && y()

      case Nor(Pure(x), And(y, z))  => !(x() || (interpret(y) && interpret(z)))
      case Nor(Pure(x), Nand(y, z)) => !(x() || !(interpret(y) && interpret(z)))
      case Nor(Pure(x), Or(y, z))   => !(x() || (interpret(y) || interpret(z)))
      case Nor(Pure(x), Nor(y, z))  => !(x() || !(interpret(y) || interpret(z)))

      case Nor(Not(x), And(y, z))  => !(!x() || (interpret(y) && interpret(z)))
      case Nor(Not(x), Nand(y, z)) => x() && interpret(y) && interpret(z)
      case Nor(Not(x), Or(y, z))   => !(!x() || (interpret(y) || interpret(z)))
      case Nor(Not(x), Nor(y, z))  => x() && (interpret(y) || interpret(z))

      case And(x, y)  => interpret(x) && interpret(y)
      case Nand(x, y) => !(interpret(x) && interpret(y))
      case Or(x, y)   => interpret(x) || interpret(y)
      case Nor(x, y)  => !(interpret(x) || interpret(y))
    }

}
