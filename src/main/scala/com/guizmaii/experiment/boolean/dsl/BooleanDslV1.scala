package com.guizmaii.experiment.boolean.dsl

sealed trait BooleanDslV1
object BooleanDslV1 {

  sealed trait Unary                      extends BooleanDslV1
  final case class Pure(v: () => Boolean) extends Unary
  final case class Not(x: () => Boolean)  extends Unary

  sealed trait Binary                                     extends BooleanDslV1
  final case class And(x: BooleanDslV1, y: BooleanDslV1)  extends Binary
  final case class Nand(x: BooleanDslV1, y: BooleanDslV1) extends Binary
  final case class Or(x: BooleanDslV1, y: BooleanDslV1)   extends Binary
  final case class Nor(x: BooleanDslV1, y: BooleanDslV1)  extends Binary

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

  // TODO Jules
  def optimise(exp: BooleanDslV1): BooleanDslV1 =
    exp match {
      case x: Unary => x

      case x @ And(_: Unary, _: Unary) => x
      case x @ Or(_: Unary, _: Unary)  => x

      case Nand(x: Not, y: Not)         => Or(x, y)
      case x @ Nand(_: Unary, _: Unary) => x

      case Nor(x: Not, y: Not)         => And(x, y)
      case x @ Nor(_: Unary, _: Unary) => x

      case And(x, y)  => And(optimise(x), optimise(y))
      case Nand(x, y) => Nand(optimise(x), optimise(y))
      case Or(x, y)   => Or(optimise(x), optimise(y))
      case Nor(x, y)  => Nor(optimise(x), optimise(y))
    }

  implicit final class BDslOps(private val exp0: BooleanDslV1) extends AnyVal {
    def &&(exp1: BooleanDslV1): BooleanDslV1 = And(exp0, exp1)
    def ||(exp1: BooleanDslV1): BooleanDslV1 = Or(exp0, exp1)

    def not: BooleanDslV1 =
      exp0 match {
        case Pure(x)    => Not(x)
        case Not(x)     => Pure(x)
        case And(x, y)  => Nand(x, y)
        case Nand(x, y) => And(x, y)
        case Or(x, y)   => Nor(x, y)
        case Nor(x, y)  => Or(x, y)
      }

    def unary_! : BooleanDslV1 = not
  }
}
