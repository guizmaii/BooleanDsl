package com.guizmaii.experiment.boolean.dsl

sealed trait BooleanDsl
object BooleanDsl {

  final case class Pure(v: () => Boolean)             extends BooleanDsl
  final case class And(x: BooleanDsl, y: BooleanDsl)  extends BooleanDsl
  final case class Nand(x: BooleanDsl, y: BooleanDsl) extends BooleanDsl
  final case class Or(x: BooleanDsl, y: BooleanDsl)   extends BooleanDsl
  final case class Nor(x: BooleanDsl, y: BooleanDsl)  extends BooleanDsl
  final case class Not(x: BooleanDsl)                 extends BooleanDsl

  def interpret(exp: BooleanDsl): Boolean =
    exp match {
      case Pure(v) => v()

      case And(Pure(x), Pure(y))  => x() && y()
      case Nand(Pure(x), Pure(y)) => !(x() && y())
      case Or(Pure(x), Pure(y))   => x() || y()
      case Nor(Pure(x), Pure(y))  => !(x() || y())
      case Not(Pure(x))           => !x()

      case And(Pure(x), And(y, z))    => x() && (interpret(y) && interpret(z))
      case And(Pure(x), Nand(y, z))   => x() && !(interpret(y) && interpret(z))
      case And(Pure(x), Or(y, z))     => x() && (interpret(y) || interpret(z))
      case And(Pure(x), Nor(y, z))    => x() && !(interpret(y) || interpret(z))
      case And(Pure(x), Not(Pure(y))) => x() && !y()
      case And(Pure(x), Not(y))       => x() && !interpret(y)

      case Nand(Pure(x), And(y, z))    => !(x() && (interpret(y) && interpret(z)))
      case Nand(Pure(x), Nand(y, z))   => !(x() && !(interpret(y) && interpret(z)))
      case Nand(Pure(x), Or(y, z))     => !(x() && (interpret(y) || interpret(z)))
      case Nand(Pure(x), Nor(y, z))    => !(x() && !(interpret(y) || interpret(z)))
      case Nand(Pure(x), Not(Pure(y))) => !(x() && !y())
      case Nand(Pure(x), Not(y))       => !(x() && !interpret(y))

      case Or(Pure(x), And(y, z))    => x() || (interpret(y) && interpret(z))
      case Or(Pure(x), Nand(y, z))   => x() || !(interpret(y) && interpret(z))
      case Or(Pure(x), Or(y, z))     => x() || (interpret(y) || interpret(z))
      case Or(Pure(x), Nor(y, z))    => x() || !(interpret(y) || interpret(z))
      case Or(Pure(x), Not(Pure(y))) => x() || !y()
      case Or(Pure(x), Not(y))       => x() || !interpret(y)

      case Nor(Pure(x), And(y, z))    => !(x() || (interpret(y) && interpret(z)))
      case Nor(Pure(x), Nand(y, z))   => !(x() || !(interpret(y) && interpret(z)))
      case Nor(Pure(x), Or(y, z))     => !(x() || (interpret(y) || interpret(z)))
      case Nor(Pure(x), Nor(y, z))    => !(x() || !(interpret(y) || interpret(z)))
      case Nor(Pure(x), Not(Pure(y))) => !(x() || !y())
      case Nor(Pure(x), Not(y))       => !(x() || !interpret(y))

      case And(Not(Pure(x)), And(y, z))    => !x() && (interpret(y) && interpret(z))
      case And(Not(Pure(x)), Nand(y, z))   => !x() && !(interpret(y) && interpret(z))
      case And(Not(Pure(x)), Or(y, z))     => !x() && (interpret(y) || interpret(z))
      case And(Not(Pure(x)), Nor(y, z))    => !x() && !(interpret(y) || interpret(z))
      case And(Not(Pure(x)), Not(Pure(y))) => !x() && !y()
      case And(Not(Pure(x)), Not(y))       => !x() && !interpret(y)

      case Nand(Not(Pure(x)), And(y, z))    => !(!x() && (interpret(y) && interpret(z)))
      case Nand(Not(Pure(x)), Nand(y, z))   => !(!x() && !(interpret(y) && interpret(z)))
      case Nand(Not(Pure(x)), Or(y, z))     => !(!x() && (interpret(y) || interpret(z)))
      case Nand(Not(Pure(x)), Nor(y, z))    => !(!x() && !(interpret(y) || interpret(z)))
      case Nand(Not(Pure(x)), Not(Pure(y))) => !(!x() && !y())
      case Nand(Not(Pure(x)), Not(y))       => !(!x() && !interpret(y))

      case Or(Not(Pure(x)), And(y, z))    => !x() || (interpret(y) && interpret(z))
      case Or(Not(Pure(x)), Nand(y, z))   => !x() || !(interpret(y) && interpret(z))
      case Or(Not(Pure(x)), Or(y, z))     => !x() || (interpret(y) || interpret(z))
      case Or(Not(Pure(x)), Nor(y, z))    => !x() || !(interpret(y) || interpret(z))
      case Or(Not(Pure(x)), Not(Pure(y))) => !x() || !y()
      case Or(Not(Pure(x)), Not(y))       => !x() || !interpret(y)

      case Nor(Not(Pure(x)), And(y, z))    => !(!x() || (interpret(y) && interpret(z)))
      case Nor(Not(Pure(x)), Nand(y, z))   => !(!x() || !(interpret(y) && interpret(z)))
      case Nor(Not(Pure(x)), Or(y, z))     => !(!x() || (interpret(y) || interpret(z)))
      case Nor(Not(Pure(x)), Nor(y, z))    => !(!x() || !(interpret(y) || interpret(z)))
      case Nor(Not(Pure(x)), Not(Pure(y))) => !(!x() || !y())
      case Nor(Not(Pure(x)), Not(y))       => !(!x() || !interpret(y))

      case And(x, y)  => interpret(x) && interpret(y)
      case Nand(x, y) => !(interpret(x) && interpret(y))
      case Or(x, y)   => interpret(x) || interpret(y)
      case Nor(x, y)  => !(interpret(x) || interpret(y))
      case Not(x)     => !interpret(x)
    }

  implicit final class BDslOps(private val exp0: BooleanDsl) extends AnyVal {
    def &&(exp1: BooleanDsl): BooleanDsl = And(exp0, exp1)
    def ||(exp1: BooleanDsl): BooleanDsl = Or(exp0, exp1)

    def not: BooleanDsl =
      exp0 match {
        case x: Pure    => Not(x)
        case And(x, y)  => Nand(x, y)
        case Nand(x, y) => And(x, y)
        case Or(x, y)   => Nor(x, y)
        case Nor(x, y)  => Or(x, y)
        case Not(x)     => x
      }

    def unary_! : BooleanDsl = not
  }
}
