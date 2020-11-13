package com.guizmaii.experiment.boolean.dsl

sealed trait BooleanDslV1
object BooleanDslV1 {

  final case class Pure(v: () => Boolean)                 extends BooleanDslV1
  final case class And(x: BooleanDslV1, y: BooleanDslV1)  extends BooleanDslV1
  final case class Nand(x: BooleanDslV1, y: BooleanDslV1) extends BooleanDslV1
  final case class Or(x: BooleanDslV1, y: BooleanDslV1)   extends BooleanDslV1
  final case class Nor(x: BooleanDslV1, y: BooleanDslV1)  extends BooleanDslV1
  final case class Not(x: BooleanDslV1)                   extends BooleanDslV1

  def interpret(exp: BooleanDslV1): Boolean =
    exp match {
      case Pure(x)      => x()
      case Not(Pure(x)) => !x()

      case And(Pure(x), Pure(y))           => x() && y()
      case And(Pure(x), Not(Pure(y)))      => x() && !y()
      case And(Not(Pure(x)), Pure(y))      => !x() && y()
      case And(Not(Pure(x)), Not(Pure(y))) => !x() && !y()
      case And(Pure(x), Not(y))            => x() && !interpret(y)
      case And(Not(Pure(x)), Not(y))       => !x() && !interpret(y)
      case And(Not(x), Pure(y))            => !interpret(x) && y()
      case And(Not(x), Not(Pure(y)))       => !interpret(x) && !y()
      case And(Not(x), Not(y))             => !interpret(x) && !interpret(y)

      case And(Pure(x), And(y, z))  => x() && interpret(y) && interpret(z)
      case And(Pure(x), Nand(y, z)) => x() && !(interpret(y) && interpret(z))
      case And(Pure(x), Or(y, z))   => x() && (interpret(y) || interpret(z))
      case And(Pure(x), Nor(y, z))  => x() && !(interpret(y) || interpret(z))

      case And(Not(Pure(x)), And(y, z))  => !x() && (interpret(y) && interpret(z))
      case And(Not(Pure(x)), Nand(y, z)) => !x() && !(interpret(y) && interpret(z))
      case And(Not(Pure(x)), Or(y, z))   => !x() && (interpret(y) || interpret(z))
      case And(Not(Pure(x)), Nor(y, z))  => !x() && !(interpret(y) || interpret(z))

      case Nand(Pure(x), Pure(y))           => !(x() && y())
      case Nand(Pure(x), Not(Pure(y)))      => !(x() && !y())
      case Nand(Not(Pure(x)), Pure(y))      => !(!x() && y())
      case Nand(Not(Pure(x)), Not(Pure(y))) => x() || y()
      case Nand(Pure(x), Not(y))            => !(x() && !interpret(y))
      case Nand(Not(Pure(x)), Not(y))       => x() || interpret(y)
      case Nand(Not(x), Pure(y))            => !(!interpret(x) && y())
      case Nand(Not(x), Not(Pure(y)))       => interpret(x) || y()
      case Nand(Not(x), Not(y))             => interpret(x) || interpret(y)

      case Nand(Pure(x), And(y, z))  => !(x() && (interpret(y) && interpret(z)))
      case Nand(Pure(x), Nand(y, z)) => !(x() && !(interpret(y) && interpret(z)))
      case Nand(Pure(x), Or(y, z))   => !(x() && (interpret(y) || interpret(z)))
      case Nand(Pure(x), Nor(y, z))  => !(x() && !(interpret(y) || interpret(z)))

      case Nand(Not(Pure(x)), And(y, z))  => !(!x() && (interpret(y) && interpret(z)))
      case Nand(Not(Pure(x)), Nand(y, z)) => x() || (interpret(y) && interpret(z))
      case Nand(Not(Pure(x)), Or(y, z))   => !(!x() && (interpret(y) || interpret(z)))
      case Nand(Not(Pure(x)), Nor(y, z))  => x() || interpret(y) || interpret(z)

      case Or(Pure(x), Pure(y))           => x() || y()
      case Or(Pure(x), Not(Pure(y)))      => x() || !y()
      case Or(Not(Pure(x)), Pure(y))      => !x() || y()
      case Or(Not(Pure(x)), Not(Pure(y))) => !x() || !y()
      case Or(Pure(x), Not(y))            => x() || !interpret(y)
      case Or(Not(Pure(x)), Not(y))       => !x() || !interpret(y)
      case Or(Not(x), Pure(y))            => !interpret(x) || y()
      case Or(Not(x), Not(Pure(y)))       => !interpret(x) || !y()
      case Or(Not(x), Not(y))             => !interpret(x) || !interpret(y)

      case Or(Pure(x), And(y, z))  => x() || (interpret(y) && interpret(z))
      case Or(Pure(x), Nand(y, z)) => x() || !(interpret(y) && interpret(z))
      case Or(Pure(x), Or(y, z))   => x() || (interpret(y) || interpret(z))
      case Or(Pure(x), Nor(y, z))  => x() || !(interpret(y) || interpret(z))

      case Or(Not(Pure(x)), And(y, z))  => !x() || (interpret(y) && interpret(z))
      case Or(Not(Pure(x)), Nand(y, z)) => !x() || !(interpret(y) && interpret(z))
      case Or(Not(Pure(x)), Or(y, z))   => !x() || (interpret(y) || interpret(z))
      case Or(Not(Pure(x)), Nor(y, z))  => !x() || !(interpret(y) || interpret(z))

      case Nor(Pure(x), Pure(y))           => !(x() || y())
      case Nor(Pure(x), Not(Pure(y)))      => !(x() || !y())
      case Nor(Not(Pure(x)), Pure(y))      => !(!x() || y())
      case Nor(Not(Pure(x)), Not(Pure(y))) => x() && y()
      case Nor(Pure(x), Not(y))            => !(x() || !interpret(y))
      case Nor(Not(Pure(x)), Not(y))       => x() && interpret(y)
      case Nor(Not(x), Pure(y))            => !(!interpret(x) || y())
      case Nor(Not(x), Not(Pure(y)))       => interpret(x) && y()
      case Nor(Not(x), Not(y))             => interpret(x) && interpret(y)

      case Nor(Pure(x), And(y, z))  => !(x() || (interpret(y) && interpret(z)))
      case Nor(Pure(x), Nand(y, z)) => !(x() || !(interpret(y) && interpret(z)))
      case Nor(Pure(x), Or(y, z))   => !(x() || (interpret(y) || interpret(z)))
      case Nor(Pure(x), Nor(y, z))  => !(x() || !(interpret(y) || interpret(z)))

      case Nor(Not(Pure(x)), And(y, z))  => !(!x() || (interpret(y) && interpret(z)))
      case Nor(Not(Pure(x)), Nand(y, z)) => x() && interpret(y) && interpret(z)
      case Nor(Not(Pure(x)), Or(y, z))   => !(!x() || (interpret(y) || interpret(z)))
      case Nor(Not(Pure(x)), Nor(y, z))  => x() && (interpret(y) || interpret(z))

      case And(x, y)  => interpret(x) && interpret(y)
      case Nand(x, y) => !(interpret(x) && interpret(y))
      case Or(x, y)   => interpret(x) || interpret(y)
      case Nor(x, y)  => !(interpret(x) || interpret(y))
      case Not(x)     => !interpret(x)
    }

  // TODO Jules
  def optimise(exp: BooleanDslV1): BooleanDslV1 =
    exp match {
      case x: Pure => x

      case x @ And(Pure(_), Pure(_))  => x
      case x @ Nand(Pure(_), Pure(_)) => x
      case x @ Or(Pure(_), Pure(_))   => x
      case x @ Nor(Pure(_), Pure(_))  => x
      case x @ Not(Pure(_))           => x

      case And(Not(x: Pure), Not(y: Pure)) => Or(x, y)
      case And(Not(x: Pure), Not(y))       => Or(x, optimise(y))
      case And(Not(x), Not(y: Pure))       => Or(optimise(x), y)
      case And(Not(x), Not(y))             => Or(optimise(x), optimise(y))

      case Nor(Not(x: Pure), Not(y: Pure)) => And(x, y)
      case Nor(Not(x: Pure), Not(y))       => And(x, optimise(y))
      case Nor(Not(x), Not(y: Pure))       => And(optimise(x), y)
      case Nor(Not(x), Not(y))             => And(optimise(x), optimise(y))

      case And(x, y)  => And(optimise(x), optimise(y))
      case Nand(x, y) => Nand(optimise(x), optimise(y))
      case Or(x, y)   => Or(optimise(x), optimise(y))
      case Nor(x, y)  => Nor(optimise(x), optimise(y))
      case Not(x)     => Not(optimise(x))
    }

  implicit final class BDslOps(private val exp0: BooleanDslV1) extends AnyVal {
    def &&(exp1: BooleanDslV1): BooleanDslV1 = And(exp0, exp1)
    def ||(exp1: BooleanDslV1): BooleanDslV1 = Or(exp0, exp1)

    def not: BooleanDslV1 =
      exp0 match {
        case x: Pure    => Not(x)
        case And(x, y)  => Nand(x, y)
        case Nand(x, y) => And(x, y)
        case Or(x, y)   => Nor(x, y)
        case Nor(x, y)  => Or(x, y)
        case Not(x)     => x
      }

    def unary_! : BooleanDslV1 = not
  }
}
