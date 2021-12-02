package advent.of.code


import scala.io.Source

object Day2 {
  type X = Int
  type Z = Int
  type Pos = (X, Z)
  val getX: Pos => X = _._1
  val getZ: Pos => Z = _._2
  val multiplyDepthHorizontalPos: Pos => Int = p => getZ(p) * getX(p)

  sealed abstract class Instruction extends Product with Serializable {
    def x: Int
  }

  case class Forward(x: Int) extends Instruction

  case class Down(x: Int) extends Instruction

  case class Up(x: Int) extends Instruction

  trait FinalPos[F[_]] {
    def followInstructions(init: Pos, instructions: Iterator[Instruction]): F[Pos]
  }

  val parse: String => Instruction = l => {
    val tokens = l.split(" ")
    tokens(0) match {
      case "forward" => Forward(tokens(1).toInt)
      case "up" => Up(tokens(1).toInt)
      case "down" => Down(tokens(1).toInt)
    }
  }

  type Id[A] = A

  object Part1 extends FinalPos[Id] {
    private val step: Pos => Instruction => Pos = p => {
      case Forward(x) => (getX(p) + x, getZ(p))
      case Down(x) => (getX(p), getZ(p) + x)
      case Up(x) => (getX(p), getZ(p) - x)
    }

    override def followInstructions(init: (X, Z), instructions: Iterator[Instruction]): Id[Pos] = {
      instructions.foldLeft(init) {
        case (p, ins) => step(p)(ins)
      }
    }
  }

  object Part2 extends FinalPos[Id] {
    type Aim = Int
    type State = (Pos, Aim)
    private val getPos: State => Pos = _._1
    private val getAim: State => Aim = _._2
    private val step: State => Instruction => State = s => {
      case Down(x) => (getPos(s), getAim(s) + x)
      case Up(x) => (getPos(s), getAim(s) - x)
      case Forward(x) =>
        val newX = getX.compose(getPos).apply(s) + x
        val newZ = getZ.compose(getPos).apply(s) + x * getAim(s)
        ((newX, newZ), getAim(s))
    }

    override def followInstructions(init: (X, Z), instructions: Iterator[Instruction]): (X, Z) = {
      val initState = (init, 0)
      val finalState = instructions.foldLeft(initState) { case (s, ins) => {
        step(s)(ins)
      }
      }
      getPos(finalState)
    }
  }

  def main(args: Array[String]): Unit = {
    println(multiplyDepthHorizontalPos(Part1.followInstructions((0, 0), Source.fromResource("day2/input").getLines().map(parse))))
    println(multiplyDepthHorizontalPos(Part2.followInstructions((0, 0), Source.fromResource("day2/input").getLines().map(parse))))
  }

}
