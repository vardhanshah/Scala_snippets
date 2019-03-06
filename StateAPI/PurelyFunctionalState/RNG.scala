package PurelyFunctionalState

/* Random Number Generator
 * Purely Functional Stateful API
 */

trait RNG {
  def nextInt(): (Int,RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

    val nextRNG = SimpleRNG(newSeed)

    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }

}

object Main extends App {

  type Rand[+A] = State[RNG,A]
  val int: Rand[Int] = new Rand[Int](rng => rng.nextInt)
  val nonNegativeInt: Rand[Int] = int.map(i => if (i<0) -i else i)
  val double: Rand[Double] = nonNegativeInt.map(i => i.toDouble/Int.MaxValue)
  val randIntDouble: Rand[(Int,Double)] = nonNegativeInt.map2(double)((_,_))
  val randDoubleInt: Rand[(Double,Int)] = double.map2(nonNegativeInt)((_,_))
  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))
  val double3: Rand[(Double,Double,Double)] = double.flatMap(x => double.flatMap(y => double.map((x,y,_))))

  val ns: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints(x%50)
  } yield xs.map(_ % y)

  val a = SimpleRNG(5894743)

  val b = SimpleRNG(-38924)
  println(int.run(b), nonNegativeInt.run(b))
  println(double.run(a))
  println(randIntDouble.run(b))
  println(ints(6).run(a))
  println(double3.run(b))
  println(ns.run(a))
}