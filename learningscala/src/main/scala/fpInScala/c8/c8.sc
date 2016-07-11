import fpInScala.c8._

val smallInt = Gen.choose(-10, 10)
val maxProp = Prop.forAll(SGen.listOf1(smallInt)) {
  ns => {
    val max = ns.max
    !ns.exists(_ > max)
  }
}

Prop.run(maxProp)

val sortedProp = Prop.forAll(SGen.listOf1(smallInt)){
  ns => {
    val sortedNS = ns.sorted
    sortedNS.foldLeft((true, Integer.MIN_VALUE))((prev, elem) => {
      (prev._2 <= elem && prev._1, elem)
    })._1
  }
}

Prop.run(sortedProp)