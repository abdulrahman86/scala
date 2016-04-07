
import week7.Stream.{streamRange, from, toList, take, #::, map}

toList (take (5) (streamRange(1, 1000000)))

List(1, 2, 3, 4)(3)


lazy val ones : week7.Stream[Int] = #:: (1, ones)

lazy val nat : week7.Stream[Int] = #:: (1, map[Int, Int](_ + 1) (nat))

toList(take(10)(nat))