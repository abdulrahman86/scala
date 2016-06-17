import fpInScala.c5.{Stream, Lazy}

Lazy.square({println("hello"); 2})

Lazy.if2(true, { Lazy.square(2)}, {"b"})

Stream.toList(Stream.apply(1, 2, 3, 4, 5))

Stream.taken(5)(Stream.apply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

Stream.dropn(5)(Stream.apply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

Stream.toList(Stream.takeWhile[Int](_ < 5)(Stream.apply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))

Stream.forAll[Int](_ <= 5)(Stream.apply(1, 2, 3, 4, 5))

Stream.toList(Stream.takeWhile2[Int](_ <= 3)(Stream.apply(1, 2, 3, 4, 5)))

Stream.headOption[Int](Stream.apply(1, 2, 3, 4))

Stream.toList(Stream.map[Int, Int](_ * 2)(Stream.apply(1, 2, 3, 4, 5)))

Stream.toList(Stream.filter[Int](_ % 2 == 0)(Stream.apply(1, 2, 3, 4, 5, 6)))

Stream.toList(Stream.append[Int](Stream.apply(1, 2))(Stream.apply(3, 4, 5)))

Stream.toList(Stream.flatMap(Stream.apply(Stream.apply(1, 2, 3, 4, 5), Stream.apply(6, 7, 8, 9, 10))))

val ones : Stream[Int] = Stream.constant(1)

Stream.toList(Stream.taken(15)(ones))

val naturals: Stream[Int] = Stream.from(0)

Stream.toList(Stream.taken(5)(naturals))

Stream.toList(Stream.taken(30)(Stream.fibs))

Stream.toList(Stream.taken(10)(Stream.fibs2))


