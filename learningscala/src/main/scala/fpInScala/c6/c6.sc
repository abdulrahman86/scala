import fpInScala.c6.{RNG, SimpleRNG}

RNG.nonNegativeInt(SimpleRNG(System.currentTimeMillis()))

RNG.double(SimpleRNG(System.currentTimeMillis()))

RNG.double3(SimpleRNG(System.currentTimeMillis()))

RNG.ints(5)(SimpleRNG(System.currentTimeMillis()))

RNG.ints2(5)(SimpleRNG(System.currentTimeMillis()))

RNG.both2(RNG.int, RNG.double2)(SimpleRNG(System.currentTimeMillis()))

