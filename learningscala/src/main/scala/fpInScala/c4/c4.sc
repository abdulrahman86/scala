import fpInScala.c4.{Option, None}

Option.sequence(List(Option.of(1), Option.of(2), Option.of(3)))

Option.sequence(List(Option.of(1), None, Option.of(3)))

Option.sequence2(List(Option.of(1), Option.of(2), Option.of(3)))