import fpInScala.c3.{List}

val l1 = List(1, 2, 3)

List.foldRight(List(1, 2, 3), 0)(_ + _)

List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _)

List.reverse(List(1, 2, 3, 4, 5))

List.foldRWithFoldL(List(1, 2, 3, 4, 5), 0)(_ + _)

List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

