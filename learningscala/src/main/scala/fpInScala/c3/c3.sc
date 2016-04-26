import fpInScala.c3.{List, BinaryTree, Tree, Functor, Test}

val l1 = List(1, 2, 3)

List.foldRight(List(1, 2, 3), 0)(_ + _)

List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _)

List.reverse(List(1, 2, 3, 4, 5))

List.foldRWithFoldL(List(1, 2, 3, 4, 5), 0)(_ + _)

List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

BinaryTree(3, 4, 1, 7, 9, 33)

Tree.size(BinaryTree(3, 4, 1, 7, 9, 33))

Tree.max(BinaryTree(3, 4, 1, 7, 9, 33))

Tree.depth(BinaryTree(3, 4, 1, 7, 9, 33))

Tree.map(BinaryTree(3, 4, 1, 7, 9, 33))(_ + 5)

Test.test2[Tuple1, Int, String](Tuple1(1))(x => "hello")
