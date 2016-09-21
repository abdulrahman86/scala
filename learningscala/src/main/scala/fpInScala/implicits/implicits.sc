import fpInScala.implicits.{Implicits1, Test, TestViewBound, TestContextBound}

Test.testViewBound(new TestViewBound)

implicit object ContextBound1 extends Implicits1.ContextBound[TestContextBound] {
  def string1 = "contextBound2"
}

Test.testContextBound(new TestContextBound)