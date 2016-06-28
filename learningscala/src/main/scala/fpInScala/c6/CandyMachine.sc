import fpInScala.c6.{CandyMachine, Coin, Machine, Turn}

CandyMachine.simulateMachine(List(Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin))(Machine(true, 20, 0))

