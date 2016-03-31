package actor

import akka.actor.Actor

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

class HelloActor extends Actor {
  def receive = {
    case "hello" => println("hello back at you")
    case _       => println("huh?")
  }
}

class HelloActor2(myName: String) extends Actor {
  def receive = {
    // (2) changed these println statements
    case "hello" => println("hello from %s".format(myName))
    case _       => println("'huh?', said %s".format(myName))
  }
}

object Main extends App {
  val system = ActorSystem("HelloSystem")
  // default Actor constructor
  val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
  val helloActor2 = system.actorOf(Props(new HelloActor2("Fred")), name = "helloactor2"
  helloActor ! "hello"
  helloActor ! "buenos dias"
  helloActor2 ! "hello"
  helloActor2 ! "buenos dias"
}