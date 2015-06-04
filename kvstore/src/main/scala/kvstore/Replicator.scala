package kvstore

import akka.actor.{ReceiveTimeout, Props, Actor, ActorRef}
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  context.setReceiveTimeout(100 millisecond)
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case replicate@Replicate(key, valueOption, id) => {
      val seq = nextSeq
      acks += seq -> (sender, replicate)
      replica ! Snapshot(key, valueOption, seq)
    }
    case SnapshotAck(key, seq) => {
      acks.get(seq) match {
        case Some(ack) => {
          acks -= seq
          ack._1 ! Replicated(ack._2.key, ack._2.id)
        }
        case None => {}
      }
    }
    case ReceiveTimeout => {
      acks foreach {case (seq, ack) => replica ! Snapshot(ack._2.key, ack._2.valueOption, seq)}
    }
    case _ =>
  }

}
