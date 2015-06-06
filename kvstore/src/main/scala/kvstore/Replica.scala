package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  var seqSecondary = 0L;

  val persistence = context.system.actorOf(persistenceProps)
  override val supervisorStrategy = OneForOneStrategy() {
    case _: PersistenceException => Restart
  }
  var persistenceAcks = Map.empty[Long, (ActorRef, Persist)]
  context.setReceiveTimeout(100 millisecond)

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) =>{
      kv += (key -> value)
      sender() ! OperationAck(id)
    }
    case Remove(key, id) =>{
      kv -= key
      sender() ! OperationAck(id)
    }
    case Get(key, id) =>{
      sender() ! GetResult(key, kv.get(key), id)
    }
    case _ =>
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) =>{
      sender() ! GetResult(key, kv.get(key), id)
    }
    case Snapshot(key, valueOption, seq) => {
      if(seq == seqSecondary){
        valueOption match {
          case Some(s) => kv += (key -> s)
          case None => kv -= key
        }
        val persistMsg = Persist(key, valueOption, seq)
        persistenceAcks += seq -> (sender(), persistMsg)
        persistence ! persistMsg
      }
      else if(seq < seqSecondary) {
        sender() ! SnapshotAck(key: String, seq: Long)
        seqSecondary = Math.max(seq + 1, seqSecondary)
      }
    }
    case Persisted(key, id)  => {
      persistenceAcks.get(id) match {
        case Some(ack) => {
          persistenceAcks -= id
          ack._1 ! SnapshotAck(ack._2.key, ack._2.id)
          seqSecondary += 1
        }
        case None => {}
      }
    }
    case ReceiveTimeout => {
      persistenceAcks foreach {case (seq, ack) => persistence ! Persist(ack._2.key, ack._2.valueOption, ack._2.id)}
    }
    case _ =>
  }

}

