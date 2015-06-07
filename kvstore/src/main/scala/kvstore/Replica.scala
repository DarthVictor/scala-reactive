package kvstore

import akka.actor._
import akka.event.Logging
import kvstore.Arbiter._
import kvstore.Replica.PersistAndReplicate
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

  case class PersistAndReplicate(sender: ActorRef, key: String, valueOption: Option[String], id: Long)
  case class UnreplicatedValue(sender: ActorRef, key: String, valueOption: Option[String], id: Long, var restReplicators: Set[ActorRef], var errorAfter: Long)

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
  val log = Logging(context.system, this)

  val MAX_PERSISTENCE_RETRY = 10;
  val MAX_REPLICATIONS_ROUNDS = 10;

  var kv = Map.empty[String, String]
  // a map from KV key to number of unanswered replicators and rest rounds to wait
  var unreplicatedKeys = Map.empty[Long, UnreplicatedValue]

  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  var seqSecondary = 0L;

  var _repCounter = -1L
  def nextRepSeq = {
    val ret = _repCounter
    _repCounter -= 1
    ret
  }

  val persistence = context.system.actorOf(persistenceProps)
  override val supervisorStrategy = OneForOneStrategy() {
    case _: PersistenceException => Restart
  }
  var persistenceAcks = Map.empty[Long, (ActorRef, Persist, Option[Long])]
  context.setReceiveTimeout(100 millisecond)

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case PersistAndReplicate (sender, key, valueOption, id) => {
      val persistMsg = Persist(key, valueOption, id)
      persistenceAcks += id -> (sender, persistMsg, Some(MAX_PERSISTENCE_RETRY) )
      persistence ! persistMsg

      if(replicators.size > 0){
        unreplicatedKeys += id -> UnreplicatedValue(sender, key, valueOption, id, replicators, MAX_PERSISTENCE_RETRY)
      }
      replicators foreach {
        case replicator => {
          replicator ! Replicate(key, valueOption, id)
        }
      }
    }

    case Insert(key, value, id) =>{
      kv += (key -> value)
      self ! PersistAndReplicate (sender(), key, Some(value), id)
    }
    case Remove(key, id) =>{
      kv -= key
      self ! PersistAndReplicate (sender(), key, None, id)
    }
    case Get(key, id) =>{
      sender() ! GetResult(key, kv.get(key), id)
    }

    case Persisted(key, id)  => {
      persistenceAcks.get(id) match {
        case Some(ack) => {
          persistenceAcks -= id
          seqSecondary += 1
          if(unreplicatedKeys.get(id).isEmpty) ack._1 ! OperationAck(ack._2.id)
        }
        case None => {}
      }
    }

    case Replicated(key, id) => {
      unreplicatedKeys.get(id) match {
        case Some(unreplicatedValue) => {
          unreplicatedValue.restReplicators -= sender()
          if(unreplicatedValue.restReplicators.isEmpty) {
            unreplicatedKeys -= id
            if(persistenceAcks.get(id).isEmpty) {
              if(unreplicatedValue.id >= 0) { // just remove in case of unreplicatedValue from new replica
                unreplicatedValue.sender ! OperationAck(id)
              }
            }
          }
        }
        case None => {}
      }
    }

    case ReceiveTimeout => {
      persistenceAcks = persistenceAcks filter {
        case(seq, ack) => {
          ack._3 match{
            case Some(i) => i > 0
            case None => true
          }
        }
      }
      persistenceAcks = persistenceAcks map {
        case(seq, ack) => {
          ack._3 match{
            case Some(i) => seq -> (ack._1, ack._2, Some(i-1))
            case None => seq -> (ack._1, ack._2, None)
          }
        }
      }
      persistenceAcks foreach {
        case (seq, ack) => {
          ack._3 match{
            case Some(i) =>{
              if( i > 0){
                persistence ! ack._2
              }
              else{
                ack._1 ! OperationFailed(seq)
              }
            }
            case None => {}
          }
        }
      }
      unreplicatedKeys = unreplicatedKeys filter {
        case(id, unreplicatedValue) => unreplicatedValue.errorAfter > 0
      }
      unreplicatedKeys foreach  {
        case(id, unreplicatedValue) => {
          unreplicatedValue.errorAfter -= 1
          if(unreplicatedValue.errorAfter == 0){
            unreplicatedValue.sender ! OperationFailed(id)
          }
        }
      }

    }

    case Replicas(replicasRef) => {
      val newReplicas = (replicasRef -- secondaries.keySet) - self
      val removedReplicas = secondaries.keySet -- replicasRef

      newReplicas foreach {
        case newReplica => {
          val replicator = context.system.actorOf(Replicator.props(newReplica))
          secondaries += newReplica -> replicator
          replicators += replicator

          kv foreach {
            case (key, value) => {

              val id = nextRepSeq
              val valueOption = Some(value)
              replicator ! Replicate(key, valueOption, id)
              unreplicatedKeys += id -> UnreplicatedValue(sender(), key, valueOption, id, Set(replicator), MAX_PERSISTENCE_RETRY)
            }
          }
        }
      }

      removedReplicas foreach {
        case removedReplica => {
          secondaries.get(removedReplica) match {
            case Some(replicator) => {
              context.stop(replicator)
              unreplicatedKeys foreach {
                case (key, unreplicatedValue) => {
                  unreplicatedValue.restReplicators -= replicator
                  if(unreplicatedValue.restReplicators.isEmpty){
                    unreplicatedKeys -= unreplicatedValue.id
                    if(persistenceAcks.get(unreplicatedValue.id).isEmpty) {
                      if(unreplicatedValue.id >= 0) { // just remove in case of unreplicatedValue from new replica
                        unreplicatedValue.sender ! OperationAck(unreplicatedValue.id)
                      }
                    }
                  }
                }
              }
              unreplicatedKeys = unreplicatedKeys filter{
                case (key, unreplicatedValue) => {

                  unreplicatedValue.restReplicators.size > 0
                }
              }
            }
            case None => {}
          }
        }
      }

    }

    case _ => {
      log.error("Unknown message")
    }
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
        persistenceAcks += seq -> (sender(), persistMsg, None)
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
      persistenceAcks foreach {case (seq, ack) => persistence ! ack._2 }
    }
    case _ => {
      log.error("Unknown msg")
    }
  }

}

