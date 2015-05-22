/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import akka.event.Logging
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  case object CheckForGc

  case object ActorCreated

  case object ActorCopied

  case class InsertCopy(requester: ActorRef, elem: Int) extends Operation{
    def id = 0
  }


  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))
  val log = Logging(context.system, this)

  var root = createRoot
  var newRoot: ActorRef = null
  var waitingForGcBegin = false
  var waitingForGcEnd = false
  var currentOperations = Map[Int, ActorRef]()
  var actorCounter: Int = 1
  var actorsToCopy: Int = 0
  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case ActorCreated => {
      actorCounter += 1
      log.info("Actor created, total: " + actorCounter)
    }
    case ActorCopied => {
      actorsToCopy -= 1
      log.info("Actor copied, rest to copy: " + actorCounter)
      if(actorsToCopy == 0) self ! CopyFinished
    }
    case Insert(requester, id, elem) => {
      //log.debug("Insert message received id = " + id +  ", elem = " + elem )
      if(!waitingForGcBegin && !waitingForGcEnd){
        currentOperations += id -> requester
        root ! Insert(self, id, elem)
      }
      else{
        pendingQueue = pendingQueue.enqueue(Insert(requester, id, elem))
      }
    }
    case Contains(requester, id, elem) => {
      //log.debug("Contains message received id = " + id +  ", elem = " + elem )
      if(!waitingForGcBegin && !waitingForGcEnd){
        currentOperations += id -> requester
        root ! Contains(self, id, elem)
      }
      else{
        pendingQueue = pendingQueue.enqueue(Contains(requester, id, elem))
      }
    }
    case Remove(requester, id, elem) => {
      //log.debug("Remove message received id = " + id +  ", elem = " + elem )
      if(!waitingForGcBegin && !waitingForGcEnd){
        currentOperations += id -> requester
        root ! Remove(self, id, elem)
      }
      else{
        pendingQueue = pendingQueue.enqueue(Remove(requester, id, elem))
      }

    }
    case ContainsResult(id, result) =>{
      //log.debug("ContainsResult message received id = " + id +  ", result = " + result )

      currentOperations(id) ! ContainsResult(id, result)
      currentOperations -= id
      self ! CheckForGc
    }
    case OperationFinished(id) =>{
      //log.debug("OperationFinished message received id = " + id)

      currentOperations(id) ! OperationFinished(id)
      currentOperations -= id
      self ! CheckForGc
    }
    case CheckForGc => {
      //if(waitingForGcBegin) log.info("CheckForGc message received and currentOperations = " + currentOperations.size)
      if(waitingForGcBegin && currentOperations.size == 0){
        newRoot = createRoot
        waitingForGcEnd = true
        waitingForGcBegin = false
        actorsToCopy = actorCounter
        actorCounter = 1
        root ! CopyTo(self, newRoot)
      }
    }
    case CopyFinished => {

      root ! PoisonPill
      root = newRoot
      newRoot = null
      waitingForGcEnd = false
      //log.info("Copy finished, queue = " + pendingQueue.toString())
      while (pendingQueue.size > 0){
        val p = pendingQueue.dequeue
        val op = p._1
        self ! op
        //log.info(op.toString())
        pendingQueue = p._2
      }
    }
    case GC => {
      //log.debug("GC message received")
      if(!waitingForGcBegin && !waitingForGcEnd){
        waitingForGcBegin = true
      }
    }
    case _ => ???
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = ???

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(requester: ActorRef, treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved
  //val log = Logging(context.system, this)
  //override def preStart() = {
    //log.debug("Starting :"+ elem)
  //}
  //override def postStop() = {
    //log.debug("Stopping :"+ elem)
  //}

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Contains(requester, id, elem) => {
      if(elem == this.elem) {
        requester ! ContainsResult(id, !removed)
      }
      else if (elem < this.elem){
        if(subtrees.contains(Left)) {
          subtrees(Left) ! Contains(requester, id, elem)
        }
        else{
          requester ! ContainsResult(id, false)
        }
      }
      else if (elem > this.elem){
        if(subtrees.contains(Right)) {
          subtrees(Right) ! Contains(requester, id, elem)
        }
        else{
          requester ! ContainsResult(id, false)
        }
      }
    }

    case Insert(requester, id, elem) => {
      //log.info(id + ": " + requester.toString() )
        if(elem == this.elem) {
          if(removed) requester ! ActorCreated
          removed = false
          requester ! OperationFinished (id)
        }
        else if (elem < this.elem){
          if(subtrees.contains(Left)) {
            subtrees(Left) ! Insert(requester, id, elem)
          }
          else{
            subtrees += Left ->  context.actorOf(BinaryTreeNode.props(elem = elem, initiallyRemoved = false))
            requester ! ActorCreated
            requester ! OperationFinished (id)
          }
        }
        else if (elem > this.elem){
          if(subtrees.contains(Right)) {
            subtrees(Right) ! Insert(requester, id, elem)
          }
          else{
            subtrees += Right ->  context.actorOf(BinaryTreeNode.props(elem = elem, initiallyRemoved = false))
            requester ! ActorCreated
            requester ! OperationFinished (id)
          }
        }
    }
    case Remove(requester, id, elem) => {
      if(elem == this.elem) {
        removed = true
        requester ! OperationFinished(id)
      }
      else if (elem < this.elem){
        if(subtrees.contains(Left)) {
          subtrees(Left) ! Remove(requester, id, elem)
        }
        else{
          requester ! OperationFinished(id)
        }
      }
      else if (elem > this.elem){
        if(subtrees.contains(Right)) {
          subtrees(Right) ! Remove(requester, id, elem)
        }
        else{
          requester ! OperationFinished(id)
        }
      }
    }
    case CopyTo(requester: ActorRef, treeNode: ActorRef) => {
      if(!removed) {
        treeNode ! InsertCopy(requester, elem)
      }
      else{
        requester ! ActorCopied
      }
      subtrees.foreach{_._2 ! CopyTo(requester, treeNode)}
    }

    case InsertCopy(requester, elem) => {
      //log.info(id + ": " + requester.toString() )
      if(elem == this.elem) {
        if(removed) requester ! ActorCreated
        requester ! ActorCopied
        removed = false
      }
      else if (elem < this.elem){
        if(subtrees.contains(Left)) {
          subtrees(Left) ! InsertCopy(requester, elem)
        }
        else{
          subtrees += Left ->  context.actorOf(BinaryTreeNode.props(elem = elem, initiallyRemoved = false))
          requester ! ActorCopied
          requester ! ActorCreated
        }
      }
      else if (elem > this.elem){
        if(subtrees.contains(Right)) {
          subtrees(Right) ! InsertCopy(requester, elem)
        }
        else{
          subtrees += Right ->  context.actorOf(BinaryTreeNode.props(elem = elem, initiallyRemoved = false))
          requester ! ActorCopied
          requester ! ActorCreated
        }
      }
    }
    case _ => ???
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???


}
