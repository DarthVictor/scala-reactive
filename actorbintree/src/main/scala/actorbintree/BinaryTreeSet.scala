/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
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

  var root = createRoot
  var newRoot: ActorRef = null
  var waitingForGcBegin = false
  var waitingForGcEnd = false
  var currentOperations = Map[Int, ActorRef]()
  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case Insert(requester, id, ref) => {
      if(!waitingForGcBegin && !waitingForGcEnd){
        currentOperations += id -> requester
        root ! Insert(self, id, ref)
      }
      else{
        pendingQueue = pendingQueue.enqueue(Insert(requester, id, ref))
      }
    }
    case Contains(requester, id, ref) => {
      if(!waitingForGcBegin && !waitingForGcEnd){
        currentOperations += id -> requester
        root ! Contains(self, id, ref)
      }
      else{
        pendingQueue = pendingQueue.enqueue(Contains(requester, id, ref))
      }
    }
    case Remove(requester, id, ref) => {
      if(!waitingForGcBegin && !waitingForGcEnd){
        currentOperations += id -> requester
        root ! Remove(self, id, ref)
      }
      else{
        pendingQueue = pendingQueue.enqueue(Remove(requester, id, ref))
      }

    }
    case ContainsResult(id, result) =>{
      currentOperations(id) ! ContainsResult(id, result)
      currentOperations -= id
      self ! CheckForGc
    }
    case OperationFinished(id) =>{
      currentOperations(id) ! OperationFinished(id)
      currentOperations -= id
      self ! CheckForGc
    }
    case CheckForGc => {
      if(waitingForGcBegin && currentOperations.size == 0){
        newRoot = createRoot
        waitingForGcEnd = true
        waitingForGcBegin = false
        root ! CopyTo(newRoot)
      }
    }
    case CopyFinished => {
      waitingForGcEnd = false
      while (pendingQueue.size > 0){
        val p = pendingQueue.dequeue
        val op = p._1
        self ! op
        pendingQueue = p._2
      }
      root = newRoot
      newRoot = null
    }
    case GC => {
     /* if(!waitingForGcBegin && !waitingForGcEnd){
        waitingForGcBegin = true
      }*/
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

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved
  var elementsToCopy: Int = 0
  var copyToSender: ActorRef = null
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
      //if(id > 0){
        if(elem == this.elem) {
          removed = false
          requester ! OperationFinished (id)
        }
        else if (elem < this.elem){
          if(subtrees.contains(Left)) {
            subtrees(Left) ! Insert(requester, id, elem)
          }
          else{
            subtrees += Left ->  context.actorOf(BinaryTreeNode.props(elem = elem, initiallyRemoved = false))
            requester ! OperationFinished (id)
          }
        }
        else if (elem > this.elem){
          if(subtrees.contains(Right)) {
            subtrees(Right) ! Insert(requester, id, elem)
          }
          else{
            subtrees += Right ->  context.actorOf(BinaryTreeNode.props(elem = elem, initiallyRemoved = false))
            requester ! OperationFinished (id)
          }
        }
     /* }
      else {
        self ! CopyFinished
      }*/
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
    case CopyTo(treeNode) => {
      if(!removed) treeNode ! Insert(self, 0, elem)
      elementsToCopy = subtrees.size + 1
      copyToSender = sender()
      subtrees.foreach{_._2 ! CopyTo(treeNode)}
    }
    case CopyFinished => {
      elementsToCopy -= 1
      if(elementsToCopy == 0) {
        copyToSender ! CopyFinished
        self ! PoisonPill
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
