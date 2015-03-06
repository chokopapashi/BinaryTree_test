
abstract class Vertex[A](data: A, parent: Vertex[A]) {
    def level = parent.level + 1
}

abstract class Node[A](
    data: A, parent Node[A], leftChild: Vertex, rightChild: Vertex)
    extends Vertex(data)

case class Leaf[A](data: A, parent: Node[A]) extends Vertex(data)
case class RootLeaf[A](data: A) extends Vertex(data)

object Sentinel extends Vertex

case class FullNode[A](
    data: A, parent Node[A], leftChild: Vertex, rightChild: Vertex)
    extends Node(data, parent, leftChild, rightChild) extends Node

case class NodeWithLeftChild[A](
    data: A, parent Node[A], leftChild: Vertex)
    extends Node(data, parent, leftChild, Sentinel) extends Node

case class NodeWithRightChild[A](
    data: A, parent Node[A], rightChild: Vertex)
    extends Node(data, parent, Sentinel, rightChild) extends Node

abstract class CompleteBinaryTree[A](btree: CompleteBinaryTree[A]) {

    val numberOfVertex: Int

    val maxVertexes = btree



    val _last = 

    val farRight

    def this(vertex: Vertex, btree: CompleteBinaryTree) {
        
    }

    

    def last: Vertex = _last

    def addLast[A](data: A) = last match {
        case FullNode          => new CompleteBinaryTree(this.root, Leaf(data, this.last))
        case NodeWithLeftChild => new CompleteBinaryTree(this.root, FullNode(data, this.last, this.last.leftChild, Leaf(data, this.last)))
        case Leaf

    }

    val root

    def height = last.level
}

object CompleteBinaryTree {
    def empty() = new ImmubableBinaryTree
    def withRoot = {
        val btree = empty()
        
    }
   
}

/*
 * last match case FullNode() ¨
 *       parent.parent
 *    ^               _
 *    parent(FullNode)  parent.parent.rightChild
 *  ^_               ^
 *     last          parent.parent.rightChild.leftChild
 */
class AddedCompleteBinaryTree[A](vertexData: A, initTree : CompleteBinaryTree[A]) extends CompleteBinaryTree {

    val root = initTree.root match {
        case RootLeaf(leafData) => NodeWithLeftChild(leafData, _, Leaf(vertexData, )
    }

    new AddedCompleteBinaryTree



    val numberOfVertex = btree.numberOfVertex + 1
    val last = btree.last.parent match {
        case parent @ FullNode(_,_,_,_)          => parent.parent.left
        
        
        
        
        new CompleteBinaryTree(this.root, Leaf(data, this.last))
        case NodeWithLeftChild => new CompleteBinaryTree(this.root, FullNode(data, this.last, this.last.leftChild, Leaf(data, this.last)))
        case Leaf
    }

}


