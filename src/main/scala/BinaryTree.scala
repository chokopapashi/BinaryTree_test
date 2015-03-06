
abstract class Vertex[A] {
    val data: A
}

case class Leaf[A](data: A) extends Vertex
object Sentinel extends Vertex[Nothing]

abstract class Node[A](
    data: A, leftChild: Vertex[A], rightChild: Vertex[A])
    extends Vertex[A]

case class FullNode[A](
    data: A, leftChild: Vertex[A], rightChild: Vertex[A])
    extends Node(data, leftChild, rightChild)

case class NodeWithLeftChild[A](
    data: A, leftChild: Vertex[A])
    extends Node(data, leftChild, Sentinel)

case class NodeWithRightChild[A](
    data: A, rightChild: Vertex[A])
    extends Node(data, Sentinel, rightChild)

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
 *        
 *    ^    _
 *   ¢      ¢
 *
 *        
 *    ^    _
 *          ¢
 *  ^_
 * ¢   S
 *
 *        
 *    ^    _
 *          ¢
 *  ^_
 * ¢  ¢
 *
 *        
 *    ^    _
 *           
 *  ^_    ^_
 * ¢  ¢  ¢   S
 *
 *        
 *    ^    _
 *           
 *  ^_    ^_
 * ¢  ¢  ¢  ¢
 *
 *          
 *      ^    _
 *             
 *    ^_    ^_
 *      ¢  ¢  ¢
 *  ^_
 * ¢   S
 *
 *        A
 *      /   \
 *     B     C
 *    / \   / \
 *   D   E F   G
 *  / 
 * šNew Leaf
 *
 * check | accumulator1  | accumulator2
 *   A   | List(B,C)     | (B,C)
 *   B   | List(C,D,E)   | ((B,C),(D,E))
 *   C   | List(D,E,F,G) | ((B,C),(D,E),(F,G))
 *   D   | List(E,F,G)   | ((B,C),(D,E),(F,G))
 *   E   | List(F,G)     | ((B,C),(D,E),(F,G))
 *   F   | List(G)       | ((B,C),(D,E),(F,G))
 *   G   | Nil           | ((B,C),(D,E),(F,G))
 *  ((B,C),(D,E),(F,G),(H,S))
 */
class AddedCompleteBinaryTree[A](newData: A, srcTree : CompleteBinaryTree[A]) extends CompleteBinaryTree {


    def addTree(vs: List[A], xs: List[A], level: Int) {

            val levelNodesCount = level match {
                case 0 => 1
                case _ => (pow(2,level) - pow(2,(level-1))) match {
                    case c if(c < list.size) => c
                    case _ => list.size
                }
            }


            {
                case v :: vs =>
            }

            val a = list.take(levelNodesCount) 


    }


    def test(vs: List[A], cs: List[A]): Vertex[A] = {
       

        test(d, test(),test())

        val lr = cs.take(2)

        Node(vs.head, test(l), test(r),
    }





    val insertPoint

    def addTree(v: Vertex[A], c: Int): Vertex[A] = v match {
        case FullNode(d, lc, rc)      => FullNode(d, addTree(lc, c+1), addTree(rc, c+ 1))
        case NodeWithLeftChild(d, lc) => FullNode(d, lc, Leaf(nd))
        case Leaf(d)                  => if(c == insertPoint) NodeWithLeftChild(d, Leaf(nd))
                                         else vertex
    }

    def createVertex(accumulator: List[Vertex])

    {
        a match {
            case Sentinel :: Leaf :: tl => {
                
            }
            



            case FullNode(d, l, r) => {
                val nl = addTree( r :: l :: accumulator)
                val nr = addTree( accumulator ::: List(l,r))
                FullNode(d, nl, nr)
            }
            case NodeWithLeftChild(d, l) => {

                addTree( accumulator ::: List(l))
            }

            case Leaf(_)       => Node


            case Sentinel      => Leaf(newData)
        }
    }




    /*
     * postorder traversal
     */
    def addTree(v: Vertex[A], c: Int): Vertex[A] = v match {
        case FullNode(d, l, r)      => {
            FullNode(d, addTree(lc, c+1), addTree(rc, c+ 1))
        }



        case NodeWithLeftChild(d, lc) => FullNode(d, lc, Leaf(nd))
        case Leaf(d)                  => if(c == insertPoint) NodeWithLeftChild(d, Leaf(nd))
                                         else vertex
    }


    val root = 

    val root = initTree.last match {
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


