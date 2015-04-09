
import scala.reflect.runtime.{universe => ru}

abstract class Vertex[+A](implicit tA: ru.TypeTag[A])  {
    val data: A
    override def toString = s"${tA.tpe}($data)"
}

case class Leaf[A](data: A)(implicit tA: ru.TypeTag[A]) extends Vertex[A]
case class Node[A](data: A, left: Vertex[A], right: Vertex[A])(implicit tA: ru.TypeTag[A]) extends Vertex[A]
object Sentinel extends Vertex[Nothing] {
    lazy val data = throw new IllegalStateException()
    override def toString = s"Sentinel[Nothing]"
}
case class VertexData[A](data: A)(implicit tA: ru.TypeTag[A]) extends Vertex[A]
case class VertexWrap[A](data: A, vertex: Vertex[A])(implicit tA: ru.TypeTag[A]) extends Vertex[A]

class CompleteBinaryTree[A](root: Vertex[A]) { //{{{

    type VA = Vertex[A]
    type LA = List[A]

    private def traverse(v: VA)(o: (A,LA,LA) => LA): LA = v match {
        case Node(d, l, r) => o(d, traverse(l)(o), traverse(r)(o))
        case Leaf(d)       => List(d)
        case Sentinel      => Nil
    }

    def traversePreOrder  = traverse(root)((d,lt,rt) => List(d) ::: lt ::: rt ::: Nil)
    def traverseInOrder   = traverse(root)((d,lt,rt) => lt ::: List(d) ::: rt ::: Nil)
    def traversePostOrder = traverse(root)((d,lt,rt) => lt ::: rt ::: List(d) ::: Nil)
    def size = traverseInOrder.size

    def traverse2[B](v: VA)(d1: B)(d2: B)(op: (B,B) => B): B = v match {
        case Node(_, l, r) => op(traverse2(l)(d1)(d2)(op), traverse2(r)(d1)(d2)(op))
        case Leaf(_)       => d1
        case _             => d2 
    }
    def hight    : Int = traverse2(root)(1)(0)((lret,rret) => List(lret,rret).max + 1)
    def leafCount: Int = traverse2(root)(1)(0)((lret,rret) => List(lret,rret).sum)
    def maxRight : A   = traversePreOrder.last
    def maxLeft  : A   = traversePostOrder.head

}   // }}}

class CompleteBinaryTree2[A](root: Vertex[A])(implicit tA: ru.TypeTag[A]) {

    type VA  = Vertex[A]
    type LVA = List[VA]
    type LA  = List[A]

    def traverseVertex[B](z: B)(opz:  (A,VA,B) => B)(vs: LVA)(ov: (VA,VA,VA,LVA) => LVA): B = vs match {
        case (n @ Node(d, l, r)) :: vst => traverseVertex(z)(opz)(ov(VertexWrap(d,n), l, r, vst))(ov)
        case (l @ Leaf(d))       :: vst => traverseVertex(z)(opz)(VertexWrap(d,l) :: vst)(ov)
        case Sentinel            :: vst => traverseVertex(z)(opz)(vst)(ov)
        case VertexWrap(d,v)     :: vst => traverseVertex(opz(d,v,z))(opz)(vst)(ov)
        case v                   :: _   => throw new IllegalStateException(v.toString)
        case Nil                        => z
    }

    def traverseVertexPreOrder  [B](z: B)(opz: (A,VA,B) => B): B = traverseVertex(z)(opz)(List(root))((dv,lv,rv,vs) => dv :: lv :: rv :: vs)
    def traverseVertexInOrder   [B](z: B)(opz: (A,VA,B) => B): B = traverseVertex(z)(opz)(List(root))((dv,lv,rv,vs) => lv :: dv :: rv :: vs)
    def traverseVertexPostOrder [B](z: B)(opz: (A,VA,B) => B): B = traverseVertex(z)(opz)(List(root))((dv,lv,rv,vs) => lv :: rv :: dv :: vs)
    def traverseVertexLevelOrder[B](z: B)(opz: (A,VA,B) => B): B = traverseVertex(z)(opz)(List(root))((dv,lv,rv,vs) => (dv :: vs) ::: (lv :: rv :: Nil))

    def leafCount: Int = traverseVertexInOrder(0) {
        (d,v,z) => v match {
            case Leaf(_) => z + 1
            case _       => z
        }
    }

    /* -------------------------------------------------------------------- */

    def traverseTree[B](z: B)(opz: (A,B) => B)(vs: LVA)(ov: (VA,VA,VA,LVA) => LVA): B = vs match {
        case Node(d, l, r) :: vst => traverseTree(z)(opz)(ov(VertexData(d), l, r, vst))(ov)
        case Leaf(d)       :: vst => traverseTree(z)(opz)(VertexData(d) :: vst)(ov)
        case Sentinel      :: vst => traverseTree(z)(opz)(vst)(ov)
        case VertexData(d) :: vst => traverseTree(opz(d,z))(opz)(vst)(ov)
        case v             :: _   => throw new IllegalStateException(v.toString)
        case Nil                  => z
    }

    def traversePreOrder [B](z: B)(opz: (A,B) => B): B = traverseTree(z)(opz)(List(root))((dv,lv,rv,vs) => dv :: lv :: rv :: vs)
    def traverseInOrder  [B](z: B)(opz: (A,B) => B): B = traverseTree(z)(opz)(List(root))((dv,lv,rv,vs) => lv :: dv :: rv :: vs)
    def traversePostOrder[B](z: B)(opz: (A,B) => B): B = traverseTree(z)(opz)(List(root))((dv,lv,rv,vs) => lv :: rv :: dv :: vs)

    /* Breadth-order traversal (also called a Level-order traversal) {{{
     * F :: Nil :: (B-(A,(D-(C,E)))) :: (G-(_,(I-(H,_)))) :: Nil
     * B :: (G-(_,(I-(H,_)))) :: A :: (D,(C,E)) :: Nil
     * G :: A :: (D-(C,E) :: (_,(I-(H,_))) :: Nil
     * A :: (D-(C,E) :: (_,(I-(H,_))) :: Nil
     * D :: (_,(I-(H,_))) :: C :: E :: Nil
     * _ :: (I-(H,_)) :: C :: E :: Nil
     * I :: C :: E :: (H,_) :: Nil
     * C :: E :: (H,_) :: Nil
     * E :: (H,_) :: Nil
     * H :: _ :: Nil
     * _ :: Nil
     * Nil
     }}} */
    def traverseLevelOrder[B](z: B)(opz: (A,B) => B): B = traverseTree(z)(opz)(List(root))((dv,lv,rv,vs) => (dv :: vs) ::: (lv :: rv :: Nil))

    def collectVertexData(d: A, z: LA) = d :: z
    def toListPreOrder   = traversePreOrder(List.empty[A])(collectVertexData).reverse
    def toListInOrder    = traverseInOrder(List.empty[A])(collectVertexData).reverse
    def toListPostOrder  = traversePostOrder(List.empty[A])(collectVertexData).reverse
    def toListLevelOrder = traverseLevelOrder(List.empty[A])(collectVertexData).reverse

    def size: Int = traverseInOrder(0)((_,z) => z + 1)

    def hight: Int = {
        def traverseHight(v: VA): Int = v match {
            case Node(_, l, r) => List(traverseHight(l), traverseHight(r)).max + 1
            case Leaf(_)       => 1
            case _             => 0
        }
        traverseHight(root)
    }

    def maxRight: A = toListPreOrder.last
    def maxLeft : A = toListPostOrder.head
    def last    : A = toListLevelOrder.last

/*
    def add(d: A): CompleteBinaryTree2[A] = {
         def traverseAdd(v: VA): Int = v match {
            case Node(_, l, r) => List(traverseHight(l), traverseHight(r)).max + 1
            case Leaf(_)       => new CompleteBinaryTree(Leaf(d))
            case Sentinel      => new CompleteBinaryTree(Leaf(d))
            case _             => 0
        }
        traverseAdd(root)
    }
*/
}

/*
 *        D
 *      /   \
 *     B     F
 *    / \   / \
 *   A   C E   G
 *
 *          H
 *        /   \
 *      D        K
 *     / \      / \
 *   B    F    J    L 
 *  /\   /\   /\
 * A  C E  G I  _
 *
 *        F
 *      /   \
 *    B       G
 *   / \       \
 *  A   D       I
 *     / \     /
 *    C   E   H
 *
 */
object BinaryTree2 extends App {

    def traverseBinaryTree[A](root: Vertex[A])(implicit tA: ru.TypeTag[A]) {

//        val btree = new CompleteBinaryTree(root) /* {{{ */
//        println("-- CompleteBinaryTree --")
//        println("PreOrder  : " + btree.traversePreOrder)
//        println("InOrder   : " + btree.traverseInOrder)
//        println("PostOrder : " + btree.traversePostOrder)
//        println("size      : " + btree.size)
//        println("hight     : " + btree.hight)
//        println("leafCount : " + btree.leafCount)
//        println("maxRight  : " + btree.maxRight)
//        println("maxLeft   : " + btree.maxLeft)
//
//        println() /* }}} */

        val btree2 = new CompleteBinaryTree2(root)
        println("-- CompleteBinaryTree2 --")
        println("PreOrder   : " + btree2.toListPreOrder)
        println("InOrder    : " + btree2.toListInOrder)
        println("PostOrder  : " + btree2.toListPostOrder)
        println("LevelOrder : " + btree2.toListLevelOrder)
        println("size       : " + btree2.size)
        println("hight      : " + btree2.hight)
        println("leafCount  : " + btree2.leafCount)
        println("maxRight   : " + btree2.maxRight)
        println("maxLeft    : " + btree2.maxLeft)
        println("last       : " + btree2.last)
    }

    println("[root1]")
    val root1 = Node("D", Node("B", Leaf("A"), Leaf("C")), Node("F", Leaf("E"), Leaf("G")))
    traverseBinaryTree(root1)

    println()

    println("[root2]")
    val root2 = Node("F", Node("B", Leaf("A"), Node("D", Leaf("C"), Leaf("E"))), Node("G", Sentinel, Node("I", Leaf("H"), Sentinel)))
    traverseBinaryTree(root2)

    println()

    println("[root3]")
    val root3 = Node("H", Node("D", Node("B", Leaf("A"), Leaf("C")), Node("F", Leaf("E"), Leaf("G"))), Node("K", Node("J", Leaf("I"), Sentinel), Leaf("L")))
    traverseBinaryTree(root3)

    /* {{{
     *[root1]
     *-- CompleteBinaryTree2 --
     *PreOrder   : List(D, B, A, C, F, E, G)
     *InOrder    : List(A, B, C, D, E, F, G)
     *PostOrder  : List(A, C, B, E, G, F, D)
     *LevelOrder : List(D, B, F, A, C, E, G)
     *size       : 7
     *hight      : 3
     *leafCount  : 4
     *maxRight   : G
     *maxLeft    : A
     *last       : G
     *
     *[root2]
     *-- CompleteBinaryTree2 --
     *PreOrder   : List(F, B, A, D, C, E, G, I, H)
     *InOrder    : List(A, B, C, D, E, F, G, H, I)
     *PostOrder  : List(A, C, E, D, B, H, I, G, F)
     *LevelOrder : List(F, B, G, A, D, I, C, E, H)
     *size       : 9
     *hight      : 4
     *leafCount  : 4
     *maxRight   : H
     *maxLeft    : A
     *last       : H
     *
     *[root3]
     *-- CompleteBinaryTree2 --
     *PreOrder   : List(H, D, B, A, C, F, E, G, K, J, I, L)
     *InOrder    : List(A, B, C, D, E, F, G, H, I, J, K, L)
     *PostOrder  : List(A, C, B, E, G, F, D, I, J, L, K, H)
     *LevelOrder : List(H, D, K, B, F, J, L, A, C, E, G, I)
     *size       : 12
     *hight      : 4
     *leafCount  : 6
     *maxRight   : L
     *maxLeft    : A
     *last       : I
     }}} */
}

/* vim: set foldmethod=marker: */
