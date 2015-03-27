
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
case class DataVertex[A](data: A)(implicit tA: ru.TypeTag[A]) extends Vertex[A]


class CompleteBinaryTree[A](root: Vertex[A]) { //{{{

    type VA = Vertex[A]
    type LA = List[A]

    private def traverse(v: VA)(o: (A,LA,LA) => LA): LA = v match {
        case Node(d, l, r) => o(d, traverse(l)(o), traverse(r)(o))
        case Leaf(d)       => List(d)
        case Sentinel      => Nil
    }

    def traversPreOrder()  = traverse(root)((d,lt,rt) => List(d) ::: lt ::: rt ::: Nil)
    def traversInOrder()   = traverse(root)((d,lt,rt) => lt ::: List(d) ::: rt ::: Nil)
    def traversPostOrder() = traverse(root)((d,lt,rt) => lt ::: rt ::: List(d) ::: Nil)
}   // }}}

class CompleteBinaryTree2[A](root: Vertex[A])(implicit tA: ru.TypeTag[A]) {

    type VA  = Vertex[A]
    type LVA = List[VA]

    def traverse[B](z: B)(opz: (A,B) => B)(vs: LVA)(ov: (VA,VA,VA,LVA) => LVA): B = vs match {
        case Node(d, l, r) :: vst => traverse(z)(opz)(ov(DataVertex(d), l, r, vst))(ov)
        case Leaf(d)       :: vst => traverse(z)(opz)(DataVertex(d) :: vst)(ov)
        case Sentinel      :: vst => traverse(z)(opz)(vst)(ov)
        case DataVertex(d) :: vst => traverse(opz(d,z))(opz)(vst)(ov)
        case v             :: _   => throw new IllegalStateException(v.toString)
        case Nil                  => z
    }

    def opz(d: A, z: List[A]) = d :: z

    def traversPreOrder()   = traverse(List.empty[A])(opz)(List(root))((dv,lv,rv,vs) => dv :: lv :: rv :: vs).reverse
    def traversInOrder()    = traverse(List.empty[A])(opz)(List(root))((dv,lv,rv,vs) => lv :: dv :: rv :: vs).reverse
    def traversPostOrder()  = traverse(List.empty[A])(opz)(List(root))((dv,lv,rv,vs) => lv :: rv :: dv :: vs).reverse

    /*
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
     */
    def traversLevelOrder() = traverse(List.empty[A])(opz)(List(root))((dv,lv,rv,vs) => (dv :: vs) ::: (lv :: rv :: Nil)).reverse
}

/*
 *        D
 *      /   \
 *     B     F
 *    / \   / \
 *   A   C E   G
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
   
//    val root = Node("D", Node("B", Leaf("A"), Leaf("C")), Node("F", Leaf("E"), Leaf("G")))
    val root = Node("F", Node("B", Leaf("A"), Node("D", Leaf("C"), Leaf("E"))), Node("G", Sentinel, Node("I", Leaf("H"), Sentinel)))
    val btree = new CompleteBinaryTree(root)
    println("[CompleteBinaryTree]")
    println("PreOrder  : " + btree.traversPreOrder())
    println("InOrder   : " + btree.traversInOrder())
    println("PostOrder : " + btree.traversPostOrder())

    println()

    val btree2 = new CompleteBinaryTree2(root)
    println("[CompleteBinaryTree2]")
    println("PreOrder   : " + btree2.traversPreOrder())
    println("InOrder    : " + btree2.traversInOrder())
    println("PostOrder  : " + btree2.traversPostOrder())
    println("LevelOrder : " + btree2.traversLevelOrder())

    /*
     * [info] Running BinaryTree2
     * [CompleteBinaryTree]
     * PreOrder  : List(F, B, A, D, C, E, G, I, H)
     * InOrder   : List(A, B, C, D, E, F, G, H, I)
     * PostOrder : List(A, C, E, D, B, H, I, G, F)
     *
     * [CompleteBinaryTree2]
     * PreOrder   : List(F, B, A, D, C, E, G, I, H)
     * InOrder    : List(A, B, C, D, E, F, G, H, I)
     * PostOrder  : List(A, C, E, D, B, H, I, G, F)
     * LevelOrder : List(F, B, G, A, D, I, C, E, H)
     */
}

/* vim: set foldmethod=marker: */
