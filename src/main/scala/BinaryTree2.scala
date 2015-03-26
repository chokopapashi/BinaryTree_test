
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

class CompleteBinaryTree[A](root: Vertex[A]) {

    private def traverse(v: Vertex[A])(o: (A,List[A],List[A]) => List[A]): List[A] = v match {
        case Node(d, l, r) => o(d, traverse(l)(o), traverse(r)(o))
        case Leaf(d)       => List(d)
        case Sentinel      => Nil
    }

    def traversPreOrder(): List[A] =
        traverse(root)((d,lt,rt) => List(d) ::: lt ::: rt ::: Nil)

    def traversInOrder(): List[A] =
        traverse(root)((d,lt,rt) => lt ::: List(d) ::: rt ::: Nil)

    def traversPostOrder(): List[A] =
        traverse(root)((d,lt,rt) => lt ::: rt ::: List(d) ::: Nil)

    def traversLevelOrder(): List[A] =
        traverse(root)((d,lt,rt) => (List(d) ::: lt) ::: rt ::: Nil)
}

/*
 *        D
 *      /   \
 *     B     F
 *    / \   / \
 *   A   C E   G
 */
object BinaryTree2 extends App {
   
//    val root = Node("D", Node("B", Leaf("A"), Leaf("C")), Node("F", Leaf("E"), Leaf("G")))
    val root = Node("F", Node("B", Leaf("A"), Node("D", Leaf("C"), Leaf("E"))), Node("G", Sentinel, Node("I", Leaf("H"), Sentinel)))
    val btree = new CompleteBinaryTree(root)
    println(btree.traversPreOrder())
    println(btree.traversInOrder())
    println(btree.traversPostOrder())
    println(btree.traversLevelOrder())
}

