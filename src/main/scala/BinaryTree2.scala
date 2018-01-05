
import scala.reflect.runtime.{universe => ru}
import scala.math.pow

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

trait BinaryTree[A] {
    def toListPreOrder   : List[A]
    def toListInOrder    : List[A]
    def toListPostOrder  : List[A]
    def toListLevelOrder : List[A]

    def size : Int
    def hight: Int
    def leafCount: Int
    def maxRight: A
    def maxLeft : A
    def last    : A

    def calcMaxNumOfVertex(h: Int) = (pow(2, (h + 1)) - 1).toInt
    def calcMaxNumOfLeaf(h: Int) = (calcMaxNumOfVertex(h) + 1) / 2

    def maxNumOfVertex = calcMaxNumOfVertex(hight)
    def maxNumOfLeaf = calcMaxNumOfLeaf(hight)
}

class CompleteBinaryTree1[A](root: Vertex[A]) extends BinaryTree[A] { //{{{

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

    def toListPreOrder   = traversePreOrder
    def toListInOrder    = traverseInOrder
    def toListPostOrder  = traversePostOrder 
    def toListLevelOrder = Nil  /* Not implemented */

    def size = traverseInOrder.size
    def traverse2[B](v: VA)(d1: B)(d2: B)(op: (B,B) => B): B = v match {
        case Node(_, l, r) => op(traverse2(l)(d1)(d2)(op), traverse2(r)(d1)(d2)(op))
        case Leaf(_)       => d1
        case _             => d2 
    }
    def hight    : Int = traverse2(root)(1)(0)((lret,rret) => List(lret,rret).max + 1) - 1
    def leafCount: Int = traverse2(root)(1)(0)((lret,rret) => List(lret,rret).sum)
    def maxRight : A   = traversePreOrder.last
    def maxLeft  : A   = traversePostOrder.head
    def last     : A   = root.data /* Not implemented */

}   // }}}

class CompleteBinaryTree2[A](root: Vertex[A])(implicit tA: ru.TypeTag[A]) extends BinaryTree[A] {

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
        traverseHight(root) - 1
    }

    def maxRight: A = toListPreOrder.last
    def maxLeft : A = toListPostOrder.head
    def last    : A = toListLevelOrder.last

    /*
     * List(H, D, B, A, C, F, E, G, K, J, I, Sentinel, L, Sentinel, Sentinel)
     */
    def add1(nd: A): CompleteBinaryTree2[A] =
    {
        val bovs = traverseVertexLevelOrder(List.empty[VA])((d,_,z) => VertexData(d) :: z)
        val newHight = if((bovs.size+1) <= maxNumOfVertex)
                           hight
                       else
                           hight + 1
        val nbovs: List[VA] = List.fill(calcMaxNumOfVertex(newHight)-(bovs.size+1))(Sentinel) ::: (VertexData(nd) :: bovs)

        def createTreeList(h: Int, l1: List[VA], l2: List[VA]): List[VA] = {
            val n = calcMaxNumOfLeaf(h)
            if(h == 0) 
                l1 ::: l2
            else {
                val lx = l1.take(n).reverse
                val ly = if(l2.isEmpty) lx else lx.zip(l2.grouped(l2.size/n).toList).map(t => t._1 :: t._2).flatten
                createTreeList(h - 1, l1.drop(n), ly)
            }
        }
        val treeList = createTreeList(newHight, nbovs, Nil)

        def createTree(tl: List[VA]): VA = tl match {
            case VertexData(dv) :: Sentinel :: Sentinel :: Nil             => Leaf(dv)
            case VertexData(dv) :: VertexData(dl) :: Sentinel :: Nil       => Node(dv, Leaf(dl), Sentinel)
            case VertexData(dv) :: VertexData(dl) :: VertexData(dr) :: Nil => Node(dv, Leaf(dl), Leaf(dr))
            case VertexData(dv) :: tlt                                     => Node(dv, createTree(tlt.take(tlt.size/2)), createTree(tlt.drop(tlt.size/2)))
            case _ => throw new IllegalStateException("invalid Tree List")
        }
        val tree = createTree(treeList)
        new CompleteBinaryTree2(tree)
    }

    def add2(nd: A): CompleteBinaryTree2[A] =
    {
        val bovs = traverseVertexLevelOrder(List.empty[VA])((d,_,z) => VertexData(d) :: z)
        val newHight = if((bovs.size+1) <= maxNumOfVertex)
                           hight
                       else
                           hight + 1
        val nbovs: List[VA] = List.fill(calcMaxNumOfVertex(newHight)-(bovs.size+1))(Sentinel) ::: (VertexData(nd) :: bovs)

        def createTree(h: Int, l1: List[VA], l2: List[Tuple2[VA,VA]]): VA = {
            val n = calcMaxNumOfLeaf(h)
            val lx = l1.take(n).reverse
            if(h == 0) l2 match {
                case Nil => Leaf(l1.head.data)
                case (l, r) :: Nil => Node(l1.head.data, l, r)
                case _ => throw new IllegalStateException("invalid new Breadth-order Tree List")
            } else {
                val lx = l1.take(n).reverse
                val ly =
                    if(l2.isEmpty) lx.map { 
                        case Sentinel      => Sentinel
                        case VertexData(d) => Leaf(d)
                    } else lx.zip(l2).map {
                        case (VertexData(dv), (Sentinel, Sentinel))         => Leaf(dv)
                        case (VertexData(dv), (l: Vertex[A], Sentinel))     => Node(dv, l, Sentinel)
                        case (VertexData(dv), (l: Vertex[A], r: Vertex[A])) => Node(dv, l, r)
                        case _ => throw new IllegalStateException("createTree:lx.zip(l2).map")
                    }
                val lz = ly.grouped(2).map(l => (l(0),l(1))).toList
                createTree(h - 1, l1.drop(n), lz)
            }
        }
        val tree = createTree(newHight, nbovs, Nil)
        new CompleteBinaryTree2(tree)
    }
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

    def inspectTreeConcrete[A](btree: BinaryTree[A])(implicit tA: ru.TypeTag[A]) {
        println(s"-- ${btree.getClass.getName} --")
        println("PreOrder   : " + btree.toListPreOrder)
        println("InOrder    : " + btree.toListInOrder)
        println("PostOrder  : " + btree.toListPostOrder)
        println("LevelOrder : " + btree.toListLevelOrder)
        println("size       : " + btree.size)
        println("hight      : " + btree.hight)
        println("leafCount  : " + btree.leafCount)
        println("maxRight   : " + btree.maxRight)
        println("maxLeft    : " + btree.maxLeft)
        println("last       : " + btree.last)
        println("maxNumOfVertex : " + btree.maxNumOfVertex)
        println("maxNumOfLeaf   : " + btree.maxNumOfLeaf)
        println("")
    }

    def inspectTree[A](root: Vertex[A])(implicit tA: ru.TypeTag[A]) {
        inspectTreeConcrete(new CompleteBinaryTree1(root))
        inspectTreeConcrete(new CompleteBinaryTree2(root))
    }
   
    val root1 = Node("D", Node("B", Leaf("A"), Leaf("C")), Node("F", Leaf("E"), Leaf("G")))
    val root2 = Node("F", Node("B", Leaf("A"), Node("D", Leaf("C"), Leaf("E"))), Node("G", Sentinel, Node("I", Leaf("H"), Sentinel)))
    val root3 = Node("H", Node("D", Node("B", Leaf("A"), Leaf("C")), Node("F", Leaf("E"), Leaf("G"))), Node("K", Node("J", Leaf("I"), Sentinel), Leaf("L")))

    println(f"[test1 : root1]")
    inspectTree(root1)

    println(f"[test2 : root2]")
    inspectTree(root2)

    println(f"[test3 : root3]")
    inspectTree(root3)

    println(f"[test4 : add Vertex to root3]")
    inspectTreeConcrete(new CompleteBinaryTree2(root3).add1("a"))
    println(f"[test5 : create tree]")
    inspectTreeConcrete(new CompleteBinaryTree2(Leaf("H")).add1("D").add1("K").add1("B").add1("F").add1("J").add1("L")
                                                          .add1("A").add1("C").add1("E").add1("G").add1("I"))
    println(f"[test6 : add repeatedly]")

    {
        var tree =  new CompleteBinaryTree2(Leaf("H")) ; inspectTreeConcrete(tree)
        tree = tree.add1("D") ; inspectTreeConcrete(tree)
        tree = tree.add1("K") ; inspectTreeConcrete(tree)
        tree = tree.add1("B") ; inspectTreeConcrete(tree)
        tree = tree.add1("F") ; inspectTreeConcrete(tree)
        tree = tree.add1("J") ; inspectTreeConcrete(tree)
        tree = tree.add1("L") ; inspectTreeConcrete(tree)
        tree = tree.add1("A") ; inspectTreeConcrete(tree)
        tree = tree.add1("C") ; inspectTreeConcrete(tree)
        tree = tree.add1("E") ; inspectTreeConcrete(tree)
        tree = tree.add1("G") ; inspectTreeConcrete(tree)
        tree = tree.add1("I") ; inspectTreeConcrete(tree)
    }

    println(f"[test7 : add Vertex to root3]")
    inspectTreeConcrete(new CompleteBinaryTree2(root3).add2("a"))
    println(f"[test8 : create tree]")
    inspectTreeConcrete(new CompleteBinaryTree2(Leaf("H")).add2("D").add2("K").add2("B").add2("F").add2("J").add2("L")
                                                          .add2("A").add2("C").add2("E").add2("G").add2("I"))
    println(f"[test9 : add repeatedly]")

    {
        var tree =  new CompleteBinaryTree2(Leaf("H")) ; inspectTreeConcrete(tree)
        tree = tree.add2("D") ; inspectTreeConcrete(tree)
        tree = tree.add2("K") ; inspectTreeConcrete(tree)
        tree = tree.add2("B") ; inspectTreeConcrete(tree)
        tree = tree.add2("F") ; inspectTreeConcrete(tree)
        tree = tree.add2("J") ; inspectTreeConcrete(tree)
        tree = tree.add2("L") ; inspectTreeConcrete(tree)
        tree = tree.add2("A") ; inspectTreeConcrete(tree)
        tree = tree.add2("C") ; inspectTreeConcrete(tree)
        tree = tree.add2("E") ; inspectTreeConcrete(tree)
        tree = tree.add2("G") ; inspectTreeConcrete(tree)
        tree = tree.add2("I") ; inspectTreeConcrete(tree)
    }

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
