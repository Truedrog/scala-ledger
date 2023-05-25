package sledger.utils

import cats._
import cats.syntax.all._
import cats.derived.semiauto
import scala.annotation.tailrec

object RoseTree {
  sealed trait Tree[+A]

  case class Node[A](rootLabel: A, subForest: List[Tree[A]]) extends Tree[A] 

  def flatten[A](tree: Tree[A]): List[A] = {
    def squish(node: Tree[A], acc: List[A]): List[A] = node match {
      case Node(x, ts) => x :: ts.foldRight(acc)(squish)
    }

    squish(tree, Nil)
  }

  object Tree {

    implicit def eqTree[A: Eq]: Eq[Node[A]] = semiauto.eq

    implicit def orderListTree[A: Order]: Order[List[Tree[A]]] =
      new Order[List[Tree[A]]] {
        @tailrec
        def compare(xs: List[Tree[A]], ys: List[Tree[A]]): Int = {
          (xs, ys) match {
            case (Nil, Nil) => 0
            case (Nil, _) => -1
            case (_, Nil) => 1
            case (x :: xs1, y :: ys1) =>
              Order[Tree[A]].compare(x, y) match {
                case 0 => compare(xs1, ys1)
                case n => n
              }
          }
        }
      }

    implicit def orderTree[A: Order]: Order[Tree[A]] = (x: Tree[A], y: Tree[A]) => {
      (x, y) match {
        case (Node(xLabel, xSub), Node(yLabel, ySub)) =>
          (xLabel.compare(yLabel), xSub.compare(ySub)) match {
            case (0, subComparison) => subComparison
            case (labelComparison, _) => labelComparison
          }
      }
    }

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Node(rootLabel, subForest) => Node(f(rootLabel), subForest.map(map(_)(f)))
      }
    }

    implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
      override def pure[A](value: A): Node[A] = Node(value, Nil)

      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
        @tailrec
        def loop(t: List[Tree[Either[A, B]]], done: List[Node[B]]): List[Node[B]] = {
          t match {
            case Nil => done
            case Node(Left(a), children) :: rest => loop(f(a) :: children ::: rest, done)
            case Node(Right(b), children) :: rest => loop(children ::: rest, Node(b, Nil) :: done)
          }
        }

        loop(List(f(a)), Nil).headOption.getOrElse(Node(throw new Exception("No root element found"), Nil))
      }

      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
        fa match {
          case Node(rootLabel, subForest) => f(rootLabel) match {
            case Node(r_, sf_) => Node(r_, sf_ ++ subForest.map(a => flatMap(a)(f)))
          }
        }
      }
    }

    implicit val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
      override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = {
        fa match {
          case Node(rootLabel, subForest) =>
            val b1 = f(b, rootLabel)
            subForest.foldLeft(b1)((b2, tree) => foldLeft(tree, b2)(f))
        }
      }

      override def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        fa match {
          case Node(rootLabel, subForest) =>
            val lb1 = f(rootLabel, lb)
            subForest.foldRight(lb1)((tree, lb2) => foldRight(tree, lb2)(f))
        }
      }
    }
  }
}
