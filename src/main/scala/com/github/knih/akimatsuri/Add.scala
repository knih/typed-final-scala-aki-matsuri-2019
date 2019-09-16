package com.github.knih.akimatsuri

import cats.~>

import scala.language.higherKinds
import scala.quoted.Type


//---------------------------------------------------------
// Symantics
//---------------------------------------------------------

trait AddSym {
  type Repr[T]

  def int(n: Int): Repr[Int]
  def add(t1: Repr[Int], t2: Repr[Int]): Repr[Int]

  type Obs[T]

  def observe[A](f: () => Repr[A]): Obs[A]
}

trait AddMulSym extends AddSym {
  def mul(t1: Repr[Int], t2: Repr[Int]): Repr[Int]
}


trait LamSym {
  type Repr[T]

  def lam[A: Type, B: Type](f: Repr[A] => Repr[B]): Repr[A => B]

  def app[A, B](t1: Repr[A => B], t2: Repr[A]): Repr[B]

  type Obs[T]

  def observe[A](f: () => Repr[A]): Obs[A]
}


trait AddMulLamSym extends AddMulSym with LamSym


//---------------------------------------------------------
// User programs
//---------------------------------------------------------

class Program[S <: AddMulSym](val sym: S) {
  import sym._

  val ex1 = add(int(1), int(0))
  val ex2 = mul(int(2), int(1))


  val ex1Res = observe(() => ex1)
  val ex2Res = observe(() => ex2)
}


//---------------------------------------------------------
// Interpreters
//---------------------------------------------------------

object Run {

  // type Id[A] = A

  case class R[A](unR: A)

  object interp extends AddSym {
    override type Repr[T] = R[T]

    override def int(n: Int): R[Int] = R(n)
    override def add(t1: R[Int], t2: R[Int]): R[Int] = R(t1.unR + t2.unR)

    override type Obs[T] = T
    override def observe[A](f: () => Repr[A]): A = f().unR
  }

  object rInterp extends AddMulSym {
    override type Repr[T] = R[T]
    override type Obs[T] = T


    override def int(n: Int): R[Int] = R(n)

    override def add(t1: R[Int], t2: R[Int]): R[Int] = R(t1.unR + t2.unR)
    override def mul(t1: R[Int], t2: R[Int]): R[Int] = R(t1.unR * t2.unR)

    override def observe[A](f: () => Repr[A]): A = f().unR
  }

  object rInterpAbs extends AddMulLamSym {
    override type Repr[T] = R[T]
    override type Obs[T] = T

    override def int(n: Int): R[Int] = R(n)

    override def add(t1: R[Int], t2: R[Int]): R[Int] = R(t1.unR + t2.unR)
    override def mul(t1: R[Int], t2: R[Int]): R[Int] = R(t1.unR * t2.unR)

    override def lam[A: Type, B: Type](f: R[A] => R[B]): R[A => B] = R((x: A) => f(R(x)).unR)
    override def app[A, B](t1: R[A => B], t2: R[A]): R[B] = R(t1.unR(t2.unR))

    override def observe[A](f: () => Repr[A]): A = f().unR
  }

}



trait RR {
  import cats.~>

  type From[_]
  type To[_] // Term

  def fwd: From ~> To // reflection
  def bwd: To ~> From // reification

  def map[A, B](f: From[A] => From[B]): To[A] => To[B] =
    (t: To[A]) => fwd(f(bwd(t)))

  def map2[A, B, C](f: (From[A], From[B]) => From[C]): (To[A], To[B]) => To[C] =
    (t1: To[A], t2: To[B]) => fwd(f(bwd(t1), bwd(t2)))

}


class GenericTrans[FS <: AddMulSym](val rr: RR)(val sym: FS {type Repr[T] = rr.From[T]}) extends AddMulSym {

  import rr._

  override type Repr[T] = To[T]
  override type Obs[T] = sym.Obs[T]

  override def int(n: Int): To[Int] = fwd(sym.int(n))
  override def add(t1: To[Int], t2: To[Int]): To[Int] = map2(sym.add _)(t1, t2)
  override def mul(t1: To[Int], t2: To[Int]): To[Int] = map2(sym.mul _)(t1, t2)

  override def observe[A](f: () => To[A]): sym.Obs[A] = sym.observe(() => bwd(f()))
}


// AdditiveIdentity
class AddIdent[FS <: AddMulSym](val sym: FS) {

  sealed trait Term[T]
  final case class Unknown[A](x: sym.Repr[A]) extends Term[A]
  final case class IntLit(e: Int) extends Term[Int]
  final case class Add(e1: Term[Int], e2: Term[Int]) extends Term[Int]

  object AddRR extends RR {
    override type From[T] = sym.Repr[T]
    override type To[T] = Term[T]

    def fwd: From ~> To = new (From ~> To) {
      def apply[A](x: sym.Repr[A]): Term[A] = Unknown(x)
    }

    def bwd: To ~> From = new (To ~> From) {
      def apply[A](term: Term[A]): sym.Repr[A] = term match {
        case Unknown(x) => x
        case IntLit(n) => sym.int(n)
        case Add(e1, e2) => sym.add(bwd(e1), bwd(e2))
      }
    }
  }

 val addIdentInterp  = new GenericTrans(AddRR)(sym) {
    def add(t1: Term[Int], t2: Term[Int]): Term[Int] = (t1, t2) match {
      case (IntLit(0), _) => t2
      case (_, IntLit(0)) => t1
      case (_, _)         => Add(t1, t2)
    }
  }
}

// MultiplicativeIdentity
class MulIdent[FS <: AddMulSym](val sym: FS) {

  sealed trait Term[T]
  final case class Unknown[A](x: sym.Repr[A]) extends Term[A]
  final case class IntLit(e: Int) extends Term[Int]
  final case class Mul(e1: Term[Int], e2: Term[Int]) extends Term[Int]

  object AddRR extends RR {
    override type From[T] = sym.Repr[T]
    override type To[T] = Term[T]

    def fwd: From ~> To = new (From ~> To) {
      def apply[A](x: sym.Repr[A]): Term[A] = Unknown(x)
    }

    def bwd: To ~> From = new (To ~> From) {
      def apply[A](term: Term[A]): sym.Repr[A] = term match {
        case Unknown(x) => x
        case IntLit(n) => sym.int(n)
        case Mul(e1, e2) => sym.mul(bwd(e1), bwd(e2))
      }
    }
  }

  val mulIdentInterp = new GenericTrans(AddRR)(sym) {
    def mul(t1: Term[Int], t2: Term[Int]): Term[Int] = (t1, t2) match {
      case (IntLit(1), _) => t2
      case (_, IntLit(1)) => t1
      case (_, _)         => Mul(t1, t2)
    }
  }
}




object test {
  val evaluator = Run.rInterp
  val addident  = new AddIdent(evaluator).addIdentInterp
  val optimizer = new MulIdent(addident).mulIdentInterp

  val p1 = new Program(evaluator)
  val p2 = new Program(optimizer)

  def main(args: Array[String]) = {
    println("---------------------------------")
    println(p1.ex1Res)


    println("---------------------------------")
    println(p2.ex2Res)
  }
}


