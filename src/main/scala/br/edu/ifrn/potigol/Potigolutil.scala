/*
 *  Potigol
 *  Copyright (C) 2015-2016  Leonardo Lucena
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/**
 *   _____      _   _             _
 *  |  __ \    | | (_)           | |
 *  | |__) |__ | |_ _  __ _  ___ | |
 *  |  ___/ _ \| __| |/ _` |/ _ \| |
 *  | |  | (_) | |_| | (_| | (_) | |
 *  |_|   \___/ \__|_|\__, |\___/|_|
 *                     __/ |
 *                    |___/
 *
 * @author Leonardo Lucena (leonardo.lucena@ifrn.edu.br)
 */

package br.edu.ifrn.potigol

import collection.generic.CanBuildFrom
import collection.mutable.{ Seq => MSeq }
import io.StdIn
import util.{ Failure, Success, Try }

object Potigolutil {
  private[this] val since094 = "0.9.4"
  // Tipos
  type Text = String
  type Integer = Int
  type Number = Double
  //type Número = Number
  type Logic = Boolean
  //type Logic = Logic
  type Real = Double
  type Character = Char
  type Matrix[T] = Lista[Lista[T]]
  type Cube[T] = Lista[Lista[Lista[T]]]
  type Nothing = Unit

  var $cor = false

  implicit class PotigolStr(ctx: StringContext) {
    def bool(a: Any) = a match {
      case false => "false"
      case true  => "true"
      case _     => a
    }
    def p(args: Any*): String = ctx.standardInterpolator(a => a, args.map(bool))
  }

  // valores
  val true = true
  val false = false

  // Expressões Regulares
  private[this] val intRE = """-?\d+""".r
  private[this] val numRE = """-?(\d*)(\.\d*)?""".r

  def lista[A](n: Integer)(valor: => A) = Lista.apply(n, valor)
  def matrix[A](i: Integer, j: Integer)(valor: => A): Matrix[A] = Matrix.apply(i, j, valor)
  def cube[A](i: Integer, j: Integer, k: Integer)(valor: => A): Cube[A] = Cube.apply(i, j, k, valor)

  trait Collection[T] {
    val _lista: Seq[T]
    def apply(a: Int): T = _lista(a)
    def length: Int = _lista.length
    override def toString: String = _lista.mkString("[", ", ", "]")
    def join(separator: Text = ""): Text = _lista.mkString(separator)
    def join(inicio: Text, separator: Text, fim: Text): Text = _lista.mkString(inicio, separator, fim)
    def size: Integer = _lista.length
    def get(a: Int): T = if (a > 0) apply(a - 1) else apply(size + a)
    def position(elem: T): Integer = _lista.indexOf(elem, 0) + 1
    def head: T = _lista.head
    def contains(a: T): Logic = _lista.contains(a)
    def last: T = _lista.last
    def inject[A >: T](f: (A, T) => A): A = _lista.reduceLeft(f)
    def inject[A](neutro: A)(f: (A, T) => A): A = _lista.foldLeft(neutro)(f)
    def find(p: T => Logic): Option[T] = _lista.find(p)
    //def contém: T => Logic = contains
    //def cabeça: T = head
    //def primeiro: T = head
    //def último: T = last
    //def posição: T => Integer = position
    //def posiçao: T => Integer = position
    //def posicão: T => Integer = position
    def to_list: Lista[T] = Lista(_lista.toList)
    def list: Lista[T] = to_list
    //def mutable: Vetor[T] = Vetor(_lista.to)
    //def mutável: Vetor[T] = mutable
    //def imutável = lista
    def immutable = list
    def divida_quando(f: (T, T) => Logic): Matrix[T] = Lista(_lista.foldRight(List.empty[Lista[T]]) { (a, b) =>
      if (b.isEmpty || f(a, b.head.head)) Lista(List(a)) :: b else (a :: b.head) :: b.tail
    })
  }

  case class Lista[T](val _lista: List[T]) extends IndexedSeq[T] with Collection[T] {
    def cauda: Lista[T] = Lista(_lista.tail)
    def ordene(implicit ord: Ordering[T]): Lista[T] = Lista(_lista.sorted)
    def inverta: Lista[T] = Lista(_lista.reverse)
    @deprecated("Use 'selecione'", since094) def filtre: (T => Logic) => Lista[T] = selecione _
    def selecione(p: T => Logic): Lista[T] = Lista(_lista.filter(p))
    def mapeie[B](f: T => B): Lista[B] = Lista(_lista.map(f))
    def pegue_enquanto(p: T => Logic): Lista[T] = Lista(_lista.takeWhile(p))
    @deprecated("Use 'descarte_enquanto'", since094) def passe_enquanto: (T => Logic) => Lista[T] = descarte_enquanto _
    def descarte_enquanto(p: T => Logic): Lista[T] = Lista(_lista.dropWhile(p))
    @deprecated("Use 'descarte'", since094) def passe: Integer => Lista[T] = descarte _
    def descarte(a: Integer): Lista[T] = Lista(_lista.drop(a))
    def pegue(a: Integer): Lista[T] = Lista(_lista.take(a))
    def +(outra: Lista[T]): Lista[T] = Lista(_lista ::: outra._lista)
    def ::[A >: T](a: A): Lista[A] = Lista(a :: _lista)
    def remova(i: Integer): Lista[T] = Lista(_lista.take(i - 1) ::: _lista.drop(i))
    def insira(i: Integer, valor: T): Lista[T] = Lista(_lista.take(i - 1) ::: valor :: _lista.drop(i - 1))
    def zip[A](outra: Collection[A]): Lista[(T, A)] = Lista(this._lista.zip(outra._lista))
    def zip(outra: Text): Lista[(T, Character)] = Lista(this._lista.zip(outra))
  }

  object Lista {
    def apply[A]: (Integer, => A) => Lista[A] = immutable
    def mutable[A](x: Integer, valor: => A): Vetor[A] = Lista(List.fill(x)(valor)).mutable
    def immutable[A](x: Integer, valor: => A): Lista[A] = Lista(List.fill(x)(valor))
    def vazia[A](x: A): Lista[A] = Lista(List.empty[A])
    def imutável[A]: (Integer, => A) => Lista[A] = immutable
    def mutável[A]: (Integer, => A) => Vetor[A] = mutable
  }

  object Matrix {
    def apply[A]: (Integer, Integer, => A) => Matrix[A] = immutable
    def mutable[A](x: Integer, y: Integer, valor: => A): Vetor[Vetor[A]] = {
      Lista.mutable(x, Lista.mutable(y, valor))
    }
    def immutable[A](x: Integer, y: Integer, valor: => A): Matrix[A] = {
      Lista.immutable(x, Lista.immutable(y, valor))
    }
    def imutável[A]: (Integer, Integer, => A) => Matrix[A] = immutable
    def mutável[A]: (Integer, Integer, => A) => Vetor[Vetor[A]] = mutable
  }

  object Cube {
    def apply[A]: (Integer, Integer, Integer, => A) => Cube[A] = immutable[A] _
    def mutable[A](x: Integer, y: Integer, z: Integer, valor: => A): Vetor[Vetor[Vetor[A]]] = {
      Lista.mutable(x, Lista.mutable(y, Lista.mutable(z, valor)))
    }
    def immutable[A](x: Integer, y: Integer, z: Integer, valor: => A): Cube[A] = {
      Lista.immutable(x, Lista.immutable(y, Lista.immutable(z, valor)))
    }
    def imutável[A]: (Integer, Integer, Integer, => A) => Cube[A] = immutable
    def mutável[A]: (Integer, Integer, Integer, => A) => Vetor[Vetor[Vetor[A]]] = mutable
  }

  case class Vetor[T](_lista: MSeq[T]) extends collection.mutable.IndexedSeq[T] with Collection[T] {
    override def update(ind: Int, elem: T): Unit = { _lista.update(ind, elem) }
    def cauda: Vetor[T] = Vetor(_lista.tail)
    def inverta: Vetor[T] = Vetor(_lista.reverse)
    def ordene(implicit ord: Ordering[T]): Vetor[T] = Vetor(_lista.sorted)
    @deprecated("Use 'selecione'", since094) def filtre: (T => Logic) => Vetor[T] = selecione _
    def selecione(p: T => Logic): Vetor[T] = Vetor(_lista.filter(p))
    def mapeie[B: Manifest](f: T => B): Vetor[B] = Vetor(_lista.map(f))
    def pegue(a: Integer): Vetor[T] = Vetor(_lista.take(a))
    def descarte(a: Integer): Vetor[T] = Vetor(_lista.drop(a))
    def pegue_enquanto(p: T => Logic): Vetor[T] = Vetor(_lista.takeWhile(p))
    @deprecated("Use 'descarte_enquanto'", since094) def passe_enquanto: (T => Logic) => Vetor[T] = descarte_enquanto _
    def descarte_enquanto(p: T => Logic): Vetor[T] = Vetor(_lista.dropWhile(p))
    def remova(i: Integer): Vetor[T] = Vetor(_lista.take(i - 1) ++ _lista.drop(i))
    def insira(i: Integer, valor: T): Vetor[T] = Vetor(_lista.take(i - 1) ++ List(valor) ++ _lista.drop(i - 1))
    def +(outra: Collection[T]): Vetor[T] = Vetor(_lista ++ outra._lista)
    def zip[A](outra: Collection[A]): Vetor[(T, A)] = Vetor(this._lista.zip(outra._lista))
    def zip(outra: Text): Vetor[(T, Character)] = Vetor(this._lista.zip(outra))
  }

  implicit class Texts(val _lista: String) {
    private[this] val ZERO = "0"
    @deprecated("Use 'integer'", since094) def para_int: Integer = integer
    @deprecated("Use 'integer'", since094) def para_i: Integer = integer
    @deprecated("Use 'integer'", since094) def para_integer: Integer = integer
    def integer: Integer = {
      if (_lista == null) 0 else
        (intRE.findPrefixOf(_lista).getOrElse(ZERO)).toInt
    }
    def get(a: Int): Character = if (a > 0) _lista(a - 1) else _lista(size + a)
    def position(elem: Character): Integer = _lista.indexOf(elem, 0) + 1
    @deprecated("Use 'real'", since094) def para_number: Real = real
    def maiusculo: Text = _lista.toUpperCase()
    def minusculo: Text = _lista.toLowerCase()
    def divida(s: Text = " "): Lista[Text] = Lista(_lista.replaceAll("( |\\n)+", " ").split(s).toList)
    def divida_quando(f: (Character, Character) => Logic): Lista[Text] = Lista((_lista.foldRight(List.empty[Lista[Character]]) { (a, b) =>
      if (b.isEmpty || f(a, b.head.head)) Lista(List(a)) :: b else (a :: b.head) :: b.tail
    }).map(_.join("")))
    def contains(a: Character): Logic = _lista.contains(a)
    def head: Character = _lista.head
    def last: Character = _lista.last
    def cauda: Text = _lista.tail
    def size: Integer = _lista.length
    def inverta: Text = _lista.reverse
    def filtre(a: Character => Logic): Text = _lista.filter(a)
    def selecione: (Character => Logic) => Text = filtre
    def maiúsculo: Text = maiusculo
    def minúsculo: Text = minusculo
    def inject[A >: Character](f: (A, Character) => A): A = _lista.reduceLeft(f)
    def inject[A](neutro: A)(f: (A, Character) => A): A = _lista.foldLeft(neutro)(f)
    def mapeie[B, That](f: Character => B)(implicit bf: CanBuildFrom[String, B, That]): That = _lista.map(f)
    def find(p: Character => Logic): Option[Character] = _lista.find(p)
    def pegue_enquanto(p: Character => Logic): Text = _lista.takeWhile(p)
    @deprecated("Use 'descarte_enquanto'", since094) def passe_enquanto: (Character => Logic) => Text = descarte_enquanto _
    def descarte_enquanto(p: Character => Logic): Text = _lista.dropWhile(p)
    def lista: Lista[Character] = Lista(_lista.toList)
    def join(separator: Text = ""): Text = _lista.mkString(separator)
    def join(inicio: Text, separator: Text, fim: Text): Text = _lista.mkString(inicio, separator, fim)
    def ordene: Text = _lista.sorted
    def descarte(n: Integer): Text = _lista.drop(n)
    def pegue(n: Integer): Text = _lista.take(n)
    def remova(i: Integer): Text = _lista.take(i - 1) + _lista.drop(i)
    def insira(i: Integer, valor: Character): Text = _lista.take(i - 1) + valor + _lista.drop(i - 1)
    def insira(i: Integer, valor: Text): Text = _lista.take(i - 1) + valor + _lista.drop(i - 1)
    def contém: Character => Logic = contains
    def cabeça: Character = head
    def primeiro: Character = head
    def último: Character = last
    @deprecated("Use 'real'", since094) def para_num: Real = real
    @deprecated("Use 'real'", since094) def para_n: Real = real
    @deprecated("Use 'real'", since094) def para_real: Real = real
    def real: Real = {
      if (_lista == null) 0 else
        (numRE.findPrefixOf(_lista).getOrElse(ZERO)).toDouble
    }
    def posição: Character => Integer = position
    def posiçao: Character => Integer = position
    def posicão: Character => Integer = position
    val qual_tipo = "Text"
    def -(s: Text): Text = _lista.diff(s)
  }

  implicit class reals(x: Double) {
    def arredonde: Integer = x.round.toInt
    def arredonde(n: Integer): Real = {
      val precisao = Math.pow(10, n)
      (x * precisao).round / precisao
    }
    def integer: Integer = x.toInt
    def real: Real = x
    def piso: Real = x.floor
    def teto: Real = x.ceil
    val qual_tipo = "Real"
  }

  implicit class Integers(x: Int) {
    def character: Character = x.toChar
    def integer: Integer = x
    def real: Real = x.toDouble
    val qual_tipo = "Integer"
  }

  implicit class Todos[T <: Any](x: T) {
    def format(fmt: Text): Text = Try {
      fmt.formatLocal(java.util.Locale.US, x)
    } match {
      case Success(s) => s
      case Failure(_) => "An error has ocurred while formatting"
    }

    def %(fmt: Text): Text = format(fmt)
    @deprecated("Use 'text'", since094) def para_text: Text = text
    def text: Text = x.toString
    def qual_tipo: Text = x match {
      case a: Integer  => "Integer"
      case a: Real     => "Real"
      case a: Logic   => "Logic"
      case a: Text    => "Text"
      case a: Lista[T] => "Lista"
      case a: Vetor[T] => "Vetor"
      case a: Product  => "Tupla"
      case a           => a.getClass.getSimpleName.takeWhile(_ != '$')
    }
  }

  private[this] def corSim = print("\u001b[32m")
  private[this] def corNao = print("\u001b[37m")
  def read(): Text = {
    //if ($cor) corSim
    val s = StdIn.readLine()
    //if ($cor) corNao
    s
  }

  def read(separator: Text): Lista[Text] = Lista(read
    .split(separator.toCharArray())
    .toList) //  .filterNot(_ == "")

  def read(n: Integer): Lista[Text] = Lista({
    for { i <- 1 to n } yield { read }
  }.toList)

  def read_text: Text = read
  def read_texts(n: Integer): Lista[Text] = read(n)
  def read_texts(separator: Text): Lista[Text] = read(separator)

  def read_integer: Integer = read.integer
  def read_integers(n: Integer): Lista[Integer] = {
    var l = Lista.vazia(0)
    while (l.size < n) {
      l = l + read_integers(" ")
    }
    l.pegue(n)
    //    Lista(((1 to n) map { _ => read_int }).toList)
  }
  def read_integers(separator: Text): Lista[Int] = {
    val l = read(separator)._lista
    Lista(l.map(_.integer))
  }
  @deprecated("Use 'read_integer'", since094) def read_int: Integer = read_integer
  @deprecated("Use 'read_integers'", since094) def read_ints(n: Integer): Lista[Integer] = read_integers(n)
  @deprecated("Use 'read_integers'", since094) def read_ints(separator: Text): Lista[Integer] = read_integers(separator)

  def read_real: Real = read.real
  @deprecated("Use 'read_real'", since094) def read_number: Real = read_real
  def read_reals(n: Integer): Lista[Real] = {
    var l = Lista.vazia(0.0)
    while (l.size < n) {
      l = l + read_reals(" ")
    }
    l.pegue(n)
    //    Lista(((1 to n) map { _ => read_num }).toList)
  }
  def read_reals(separator: Text): Lista[Real] = Lista(read(separator)._lista.map { _.real })
  @deprecated("Use 'read_reals'", since094) def read_numbers(n: Integer): Lista[Real] = read_reals(n)
  @deprecated("Use 'read_reals'", since094) def read_numbers(separator: Text): Lista[Real] = read_reals(separator)
  @deprecated("Use 'read_real'", since094) def read_num: Real = read_real
  @deprecated("Use 'read_reals'", since094) def read_nums(n: Integer): Lista[Real] = read_reals(n)
  @deprecated("Use 'read_reals'", since094) def read_nums(separator: Text): Lista[Real] = read_reals(separator)

  def write(text: Any): Unit = {
    if ($cor) corNao
    text match {
      case true  => Console.println("true")
      case false => Console.println("false")
      case _     => Console.println(text.toString)
    }
  }
  def print(text: Any): Unit = {
    if ($cor) corNao
    text match {
      case true  => Console.print("true")
      case false => Console.print("false")
      case _     => Console.print(text.toString)
    }
  }

  implicit class Tupla2[T1, T2](t: (T1, T2)) {
    def primeiro = t._1
    def segundo = t._2
    def qual_tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo})"
  }

  implicit class Tupla3[T1, T2, T3](t: (T1, T2, T3)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo})"
  }

  implicit class Tupla4[T1, T2, T3, T4](t: (T1, T2, T3, T4)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo})"

  }

  implicit class Tupla5[T1, T2, T3, T4, T5](t: (T1, T2, T3, T4, T5)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def quinto = t._5
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo}, ${t._5.qual_tipo})"

  }

  implicit class Tupla6[T1, T2, T3, T4, T5, T6](t: (T1, T2, T3, T4, T5, T6)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def quinto = t._5
    def sexto = t._6
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo}, ${t._5.qual_tipo}, ${t._6.qual_tipo})"

  }

  implicit class Tupla7[T1, T2, T3, T4, T5, T6, T7](
      t: (T1, T2, T3, T4, T5, T6, T7)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def quinto = t._5
    def sexto = t._6
    def setimo = t._7
    def sétimo = t._7
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo}, ${t._5.qual_tipo}, ${t._6.qual_tipo}, ${t._7.qual_tipo})"
  }

  implicit class Tupla8[T1, T2, T3, T4, T5, T6, T7, T8](
      t: (T1, T2, T3, T4, T5, T6, T7, T8)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def quinto = t._5
    def sexto = t._6
    def setimo = t._7
    def sétimo = t._7
    def oitavo = t._8
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo}, ${t._5.qual_tipo}, ${t._6.qual_tipo}, ${t._7.qual_tipo}, ${t._8.qual_tipo})"
  }

  implicit class Tupla9[T1, T2, T3, T4, T5, T6, T7, T8, T9](
      t: (T1, T2, T3, T4, T5, T6, T7, T8, T9)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def quinto = t._5
    def sexto = t._6
    def setimo = t._7
    def sétimo = t._7
    def oitavo = t._8
    def nono = t._9
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo}, ${t._5.qual_tipo}, ${t._6.qual_tipo}, ${t._7.qual_tipo}, ${t._8.qual_tipo}, ${t._9.qual_tipo})"
  }

  implicit class Tupla10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
      t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) {
    def primeiro = t._1
    def segundo = t._2
    def terceiro = t._3
    def quarto = t._4
    def quinto = t._5
    def sexto = t._6
    def setimo = t._7
    def sétimo = t._7
    def oitavo = t._8
    def nono = t._9
    def decimo = t._10
    def décimo = t._10
    def tipo = s"(${t._1.qual_tipo}, ${t._2.qual_tipo}, ${t._3.qual_tipo}, ${t._4.qual_tipo}, ${t._5.qual_tipo}, ${t._6.qual_tipo}, ${t._7.qual_tipo}, ${t._8.qual_tipo}, ${t._9.qual_tipo}, ${t._10.qual_tipo})"
  }

  case class URL(caminho: Text) {
    lazy val erro = conteudo == ""
    lazy val conteudo = Try {
      io.Source.fromURL(caminho).mkString("")
    } getOrElse ("")
  }

  import scala.io.Source

  object Arquivo {
    def read(caminho: Text): Lista[Text] = {
      Lista(Source.fromFile(caminho).getLines().toList)
    }
  }
}
