/*
 *  Potigol
 *  Copyright (C) 2015-2016 by Leonardo Lucena
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

object Erros {

  private[this] object C {
    val Char = "Char"
    val String = "String"
    val Int = "Int"
    val Boolean = "Boolean"
    val Double = "Double"
    val Inteiro = "Integer"
    val Real = "Real"
    val Texto = "Text"
    val Logico = "Logic"
    val Caractere = "Caracter"
  }

  private[this] object Erro {
    val naoDeclarado = "not found: value (\\S+).*".r
    val parametroAusente = "not enough arguments for method (\\S+)\\:.+Unspecified value parameter (\\S).*".r
    val parametroMais = "too many arguments for method (\\S+)\\:(.+)\\Z.*".r
    val tipoIndefinido = "not found: type (\\S+).*".r
    val tipoDiferente = "type mismatch.+required: (?:\\S*\\.)?([^| .]+) .*".r
    val parametroTipo = "(?:type|class) (\\S+) takes type parameters .*".r
    val variavelJaExiste = "(\\S+) is already defined as variable .+".r
    val alterarValorConstante = "reassignment to val \\| (\\S+).*".r
    val valorJaDeclarado = "(\\S+) is already defined as value.*".r
    val funcaoJaDefinida = "method (\\S+) is defined twice .*".r
    val funcaoRecursivaSemTipo = "recursive method (\\S+) needs result type.*".r
    val matrizNaoDeclarada = "value (?:get|update) is not (\\S+) member of.*".r
    val metodoNaoExiste = "value (\\S+) is not a member of (?:\\S*\\.)?(\\S+) .*".r
    val semParametros = "(.+) does not take parameters.*".r
    val divisaoTipoErrado = "overloaded method value.*".r
  }

  private[this] object Msg {
    def valorNaoDeclarado(a: String): String = s"'${a}' value not declared."
    def tipoNaoPossuiMetodo(tipo: String, a: String): String =
      s"'${tipo}' type values cannot use method '${a}'."
    def tipoErrado(a: String): String =
      s"Wrong type.\nI was expecting a value with '${a}' type ."
    def faltaParametro(a: String, b: String): String =
      s"The '${a}' function needs more parameters.\nYou forgot to enter parameter '${b}'."
    def parametrosMais(n: Int): String =
      s"You entered more parameters than needed.\nEnter ${n} parameter(s) only."
    def semParametros(a: String): String = s"a value ${a} must not have any parameters."
  }

  private[this] def contar(b: String): Int = b.count(_ == ':')

  private[this] def mensagens(s: String): String = {

    val erro = s.replace("\n", " | ")
    erro match {
      case Erro.naoDeclarado(a) => Msg.valorNaoDeclarado(a)
      case Erro.parametroAusente(a, b) => Msg.faltaParametro(a, b)
      case Erro.parametroMais("apply", b) => Msg.parametrosMais(contar(b))
      case Erro.parametroMais(a, b) if contar(b) == 0 => s"The '${a}' function does not need parameters."
      case Erro.parametroMais(a, b) if contar(b) == 1 => s"The '${a}' function only needs 1 parameter."
      case Erro.parametroMais(a, b) => s"The '${a}' function only needs ${contar(b)} parameters."
      case Erro.tipoIndefinido(a) => s"The '${a}' type does not exist.\nWouldn't that be 'Integer', 'Real' or 'Text'?"
      case Erro.tipoDiferente(C.Int) => Msg.tipoErrado(C.Inteiro)
      case Erro.tipoDiferente(C.Double) => Msg.tipoErrado(C.Real)
      case Erro.tipoDiferente(C.String) => Msg.tipoErrado(C.Texto)
      case Erro.tipoDiferente(C.Boolean) => Msg.tipoErrado(C.Logico)
      case Erro.tipoDiferente(a) => Msg.tipoErrado(a)
      case Erro.parametroTipo(a) => s"'${a}' needs a type.\nCould it be '${a}[Integer]' or '${a}[Text]'?"
      case Erro.variavelJaExiste(a) => s"The variable '${a}' is already set.\nIf you wnat to modify the value of '${a}' use ':=' instead of '='."
      case Erro.alterarValorConstante(a) => s"'${a}' is a constant value, it cannot be altered."
      case Erro.valorJaDeclarado(a) => s"Value '${a}' has already been declared.\nUse another name."
      case Erro.funcaoJaDefinida(a) => s"A function named '${a}' already exists.\nUse another name."
      case Erro.funcaoRecursivaSemTipo(a) => s"The recursive function '${a}' needs to define the return value type."
      case Erro.metodoNaoExiste(a, C.Int) => Msg.tipoNaoPossuiMetodo(C.Inteiro, a)
      case Erro.metodoNaoExiste(a, C.Double) => Msg.tipoNaoPossuiMetodo(C.Real, a)
      case Erro.metodoNaoExiste(a, C.String) => Msg.tipoNaoPossuiMetodo(C.Texto, a)
      case Erro.metodoNaoExiste(a, C.Boolean) => Msg.tipoNaoPossuiMetodo(C.Logico, a)
      case Erro.metodoNaoExiste("get", C.Char) => Msg.tipoNaoPossuiMetodo(C.Caractere, "[ ]")
      case Erro.metodoNaoExiste(a, C.Char) => Msg.tipoNaoPossuiMetodo(C.Caractere, a)
      case Erro.metodoNaoExiste(a, b) => Msg.tipoNaoPossuiMetodo(b, a)
      case Erro.matrizNaoDeclarada(a) => s"The variable '${a}' is not a mutable list."
      case Erro.semParametros(C.Int) => Msg.semParametros(C.Inteiro)
      case Erro.semParametros(C.Double) => Msg.semParametros(C.Real)
      case Erro.semParametros(C.String) => Msg.semParametros(C.Texto)
      case Erro.semParametros(C.Boolean) => Msg.semParametros(C.Logico)
      case Erro.semParametros(a) => Msg.semParametros(a)
      case Erro.divisaoTipoErrado() => Msg.tipoErrado(C.Real)
      case _ => erro
    }
  }
  def traduzir(texto: String): String = {
    val inicio = texto.indexOf("error:", 0)
    val s = texto.drop(inicio).split('^')(0).split(": ", 3).drop(1)(1)
    mensagens(s)
  }

  def texto(erro: String): String = {
    traduzir(erro)
  }
}
