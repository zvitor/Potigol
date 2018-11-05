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

import Potigolutil.{ Collection, Integer, Real }

object Matematica {
  def sin(a: Real): Real = Math.sin(a)
  def cos(a: Real): Real = Math.cos(a)
  def tan(a: Real): Real = Math.tan(a)
  def arcsin(a: Real): Real = Math.asin(a)
  def arccos(a: Real): Real = Math.acos(a)
  def arctan(a: Real): Real = Math.atan(a)
  def abs(a: Real): Real = Math.abs(a)
  def abs(a: Integer): Integer = Math.abs(a)
  def sqroot(a: Real, b: Real = 2.0): Real = Math.pow(a, 1.0 / b)
  val PI: Real = Math.PI
  def log(a: Real): Real = Math.log(a)
  def log10(a: Real): Real = Math.log10(a)
  def random(): Real = Math.random()
  //def random: Real = random
  def random(last: Integer): Integer = random(1, last)
  def random(first: Integer): Integer = random(first)
  def random(first: Integer, last: Integer): Integer = {
    val range = last - first + 1
    (Math.random() * range).toInt + first
  }
  def random(first: Integer, last: Integer): Integer = random(first, last)
  def random[T](lista: Collection[T]): T = lista.get(random(lista.tamanho))
  def random[T](lista: Collection[T]): T = random(lista)
}
