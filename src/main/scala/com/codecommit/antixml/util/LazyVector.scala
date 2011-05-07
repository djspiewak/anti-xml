package com.codecommit.antixml.util

private[antixml] class LazyVector[S, +A] private[antixml] (body: Vector[A], tail: Vector[A], state: S, f: S => Option[(S, A)]) {
  def apply(i: Int): (A, LazyVector[S, A]) = null
  
  def updated[B >: A](i: Int, b: B): LazyVector[S, B] = null
  
  def +:[B >: A](b: B): LazyVector[S, B] = null
  
  def :+[B >: A](b: B): LazyVector[S, B] = null
  
  def ++[B >: A](that: LazyVector[S, B]): LazyVector[S, B] = null
  
  def map[B](f: A => B): LazyVector[S, B] = null
  
  def flatMap[B](f: A => LazyVector[S, B]): LazyVector[S, B] = null
  
  def collect[B](pf: PartialFunction[A, B]): LazyVector[S, B] = null
  
  def force: Vector[A] = null
}

private[antixml] object LazyVector {
  def apply[S, A](init: S)(f: S => (S, A)): LazyVector[S, A] = null
}
