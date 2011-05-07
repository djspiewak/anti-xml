package com.codecommit.antixml.util

private[antixml] class LazyVector[S, +A] private (body: Vector[A], tail: Vector[A], state: S, f: S => Option[(S, A)]) {
  
  def apply(i: Int): (A, LazyVector[S, A]) = {
    if (i < body.length) {
      (body(i), this)
    } else {
      val shifted = i - body.length
      
      def gen(state: S): Stream[(S, A)] =
        f(state) map { case tuple @ (state2, _) => tuple #:: gen(state2) } getOrElse Stream.Empty
      
      val stream = gen(state) take shifted
      val body2 = body ++ (stream map { case (_, a) => a })
      
      if (i < body2.length) {
        val (state2, back) = stream(shifted)
        (back, new LazyVector(body2, tail, state2, f))
      } else {
        lazy val finalState = stream.lastOption map { case (s, _) => s } getOrElse state
        val shifted2 = i - body2.length
        
        if (shifted2 < tail.length)
          (tail(i), new LazyVector(body2 ++ tail, Vector(), finalState, f))       // we're basically done at this point
        else
          throw new IndexOutOfBoundsException(i.toString)
      }
    }
  }
  
  def updated[B >: A](i: Int, b: B): LazyVector[S, B] = null
  
  def +:[B >: A](b: B): LazyVector[S, B] =
    new LazyVector(b +: body, tail, state, f)
  
  def :+[B >: A](b: B): LazyVector[S, B] =
    new LazyVector(body, tail :+ b, state, f)
  
  def ++[B >: A](that: LazyVector[S, B]): LazyVector[S, B] = null
  
  def map[B](f: A => B): LazyVector[S, B] = {
    val body2 = body map f
    val tail2 = tail map f
    val f2 = this.f andThen { _ map { case (s, a) => (s, f(a)) } }
    new LazyVector(body2, tail2, state, f2)
  }
  
  def flatMap[B](f: A => LazyVector[S, B]): LazyVector[S, B] = null
  
  def collect[B](pf: PartialFunction[A, B]): LazyVector[S, B] = null
  
  def force: Vector[A] = null
}

private[antixml] object LazyVector {
  def apply[S, A](init: S)(f: S => Option[(S, A)]): LazyVector[S, A] =
    new LazyVector(Vector(), Vector(), init, f)
}
