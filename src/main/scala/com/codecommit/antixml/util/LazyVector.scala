package com.codecommit.antixml.util

private[antixml] class LazyVector[S, +A] private (
    private val body: Vector[A],
    private val tail: Vector[A],
    private val state: S,
    private val f: S => Option[(S, A)]) {
  
  def apply(i: Int): (A, LazyVector[S, A]) = {
    val this2 = extend(i)
    if (i < this2.body.length)
      (this2.body(i), this2)
    else
      throw new IndexOutOfBoundsException(i.toString)
  }
  
  def updated[B >: A](i: Int, b: B): LazyVector[S, B] = {
    val this2 = extend(i)
    val body2 = if (i < this2.body.length)
      this2.body.updated(i, b)
    else
      throw new IndexOutOfBoundsException(i.toString)
    
    new LazyVector(body2, this2.tail, this2.state, this2.f)
  }
  
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
  
  private def extend(i: Int) = {
    if (i < body.length) {
      this
    } else {
      val shifted = i - body.length
      
      def gen(state: S): Stream[(S, A)] =
        f(state) map { case tuple @ (state2, _) => tuple #:: gen(state2) } getOrElse Stream.Empty
      
      val stream = gen(state) take shifted
      val body2 = body ++ (stream map { case (_, a) => a })
      lazy val state2 = stream.lastOption map { case (s, _) => s } getOrElse state
      
      if (i < body2.length)
        new LazyVector(body2, tail, state2, f)
      else
        new LazyVector(body2 ++ tail, Vector(), state2, f)       // we're basically done at this point
    }
  }
}

private[antixml] object LazyVector {
  def apply[S, A](init: S)(f: S => Option[(S, A)]): LazyVector[S, A] =
    new LazyVector(Vector(), Vector(), init, f)
}
