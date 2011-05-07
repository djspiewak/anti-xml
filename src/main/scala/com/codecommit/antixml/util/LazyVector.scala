package com.codecommit.antixml.util

private[antixml] class LazyVector[S, +A] private[antixml] (
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
  
  def ++[B >: A](that: LazyVector[S, B]): LazyVector[S, B] = {
    if (tail.isEmpty && that.body.isEmpty) {
      val f2: S => Option[(S, B)] = { s =>
        f(s) orElse that.f(s)
      }
      
      new LazyVector(body, that.tail, state, f2)
    } else {
      new LazyVector(force ++ that.body, that.tail, that.state, that.f)
    }
  }
  
  def ++[B >: A](that: Vector[B]): LazyVector[S, B] =
    new LazyVector(body, tail ++ that, state, f)
  
  def map[B](f: A => B): LazyVector[S, B] = {
    val body2 = body map f
    val tail2 = tail map f
    val f2 = this.f andThen { _ map { case (s, a) => (s, f(a)) } }
    new LazyVector(body2, tail2, state, f2)
  }
  
  def flatMap[B](f: A => LazyVector[S, B]): LazyVector[S, B] = null
  
  def collect[B](pf: PartialFunction[A, B]): LazyVector[S, B] = null
  
  def force: Vector[A] = extend(Int.MaxValue).body
  
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
  
  override def equals(a: Any) = a match {
    case that: LazyVector[S, A] =>
      this.body == that.body && this.tail == that.tail && this.state == that.state && this.f == that.f
      
    case _ => false
  }
  
  override def hashCode = body.hashCode + tail.hashCode + state.hashCode + f.hashCode
  
  override def toString = "LazyGroup(%s, %s, %s, %s)".format(body, tail, state, f)
}

private[antixml] object LazyVector {
  def apply[S, A](init: S)(f: S => Option[(S, A)]): LazyVector[S, A] =
    new LazyVector(Vector(), Vector(), init, f)
}
