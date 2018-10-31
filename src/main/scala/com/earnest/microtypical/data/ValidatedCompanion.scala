package com.earnest.microtypical.data

import cats.Show

trait ValidatedCompanion [V, R, E] {
  implicit val model: Model [V, R, E]

  def apply (r: R): Either [E, V] = model apply r
  def applyOrShow (r: R) (implicit s: Show [E]): Either [String, V] = model applyOrShow r
  def applyUnsafe (r: R): V = model applyUnsafe r
  def preview (r: R): Option [V] = model preview r
  def review (v: V): R = model review v
  def validated (r: R): cats.data.Validated [E, V] = model validated r
  def validatedNel (r: R): cats.data.ValidatedNel [E, V] = model validatedNel r
}

object ValidatedCompanion {
  def from [V, R, E] (_model: Model [V, R, E]): ValidatedCompanion [V, R, E] = new ValidatedCompanion [V, R, E] {
    override implicit val model: Model [V, R, E] = _model
  }

  trait Poly1 [V [_], R [_], E] {
    implicit def model [A]: Model [V [A], R [A], E]

    def apply [A] (r: R [A]): Either [E, V [A]] = model apply r
    def applyOrShow [A] (r: R [A]) (implicit s: Show [E]): Either [String, V [A]] = model applyOrShow r
    def applyUnsafe [A] (r: R [A]): V [A] = model applyUnsafe r
    def preview [A] (r: R [A]): Option [V [A]] = model preview r
    def review [A] (v: V [A]): R [A] = model review v
    def validated [A] (r: R [A]): cats.data.Validated [E, V [A]] = model validated r
    def validatedNel [A] (r: R [A]): cats.data.ValidatedNel [E, V [A]] = model validatedNel r
  }

  trait Poly1Typeclass1 [V [_], R [_], T [_], E] {
    implicit def model [A] (implicit t: T [A]): Model [V [A], R [A], E]

    def apply [A] (r: R [A]) (implicit t: T [A]): Either [E, V [A]] = model apply r
    def applyOrShow [A] (r: R [A]) (implicit s: Show [E], t: T [A]): Either [String, V [A]] = model applyOrShow r
    def applyUnsafe [A] (r: R [A]) (implicit t: T [A]): V [A] = model applyUnsafe r
    def preview [A] (r: R [A]) (implicit t: T [A]): Option [V [A]] = model preview r
    def review [A] (v: V [A]) (implicit t: T [A]): R [A] = model review v
    def validated [A] (r: R [A]) (implicit t: T [A]): cats.data.Validated [E, V [A]] = model validated r
    def validatedNel [A] (r: R [A]) (implicit t: T [A]): cats.data.ValidatedNel [E, V [A]] = model validatedNel r
  }

  trait Poly1Typeclass2 [V [_], R [_], T1 [_], T2 [_], E] {
    implicit def model [A] (implicit t1: T1 [A], t2: T2 [A]): Model [V [A], R [A], E]

    def apply [A] (r: R [A]) (implicit t1: T1 [A], t2: T2 [A]): Either [E, V [A]] = model apply r
    def applyOrShow [A] (r: R [A]) (implicit s: Show [E], t1: T1 [A], t2: T2 [A]): Either [String, V [A]] = model applyOrShow r
    def applyUnsafe [A] (r: R [A]) (implicit t1: T1 [A], t2: T2 [A]): V [A] = model applyUnsafe r
    def preview [A] (r: R [A]) (implicit t1: T1 [A], t2: T2 [A]): Option [V [A]] = model preview r
    def review [A] (v: V [A]) (implicit t1: T1 [A], t2: T2 [A]): R [A] = model review v
    def validated [A] (r: R [A]) (implicit t1: T1 [A], t2: T2 [A]): cats.data.Validated [E, V [A]] = model validated r
    def validatedNel [A] (r: R [A]) (implicit t1: T1 [A], t2: T2 [A]): cats.data.ValidatedNel [E, V [A]] = model validatedNel r
  }

  trait Poly2 [V [_, _], R [_, _], E] {
    implicit def model [A, B]: Model [V [A, B], R [A, B], E]

    def apply [A, B] (r: R [A, B]): Either [E, V [A, B]] = model apply r
    def applyOrShow [A, B] (r: R [A, B]) (implicit s: Show [E]): Either [String, V [A, B]] = model applyOrShow r
    def applyUnsafe [A, B] (r: R [A, B]): V [A, B] = model applyUnsafe r
    def preview [A, B] (r: R [A, B]): Option [V [A, B]] = model preview r
    def review [A, B] (v: V [A, B]): R [A, B] = model review v
    def validated [A, B] (r: R [A, B]): cats.data.Validated [E, V [A, B]] = model validated r
    def validatedNel [A, B] (r: R [A, B]): cats.data.ValidatedNel [E, V [A, B]] = model validatedNel r
  }

  trait Typeclass1 [V [_], T [_], E] {
    implicit def model [R] (implicit t: T [R]): Model [V [R], R, E]

    def apply [R] (r: R) (implicit t: T [R]): Either [E, V [R]] = model apply r
    def applyOrShow [R] (r: R) (implicit s: Show [E], t: T [R]): Either [String, V [R]] = model applyOrShow r
    def applyUnsafe [R] (r: R) (implicit t: T [R]): V [R] = model applyUnsafe r
    def preview [R] (r: R) (implicit t: T [R]): Option [V [R]] = model preview r
    def review [R] (v: V [R]) (implicit t: T [R]): R = model review v
    def validated [R] (r: R) (implicit t: T [R]): cats.data.Validated [E, V [R]] = model validated r
    def validatedNel [R] (r: R) (implicit t: T [R]): cats.data.ValidatedNel [E, V [R]] = model validatedNel r
  }

  trait Typeclass2 [V [_], T1 [_], T2 [_], E] {
    implicit def model [R] (implicit t1: T1 [R], t2: T2 [R]): Model [V [R], R, E]

    def apply [R] (r: R) (implicit t1: T1 [R], t2: T2 [R]): Either [E, V [R]] = model apply r
    def applyOrShow [R] (r: R) (implicit s: Show [E], t1: T1 [R], t2: T2 [R]): Either [String, V [R]] = model applyOrShow r
    def applyUnsafe [R] (r: R) (implicit t1: T1 [R], t2: T2 [R]): V [R] = model applyUnsafe r
    def preview [R] (r: R) (implicit t1: T1 [R], t2: T2 [R]): Option [V [R]] = model preview r
    def review [R] (v: V [R]) (implicit t1: T1 [R], t2: T2 [R]): R = model review v
    def validated [R] (r: R) (implicit t1: T1 [R], t2: T2 [R]): cats.data.Validated [E, V [R]] = model validated r
    def validatedNel [R] (r: R) (implicit t1: T1 [R], t2: T2 [R]): cats.data.ValidatedNel [E, V [R]] = model validatedNel r
  }

  trait Parametrized [V [_], R, T [_ <: C], C, E] {
    implicit def model [P <: C] (implicit p: T [P]): Model [V [P], R, E]

    def apply [P <: C] (r: R) (implicit p: T [P]): Either [E, V [P]] = model [P] apply r
    def applyOrShow [P <: C] (r: R) (implicit p: T [P], s: Show [E]): Either [String, V [P]] = model [P] applyOrShow r
    def applyUnsafe [P <: C] (r: R) (implicit p: T [P]): V [P] = model [P] applyUnsafe r
    def preview [P <: C] (r: R) (implicit p: T [P]): Option [V [P]] = model [P] preview r
    def review [P <: C] (v: V [P]) (implicit p: T [P]): R = model [P] review v
    def validated [P <: C] (r: R) (implicit p: T [P]): cats.data.Validated [E, V [P]] = model [P] validated r
    def validatedNel [P <: C] (r: R) (implicit p: T [P]): cats.data.ValidatedNel [E, V [P]] = model [P] validatedNel r
  }
}
