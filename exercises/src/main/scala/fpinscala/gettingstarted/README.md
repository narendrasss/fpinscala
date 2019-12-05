# 02. Getting Started With FP in Scala

## Modules, objects, and namespaces

In order to call the `abs` method, we had to do `MyModule.abs`.

* `MyModule` is considered its *namespace*.
* Every value in Scala is an *object*, and each object may have zero or more *members*.
* A member is either a method declared with `def`, or another object declared with `val` or `object`.

Even an expression like `2 + 1` is calling a member of an object.

* This is syntatic sugar for `2.+(1)`.
* Because of this, any method name can be used like above, e.g. `MyModule abs 42`.

## Functional loops

Loops are written functionally, i.e. without mutating a loop variable, through recursion.

Normally this can get problematic because recursion leads to high call stack memory use. But in Scala, as long as the recursive call is in *tail position*, the compiler converts it to a `while` loop.

> A recursive call is in tail position if the caller does nothing other than return the value of the recursive call.

The following recursive function is *not* in tail position as we do something with the value of the recursive call:

```scala
def factorial(n: Int): Int = {
	def go(n: Int): Int = 
		if (n <= 0) 1
		else n * go(n - 1)
	go(n)
}
```

On the other hand, the following recursive function *is* in tail position as it only returns the value of the recursive call:

```scala
def factorial(n: Int): Int = {
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n - 1, n * acc)
  go(n, 1)
}
```

* Scala provides the `tailrec` annotation which tells the compiler to throw an error if it cannot convert the recursive call to a `while` loop:

```scala
def factorial(n: Int): Int = {
	@annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n - 1, n * acc)
  go(n, 1)
}
```

## Higher-order functions

A higher order function is a function that accepts other functions as arguments. With this we can extract common functionality by passing a function as a parameter.

Since we have a `formatAbs` and `formatFactorial` function that does similar things, we can refactor to a `formatResult` higher-order function that accepts `abs` or `factorial` as a parameter:

```scala
def formatResult(name: String, n: Int, f: Int => Int) = {
	val msg = "The %s of %d is %d"
	msg.format(name, n, f(n))
}
```

We can then do something like:

```
scala> formatResult("absolute value", -42, abs)
res0: String = "The absolute value of -42 is 42"
```

## Polymorphic functions

Our previous HOF can only operate on `Int`s.

**Polymorphic functions** are higher order functions that accepts functions with *arbitrary* types. To write a polymorphic method, we add a list of *type parameters* following the name of the function:

```scala
def findFirst[A](as: Array[A], p: A => boolean): Int
```

* `A` is called a *type variable* and the compiler will ensure that all of its references have the same type.

## Functions as values

In Scala, a function is just an object that has an `apply` method:

```scala
val lessThan = new Function2[Int, Int, Boolean] {
	def apply(a: Int, b: Int) = a < b
}
```

Thus calling a function is only syntatic sugar for calling its `apply` method:

```scala
lessThan.apply(10, 20) // same as lessThan(10, 20)
```

## Function composition

Scala's standard library provides `compose` as a method in `Function1`. This means that we can do `f compose g` to perform `f(g(x))`.

* `Function1` also has a `andThen` method which is `compose` but in the opposite direction e.g. `f andThen g` is the same as `g compose f`.
