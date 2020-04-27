# Thoughts on Codata

## Motivation

So, for context, we need to establish that `HList` exists as a type of heterogeneous lists, meaning that it can have any number of different values of any number of different types in it. As is the norm, the `HList` is constructed by supplying members, for example: `3 :: 4 :: "foo" :: HNil` would be of type `Int :: Int :: String :: HNil`. Implementations of this are almost always hacking away at extremely high-level features of the source language's type system, meaning that they're pretty much considered rare beasts. They are **extremely** useful, however.

At some point during the previous week, the question of putting `HList` into a `List` arose, and it got me thinking about what happens when we put an instance of `HList` into a `List`, and it got me here:

>	A `List` over any `HList` instance somehow changes the instance itself, so that it can no longer behave as it did. Where any _data_ type `T` that you put into a `List` permits that type to fully behave as it would outside of the list, an `HList` instance is basically reduced to a tuple of the same types that were in it, for all intents and purposes. This is because we didn't *put* the `HList` _type_ into the `List` but rather only one of the instances of an `HList` (e.g. `Int :: HNil`). The true underlying type of an `HList` can properly be written only as an infinite sum of all possible combinations of types, permutations, and lengths before `HNil`. Passing `HList` (as a Scala trait) to a `List` is possible, but it loses all the information about the types held within the `HList`.

## Problem statement

In short, we can either hold only one single, fully-typed `HList` alternative in a `List`, or we can abstract away the type information about the `HList` and simply hold the trait within the `List` and then pattern match (totally) on the alternatives. This discrepancy made me think about what was special 
about the heterogeneous list that would make it behave like this. 

## Searching for the explicit shape of the problem

First, I noticed that the `HList` trait holds no information about the members of any specific `HList`. It gives a name to the family of types that make the `HList` type-set but it is nominal in nature. The type operator `::` that lets us construct members of `HList` is of the following type, given that T and U
are any two types of the default kind (*):

```scala
	given T, U : *. (T :: U) : HList
```

As I write this, I realize that I have just technically declared `HList` to be a kind (to be explicit, a type of types, as "kind", "type" and "sort" have different meanings in different fields), and the `T :: U` a way of combining types T and U from any kind into a type in this new kind. What about the terminator singleton `HNil`? There's nothing peculiar about `HNil`, and for all intents and purposes, it is selected arbitrarily as a nominal `Unit`-equivalent type to represent the end of our lists.

In practice, lifting `HList` to be a kind is done (simulated if we don't have higher-kinded types) by adding a type parameter to the type constructor of `::` that is constrainted to itself be an `HList`. In theory, it's done through fix-pointing on a coalgebra. Before you get scared, here's what a coalgebra is for us:

```scala
	type Coalgebra[F[_], A] = A => F[A]
```

Similarly, an algebra is:

```scala
	type Algebra[F[_], A] = F[A] => A
```

>	You can imagine that the `F[_]` as a collection of any kind (list, tree, generally mappable thing), and so the `Algebra` is a process of reconciling the parts of that collection into a singular value (algebra, from "al jabr - reunion of broken parts"); while a `Coalgebra` splits an `A` into a collection of similar parts (the "co-" prefix used to denote a dual nature). In reality, `F[_]` should always be a functor for the type to actually be a (co)algebra, meaning that `F[_]` has to be able to fill the following shape:

```scala
	map[F[_], A, B](fa: F[A])(f: A => B): F[B]
```

>	The `map` functionality gives us a way to generalize conversions between whatever domain is presented by `A` and `B`, meaning that it will apply totally regardless of value.

In our case, we have something of this kind:

```scala
	given A, B : *. Algebra[::[_], (A, B)] : HList = (A, B) => A :: B
```

This is _precisely_ our `HList` list constructor. It can be hard to think about this, because `F[A] => A` looks more correct, given that we are feeding in a number of elements (one could almost say, a list), and getting out one single element. The point is that the resulting element is of a higher-kinded type, in our case `::`. 

Of course, Scala has to do this a bit differently as it doesn't natively support higher-kinded types and even other kinds, so we add the constraint to `B` instead, but that's a limitation of the compiler and typechecker, and not a change in paradigm. Okay, so now we know that our `HList` is built by a coalgebra. Is this weird? No, coalgebras are almost precisely 50% of all constructions. It's just important to note what kind it is, because certain things become easier as we go on.

What constraints did figuring this out add to our `HList`, if any? For one, our list constructor `::[_]` has to be a functor now, so the following has to be implicitly doable (I've expanded to tuples to maintain readability):

```scala
	map[A, B](fa: F[(A, B)])(f: (A, B) => (C, D)): F[(C, D)]
```

We know that this is possible, because we implemented an `HList` member-wise, so that the second part of the tuple behaves like the tail of the list. If that's true, then `B` itself is similarly a functor with the same property, and we can then prove that `map` is implementable by induction on the length of `B` (with a base case of `HNil`). Of course, in the implementation, `D` is also constrained by the same thing as `B`, but that's beside the point.

## Towards Codata

After finding out that `HList` is constructed by a coalgebra, my mind went to it being more easily observable as codata. Codata and data aren't exclusive to one another, but are rather like different views on the same thing. They have strict rules about them **if** you want to use them in productive ways, and they require certain things (like laziness for codata) to be useful. 

In short, both are _types_. Data is always finite and constructed in a terminating way (otherwise it's useless), while codata can be infinite and the creation of it can be non-terminating (using laziness to stay useful). Data is constructed from a finite set of parts and can be distructed using recursive methods to break into smaller pieces and evaluate until we reach a base case, while codata starts from a small seed and builds up using corecursion (otherwise known as generation). Because of this, codata is usually self-referential in some regard, as every next bit is built up from the previous one. This isn't always the case, but in a theoretically completely pure case, it would be. 

Examples should spring to mind: Peano arithmetic natural numbers generate from a base case of 0 and are infinite. The result of any Prolog query that yields an infinite dataset as a result by building upon a grammar. So many more...

In type theory terms, the `introduction` mechanism for data is the **named data constructor** which takes a number of parameters, and the `elimination` mechanism is **pattern matching**. The `introduction` for codata is its **generator** (corecursion), and `elimination` is done by **pattern matching** on a single pattern that was built.

>	Corecursion can have a stupid generator as well, for example. It could build only a single instance and then stop (or repeat that instance forever). I'll separate this into a separate form of introduction, even though it really is just degenerate, called **instance-defining**. Instance-defined codata looks like a data constructor, and I think that's the basic culprit for all the confusion. Codata is much larger than data, by construction, and separate piece of codata, if one matches them, could indeed _be_ data.

In a more abstract sense, introducing data hides and abstracts away while enabling recursion as a way of processing that data without looking at the details. In a similar way, introducing codata presents the evolving instances derived from a process that was explained in the abstract (for example, what a grammar looks like and how to build complex expressions) by using corecursion, while enabling us to ever only match a single case of those instances at a time by using pattern matching.

>	"But wait", you might say. "Look at this recursive function, undoubtedly used to deal with eliminating data."

```haskell
	sum [] = 0
	sum (head : tail) = head + sum(tail)
```

>	"It does pattern matching on the argument, and it's defining recursion. That doesn't make any sense with what you've just said!"

That's right, you've got it, functions that we use to write recursions, that use pattern matching; they are codata too! Functions, pattern matching in general, are codata mechanisms and as such, they are first-order codata constructs in the language. Every function (except for template metaprogramming, for example, which generates functions based on a template call) is an instance-defined generator that generates only then and there, and only what we've written. It pattern-matches one of its cases and then we get to see only that result or path of execution, after which the function itself is gone, eliminated.

## Why `HList` is codata?

Well, for starters, we always have to type the value to construct it. It lacks a proper constructor, and we have to basically write it all out to carry all of its type-value information onwards. We construct a `HNil` because we need to have a marker to end it, but it basically constitutes a nested generator (written in some cool pseudo-type language):

```scala
	gen[T]: for some U. HList[T, gen[U]]
```

The second giveaway was its usage in the example at hand, which is high-level type pattern-matching via unification. It's not as explicit as Scala syntax-level pattern matching, but the only way the `HList` types are used is by passing them around, matching and then constructing new ones using the old ones. All of this smells like codata.

>	As I've said already, something isn't _just_ data or codata. You can look at any data type as both, it's just that one of them is easier to work with than the other for any specific case.

Furthermore, the inciting incident was the fact that if we wrap `HList` into any container that is undoubtedly built to work like data, we lose the codata-nature of the `HList`, and it boils down to a tuple of elements frozen in data-space. The moment we extract it from the data, we can use it again, but while inside, its type follows the rules of standard data and it cannot exhibit its more unique properties. My motivation for solving this was built around the fact that a strange behaviour was happening, and it is now clear: being that data (in both value and type) is less free to shape itself than codata (one might even say that this _freedom_ moves from being a "statically determined shape" for data to "statically determined growth" for codata), any nesting of codata in data resolves it down to its underlying _data_ type. In the case of `HList`, a `Tuple` of specific types. This isn't unknown in general, getting a whole chapter in one of the seminal work about codata, referenced [here](https://www.cs.uoregon.edu/Reports/MS-201806-Sullivan.pdf), in chapter 4.1 "Flattening codata", where it reads:

> "We notice a point of asymmetry between data and codata, [...]. Copatterns are expressive enough to contain patterns, but patterns cannot contain copatterns. This means that whereas we can express a function A → B as codata we cannot express its dual, the subtraction connective A − B, which would involve embedding a copattern in a pattern."

This fact shows me that in many regards, we implicitly feel that it is natural (very freely used here) to use data _in_ our programs, and codata for fixed program structure constructs, parsers, generators and typecheckers. Almost all of these structures are more easily expressed as codata, but we never need to think about it that way, because we are working with them on a instance-defined basis, and being codata, they start off with exposed internals, so it's easy to disregard what the _whole_ is. We do not care, as long as it works. Caring about it might bring us some benefits, however.

# Caring is (co)sharing

Observe the following function:

```scala
	(A => F[A]) => A => Fix[F]
```

This is a general `unfold` operation. It takes a coalgebra `(A => F[A])`, a seed element `A`, and then unfolds from that seed a `Fix` (so basically, a structural recursion) of the functor `F`. In our case, the functor is the `HList`. The seed element could be `HNil`, and the `A => F[A]`, we already saw, is our list constructor `::`!

>	This kind of general unfold (unfolds A into an underlying structure) is called an `anamorphism`.

The dual to this function would be:

```scala
	Fix[F] => (F[A] => A) => A
```

This is a general `fold`, taking a structurally (`F`) recursed (`Fix`) object of type `Fix[F]`, a fold of one step of recursion into a single value of type `A`, and then stepping through the process until we get a single `A`.

>	A general of this type is called a `catamorphism`.

This means that once we've corecursively built up a `Fix[F]` (for example using our `gen()` function from above), we could apply a random algebra to bring it down, simplify it into, say a boolean. Something like:

```scala
      (A => F[A]) => A => Fix[F] 
                  |
                  |
  map A => Bool through F <: Functor[_]
                  |
                  v
  Fix[F] => (F[Bool] => Bool) => Bool
```

If you imagine interesting types for A, and interesting structures for F, you might call this a bytecode emitter plus typechecker, with very bad error messaging! That isn't too far away either, it just requires a bit more talk about different `*morphisms`. ;)

Thank you for the time and I hope there's some nice discussions to come from this!
