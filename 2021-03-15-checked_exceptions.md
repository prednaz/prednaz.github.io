---
title: Checked Exceptions in Haskell
lang: en
---

All Haskell examples were compiled with Stack [@stack] using the
`lts-16.31` resolver.

We want to represent the potential content types through the type of
the sum type. We must only represent the set of potential content
types because if the type level representation is sensitive to order,
sum types potentially containing the same set of content types would
not unify. There is only a unique way to serialize a set if there is
an order defined on the elements but I cannot see a way to define an
order on Haskell types within Haskell. Therefore, sets of types cannot
be uniquely serialized and therefore not represented at the type level
such that equal sets correspond to equal types.

The plucky approach attempts to solve this impasse by using type
variables which are subject to constraints. Because Haskell's type
constraints do behave exactly like sets. For example, the types `(Num
a, Ord a) => a`{.haskell} and `(Ord a, Num a) => a` are the same. The
type constraint corresponding to an exception type ought to be
discharged when the `catch`{.haskell} function is applied to
potentially catch that exception type.

## Approach: well-typed to-do

## Approach: plucky to-do

## Approach: to-do with fixed run time representation

With the plucky approach, `catch`{.haskell} discharged type
constraints by specializing the type variable. But if you
`catch`{.haskell} in different orders, the type variable will be
specialized in different orders, necessarily resulting in distinct
concrete types. These distinct types corresponded to distinct run time
representations. This lead to the loss of sharing.

Let us try if we can fix the problem of loss of sharing by ensuring
that the order of catching cannot influence the run time
representation of the open sum type. A *phantom type variable* is a
type variable that occurs on the left-hand side of a data type
definition but not on the right-hand side. Therefore, distinct
specializations of phantom type variables do not conflict with a fixed
run time representation. Furthermore specializing the phantom type
variables in the type of an expression to distinct types has no effect
on the expression's evaluation and therefore its value, if ad hoc
polymorphism [@strachey2000, 37] is avoided. Ad hoc polymorphism is
implemented by type classes in Haskell.

All code examples need the `TypeFamilies` language extension in
addition to the explicitly declared ones.

With this approach, the library would look as follows.

```haskell
{-# language
  DataKinds,
  ExistentialQuantification,
  PolyKinds,
  TypeOperators
#-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
```

Do *not* warn about redundant constraints because we explicitly want
to make `throw`{.haskell}'s and `makeOpenSum`{.haskell}'s type
signature stricter than necessary to connect `OpenSum`{.haskell}'s
phantom type variable to its content.

```haskell
module Library
  (
    OpenSum (),
    Elem,
    makeOpenSum,
    openSumFold,
    toEither,
    absurd,
    throw,
    catch,
    caughtAll,
  )
where
```

Do not export `OpenSum`{.haskell}'s data constructor forcing the
library user to use `throw`{.haskell} or `makeOpenSum`{.haskell} to
obtain an `OpenSum`{.haskell}. These functions guarantee
`OpenSum`{.haskell}'s invariant that its content type finds expression
in its phantom variable.

```haskell
import Data.Typeable (Typeable, cast)
import Unsafe.Coerce (unsafeCoerce)
import Data.Kind (Type)

data OpenSum list = forall value. Typeable value => OpenSum value
```

The potential content types will be tracked by a phantom type variable
`list`{.haskell}. This phantom type variable will store the potential
content types by means of a *type level list*, which are described by
[@yorgey2012giving] and provided by the `DataKinds` language
extension.

```haskell
type family Elem (element :: k) (list :: [k]) :: Type where
  Elem head (head ': _tail) = ()
  Elem element (_head ': tail) = Elem element tail

makeOpenSum ::
  (Elem element list ~ (), Typeable element) =>
  element -> OpenSum list
makeOpenSum = OpenSum

openSumFold ::
  Typeable head =>
  (head -> result) ->
  (OpenSum tail -> result) ->
  OpenSum (head ': tail) ->
  result
openSumFold f g openSum@(OpenSum value)
  | Just valueHead <- cast value = f valueHead
  | otherwise = g (unsafeCoerce openSum)

absurd :: OpenSum '[] -> void
absurd = error "OpenSum's parameter list cannot be empty."
```

This is where the *trusted kernel* of the library ends. It relies on
the `OpenSum`{.haskell} data constructor, using which can violate
`OpenSum`{.haskell}'s invariant and functions like
`unsafeCoerce`{.haskell} and `error`{.haskell}. It implements an open
sum type, upon which checked exceptions can be built in different
ways. We will implement checked exceptions through the simple
`Either`{.haskell} monad for demonstration purposes. But
implementations via the `Control.Monad.Except.ExceptT`{.haskell} monad
transformer [@mtl_control_monad_except] or more abstractly the
`Control.Monad.Except.MonadError`{.haskell} type class
[@mtl_control_monad_except] are possible in external modules only
using the safe functions the *trusted kernel* exports too. When
turning this into a fully fledged library it would make sense to put
even the following `Either`{.haskell}-monad-based checked exceptions
implementation into a different module.

```haskell
throw ::
  (Elem exception list ~ (), Typeable exception) =>
  exception -> Either (OpenSum list) result
throw exception = Left (makeOpenSum exception)

catch ::
  Typeable head =>
  Either (OpenSum (head ': tail)) result ->
  (head -> Either (OpenSum tail) result) ->
  Either (OpenSum tail) result
catch expression handler =
  either (openSumFold handler Left) Right expression

caughtAll ::
  Either (OpenSum '[]) result -> result
caughtAll = either absurd id
```

The library ends here.

Here is the usage example.

```haskell
{-# language
  PolyKinds,
  ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
```

*Do* warn about redundant constraints because this will warn us about
imprecise *exceptions declarations* as we will find out in section
to-do.

```haskell
import Library

data Exception1 = Exception1
data Exception2 = Exception2

thrower1 ::
  (Elem Exception1 exceptions ~ ()) =>
  Either (OpenSum exceptions) Bool
thrower1 = throw Exception1

thrower2 ::
  (
    Elem Exception1 exceptions ~ (),
    Elem Exception2 exceptions ~ ()
  ) =>
  Either (OpenSum exceptions) Bool
thrower2 =
  do
    result1 <- thrower1
    if result1
      then pure False
      else throw Exception2

catcher1 ::
  forall exceptions.
  (Elem Exception2 exceptions ~ ()) =>
  Either (OpenSum exceptions) Bool
catcher1 =
  thrower2 `catch` handler
  where
    handler :: Exception1 -> Either (OpenSum exceptions) Bool
    handler Exception1 = throw Exception2 -- (1)

catcher2 :: Bool
catcher2 =
  caughtAll (catcher1 `catch` handler)
  where
    handler :: Exception2 -> Either (OpenSum exceptions) Bool
    handler Exception2 = Right False
```

Notice that in line (1) the `handler`{.haskell} is itself allowed to
throw the checked exceptions declared in its type signature. In this
example its type signature does not seem to declare any
exceptions. `handler`{.haskell} throwing `Exception2`{.haskell} is
still well-typed because `catcher1`{.haskell} already declares in its
type signature that the type variable `exceptions`{.haskell} contains
`Exception2`{.haskell}. And `handler`{.haskell}'s type signature
refers to the same `exceptions`{.haskell} owing to the language
extension `ScopedTypeVariables`{.haskell}. So `handler`{.haskell} may
throw exactly those exceptions that `catcher1`{.haskell} may
throw. This is convinient in this case because `handler`{.haskell}
throwing exceptions that `catcher1`{.haskell} does not declare for
itself would be a *type error* anyway because applying some handler
via `Library.catch`{.haskell} might of course throw any exception that
that handler might throw.

### Imprecise *exceptions declarations*

Let us write an imprecise *exceptions declaration*, declaring that
exceptions might be thrown that cannot actually be thrown.

```haskell
thrower1 ::
  (
    Elem Exception1 exceptions ~ (),
    Elem Exception2 exceptions ~ ()
  ) =>
  Either (OpenSum exceptions) Bool
thrower1 = throw Exception1
```

This does not make the *exceptions declarations* of any of our binders
like `thrower1`{.haskell}, `thrower2`{.haskell},
`catcher1`{.haskell},... incorrect because `thrower1`{.haskell}, the
*exceptions declaration* of which we changed, is only used by
`thrower2`{.haskell}, which declares throwing the added
`Exception2`{.haskell} already anyway.

As stated before, we would prefer that we cannot implicitly extend the
declared exceptions, making the type imprecise, weakening the type,
losing information at the type level. Fortunately, GHC offers warnings
for redundant constraints, which fulfills our preference perfectly.

```
app/Main.hs:14:1: warning: [-Wredundant-constraints]
    • Redundant constraint: Elem Exception2 exceptions ~ ()
    • In the type signature for:
           thrower1 :: forall (exceptions :: [*]).
                       (Elem Exception1 exceptions ~ (),
                        Elem Exception2 exceptions ~ ()) =>
                       Either (OpenSum exceptions) Bool
```

### Sharing

This approach to-do currently suffers from the same problem of
loss of sharing as the plucky approach to-do. But the test case
that we used to confirm the problem for the plucky approach is not
enough to demonstrate this. So here is an similar but more complex
test case.

```haskell
import Data.OpenSum
import Debug.Trace (trace)

data Exception1 = Exception1
data Exception2 = Exception2

main :: IO ()
main =
  let
    handle12 :: Integer
    handle12 =
      caughtAll ((expensive `catch` handle1) `catch` handle2)
    handle21 :: Integer
    handle21 =
      caughtAll ((expensive `catch` handle2) `catch` handle1)
  in
    print (handle12 == handle21)

expensive ::
  (
    Elem Exception1 exceptions ~ (),
    Elem Exception2 exceptions ~ ()
  ) =>
  Either (OpenSum exceptions) Integer
expensive = trace "expensive" (fib 2)

fib ::
  (Elem Exception1 exceptions ~ ()) =>
  Integer -> Either (OpenSum exceptions) Integer
fib n
  | 0 <- n = Right 0
  | 1 <- n = Right 1
  | n > 1 = (+) <$> fib (n-1) <*> fib (n-2)
  | otherwise = throw Exception1

handle1 :: Exception1 -> Either exceptions Integer
handle1 Exception1 = Right 1
handle2 :: Exception2 -> Either exceptions Integer
handle2 Exception2 = Right 2
```

The output confirms the problem.

```
expensive
expensive
True
```

Similar as with the plucky approach, `handle12`{.haskell} and
`handle21`{.haskell} specialized `expensive`{.haskell}'s type to
`Either (OpenSum (Exception1 ': Exception2 ': '[])) Integer` and
`Either (OpenSum (Exception2 ': Exception1 ': '[])) Integer`
respectively. Because the only difference is confined to phantom type
variables and ad hoc polymorphism was avoided, we hoped that the
compiler would realize that `expensive`{.haskell} evaluates to the
same value in both cases and maintains sharing.

We will now discuss what obstructs sharing.

One of GHC's first compilation steps is to compile Haskell to the core
language. A tiny functional, statically typed language implementing a
variant of System F [@tolmach2001] [@core]. Almost all optimizations
that the compiler performs are implemented as transformations of the
core language represenation [@core_to_core].


We implemented the test case that we used to confirm that the plucky
approach (to-do) evaluates redundantly for this approach (to-do).

```haskell
import Data.OpenSum
import Debug.Trace (trace)

data Exception1 = Exception1
data Exception2 = Exception2

main :: IO ()
main =
  let
    handle12 :: Integer
    handle12 =
      caughtAll ((expensive `catch` handle1) `catch` handle2)
    handle21 :: Integer
    handle21 =
      caughtAll ((expensive `catch` handle2) `catch` handle1)
  in
    print (handle12 == handle21)

expensive ::
  (Elem Exception1 exceptions ~ (), Elem Exception2 exceptions ~ ()) =>
  Either (OpenSum exceptions) Integer
expensive = trace "expensive" (throw Exception1)

handle1 :: Exception1 -> Either exceptions Integer
handle1 Exception1 = Right 1
handle2 :: Exception2 -> Either exceptions Integer
handle2 Exception2 = Right 2
```

Like with the plucky approach to-do the application of
`catch`{.haskell} in `handle12`{.haskell} and `handle21`{.haskell} Unlike with the pluck approach (to-do), The output is as desired.

```
expensive
True
```

This seems to demonstrate that 

We will examine the GHC core language [@core] output to understand why
and how either the library code or the compiler's optimizer need to
change to guarantee sharing. `expensiveNoInline`{.haskell}'s
core representation looks as follows before optimization.

```
expensiveNoInline_ruz ::
  forall err.
  (Elem Exception1 err, Elem Exception2 err) =>
  Either (OpenSum err) Integer
expensiveNoInline_ruz =
  \(@ err) (irred :: Elem Exception1 err) _ ->
  trace
    (@ (Either (OpenSum err) Integer))
    (unpackCString# "expensiveNoInline"#)
    (fib (@ err) irred 34)

```

`expensiveNoInline`{.haskell} does not take any arguments. But
`expensiveNoInline`{.haskell}'s core representation takes three
arguments before optimization. The first one is called *type
argument*. The second and third ones are called *System FC coercion*
arguments [@sulzmann2007]. So the result of applying `fib`{.haskell}
to `34`{.haskell} is under a lambda abstraction and therefore not
memoized. Small lambda expressions like this are usually inlined
during optimization. This ruins any chance of sharing that might later
arise because the optimizer manages to float free expressions out from
under the lambda abstraction [@jones1997]. But that is not immediately
possible [@core_to_core] and inlining often unlocks powerful
optimizations. 

But if inlining is prohibited using the `noInline` pragma, the
following might happen.

```
lvl11 :: forall err. Either (OpenSum err) Integer
lvl11 =
  \(@ err) ->
  trace
    (@ (Either (OpenSum err) Integer))
    lvl9
    (lvl10 (@ err))

$wexpensiveNoInline ::
  forall err. Void# -> Either (OpenSum err) Integer
$wexpensiveNoInline =
  \(@ err) _ -> lvl11 (@ err)
```

The optimizer has noticed that `fib`{.haskell} does not use the
*System FC coercion* argument. Therefore, the body of
`expensiveNoInline`{.haskell}'s core language representation
does not use the coercion argument. Therefore, it can be floated out
into its own top level binding `lvl11`. `lvl11` is technically still a
lambda but its only argument is a *type* argument. A type argument
serves the only purpose of type checking polymorphic functions in the
GHC core language. It will disappear in STG, as GHC flag `-ddump-stg`
shows, and does not prevent sharing.