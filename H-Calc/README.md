
So, you want to write an interpreter for your own Domain Specific Language (DSL), in Haskell...  Then, clone H-Calc and start editing it.  H-Calc is a showcase of some of the best Haskell technologies to write a DSL interpreter, showing you that you can :

- parse source code in your DSL with ease, and build the corresponding Abstract Syntax Tree (AST), thanks to [megaparsec](http://hackage.haskell.org/package/megaparsec) ([tutorial](https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html))
- start with a simple interpreter for the core features of your DSL, then add extensions in a modular way (each new feature in its own Haskell module), thanks to [Open Algebraic Datatype](https://hackage.haskell.org/package/open-ADT)  ([tutorial](https://woehr.github.io/open-adt/open-adt-tutorial-1.0/Data-OpenADT-Tutorial.html)) (see an Introduction below)
- transform the AST efficiently, without boilerplate, thanks to [Recursion-scheme](http://hackage.haskell.org/package/recursion-schemes-5.0.3) ([tutorial](https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/))
- embed your DSL in Haskell, thanks to [RebindableSyntax](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide)
- easily use the best functions and libraries of Haskell, thanks to [Relude](http://hackage.haskell.org/package/relude)<sup>[1](#myfootnote1)</sup>
- easily format values into Text string, using [Fmt](http://hackage.haskell.org/package/fmt)
- organize your test suite, thanks to [HSpec](http://hackage.haskell.org/package/hspec), with [hspec-discover](http://hackage.haskell.org/package/hspec-discover)
- automatically run your test suite as soon as you save a program file, provided it has no error, thanks to [Ghcid](https://github.com/ndmitchell/ghcid)

H-Calc is a toy example, a calculator that can add integers or floats.  It also supports multiplication by repeated additions: it transforms `n*a` into `a+a+...` (n times), after applying the distribution rule recursively: `a*(b+c) = a*b + a*c`.  `n` must be a positive integer.

    2 * (3 + 4) -> ((2 * 3) + (2 * 4)) -> ((3 + 3) + (4 + 4)) -> 14

The evaluation pipeline has these steps:

* parse the text containing the formula into the AST
* annotate the AST with type information, and report any type error
* transform the AST in several "nanopass"
* finally evaluate the simplified AST to get the desired result

# Get started

* Install [Stack](haskellstack.org)
* clone H-Calc from github to a directory on your machine
* go to that directory
~~~~
dir> stack build
dir> stack install ghcid
~~~~
To automatically run Main.hs whenever your program changes:
~~~~
dir> ghcid --command="stack ghci H-Calc" --test="main"
~~~~
To automatically run the test suite whenever your program changes:
~~~~
dir> ghcid --command="stack ghci H-Calc:lib H-Calc:spec" --test="main"
~~~~

# An introduction to Open ADT

## Abstract Syntax Tree

Our abstract syntax tree has the following core types of nodes, defined using recursive data constructors ([tutorial](https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/)):

```haskell
  data ValF      e =      ValF' e Int    deriving (Eq, Functor, Show)
  data FloatValF e = FloatValF' e Float  deriving (Eq, Functor, Show)
  data AddF      e =      AddF' e (e, e) deriving (Eq, Functor, Show)
```

We also have a node to represent an error detected when interpreting an H-Calc formula.

```haskell
  data HErrorF    e = HErrorF' e Text deriving (Eq, Functor, Show)
```

The first data element of each constructor will contain some annotations, and the second will contain the arguments.  Annotations are represented as a chain of annotation nodes.  The simplest annotation node is `EmptyNote`; another annotation node will contain the data type of the node (TInt or TFloat).

```haskell
  data EmptyNoteF e = EmptyNoteF'     deriving (Eq, Functor, Show)

  data TType = TInt | TFloat deriving (Show, Eq)
  data TypF       e = TypF' e TType   deriving (Eq, Functor, Show)
```

To facilitate the creation and handling of nodes, the open-adt package proposes `mkVarPattern`:

```haskell
  mkVarPattern ''ValF      "valF"      "Val"      "ValF"
  mkVarPattern ''FloatValF "floatValF" "FloatVal" "FloatValF"
  mkVarPattern ''AddF      "addF"      "Add"      "AddF"

  mkVarPattern ''HErrorF    "hErrorF"    "HError"    "HErrorF"
  mkVarPattern ''EmptyNoteF "emptyNoteF" "EmptyNote" "EmptyNoteF"
  mkVarPattern ''TypF       "typF"       "Typ"       "TypF"
```
We can now define the type of an AST tree:

```haskell
  type AST0F  = (("hErrorF"    .== HErrorF)
              .+ ("emptyNoteF" .== EmptyNoteF)
              .+ ("valF"       .== ValF)
              .+ ("floatValF"  .== FloatValF)
              .+ ("addF"       .== AddF))
  type AST0 = OpenADT AST0F
```

With this apparatus, we can now represent `1` and `(1+2)` with the following formula:

```haskell
    one :: AST0
    one = Val EmptyNote 1

    oneTwo :: AST0
    oneTwo = Add EmptyNote (Val EmptyNote 1, Val EmptyNote 2)
```

We can easily extend what our trees can represent by adding new types of nodes, for example a multiplication:

```haskell
  data MulF      e =      MulF' e (e, e) deriving (Eq, Functor, Show)
  mkVarPattern ''MulF      "mulF"      "Mul"      "MulF"

  mulTwo :: OpenADT (AST0F .+ ("mulF" .== MulF))
  mulTwo = Mul EmptyNote (Val EmptyNote 2, Val EmptyNote 1)
```

## Algebra

We can now define various algebra on our recursive data types, i.e. various functions to evaluate our tree to a fixed data type.  For example, `showAST` is a function to evaluate our tree as a Text value.

```haskell
    showAST :: OpenADT xs -> Text
```

Some algebra are extensible, tree-wide transformations: as new types of nodes are added, new definitions are required.  To do that, we use `Class` and `instance`:

```haskell
  class Algebra (f :: * -> *) where
    showAST' :: f Text -> Text
    -- add your algebra declaration here

  -- (additional code not shown for clarity. See Transfos.hs)

  showAST = cata showAST'

  instance Algebra AddF where
    showAST' (AddF' α (v1,v2)) = format "({} + {}){}" v1 v2 α
```

`cata` is a function for catamorphism, from the recursion-scheme package.  (`ShowAST` could also be implemented without it)  Check the source code for the instances of `showAST'` for the other node types.

The `eval` function is another algebra, from AST tree to Result.

```haskell
    data Result
      = RInt Int
      | RFloat Float
      | RError Text
```

`eval` is NOT supposed to be extensible: it accepts only a limited set of tree nodes.  Trees with other types of nodes will have to be simplified before being evaluated (see below).  For such non-extensible algebra, we define how to interpret each type of node using anonymous functions, called "continuations" (see Interpreter.hs):

```haskell
  eval :: AST0 -> Result
  eval l = caseonF r (unfix l) where
    r = #emptyNoteF   .== (\EmptyNoteF' -> RError "can't evaluate empty expression")
     .+ #hErrorF      .== (\(HErrorF' _ t) -> RError t)
     .+ #valF         .== (\(ValF' _ i) -> RInt i)
     .+ #floatValF    .== (\(FloatValF' _ f) -> RFloat f)
     .+ #addF         .== (\(AddF' _ (v1,v2)) ->
              case (eval v1, eval v2) of
                (RInt v1', RInt v2')     -> RInt (v1'+v2')
                (RFloat v1', RFloat v2') -> RFloat (v1'+v2')
                (RError e, _) -> RError e
                (_, RError e) -> RError e
                (a,b)         -> RError $ format "Error in eval({}+{})" (show a) (show b)
      )
```

## Expansions and isomorphisms

Often, you want to enrich a tree with some annotations, for example to add the data type of each sub-tree.  To expand the capability of a tree, use `diversifyF`:

```haskell
    one' = cata (Fix . diversifyF @("typF" .== TypF)) one
```

You can then fill the type using an isomorphism, i.e. a transformation from `EADT xs` to `EADT xs`: the list of possible nodes does not change, but the structure of the tree may very well.  Again, when we want to have an extensible, tree-wide isomorphism, we'll use `Class` and `instance` (see Transfos.hs):

```haskell
  class Isomorphism xs (f :: * -> *) where
    getAnnotation :: f (OpenADT xs) -> OpenADT xs
    setType'      :: f (OpenADT xs) -> OpenADT xs
    -- add more isomorphisms here

  setType = cata setType'
```

Here is the isomorphism definition for Val (see source code for more):

```haskell
  instance ( OpenAlg xs "typF" TypF (OpenADT xs)
           , OpenAlg xs "valF" ValF (OpenADT xs))
           => Isomorphism xs ValF where
    getAnnotation (ValF' α _) = α
    setType' (ValF' α i) = Val (Typ α TInt) i
```

`OpenAlg` is used to specify that `xs` must contain `TypF` and `ValF` constructors.

When the isomorphism is not extensible and tree-wide (i.e. it requires no new definition for new types of nodes) you should use continuations.  This is the case when we want to apply the distributivity rule on the tree (C_Mul.hs):

```haskell
    -- apply distribution : a*(b+c) -> (a*b+a*c)
  distribute x = case trialF (unfix x) #mulF of
      Right other -> Fix $ diversifyF @("mulF" .== MulF) $ fmap d other
      Left (MulF' α (v1,v2)) -> go α (v1,v2)
      where
        d = distribute
        go α (i, (Add β (v1,v2))) = Add β (d (Mul α (i,d v1)), d (Mul α (i,d v2)))
        go α ((Add β (v1,v2)), i) = Add β (d (Mul α (d v1,i)), d (Mul α (d v2,i)))
        go α (v1,v2)              = Mul α (d v1, d v2)
```

The Right branch defines a default implementation for the other types of node: just transform their children.  If you want to transform more than one type of node, use `multiTrialF`.


## Tree reductions

Another type of transformation you may want to make on your tree is to reduce the list of possible types of nodes.

For example, at some point, you'll want to remove the annotations, i.e. replace all of them by `EmptyNote`.  When the reduction is extensible and tree-wide, use `Class` and `instance` (see Transfos.hs):

```haskell
  class RemoveAnnotation ys (f :: * -> *) where
    removeAnnotation'      :: f (OpenADT ys) -> OpenADT ys

  removeAnnotation = cata removeAnnotation'

  -- (additional code omitted)

  instance (OpenAlg xs "emptyNoteF" EmptyNoteF (OpenADT xs)) => RemoveAnnotation xs TypF where
    removeAnnotation' (TypF' _ _) = EmptyNote
```

You can define a default instance implementation, e.g. for Add, Mul, Sub, using `{-# OVERLAPPABLE #-}`:

```haskell
    TODO
```

If instead, the tree reduction is not extensible and tree-wide, i.e. it transforms one type of node only, use continuations, as in `demultiply`:

```haskell
  demultiply :: AST2 -> AST1
  demultiply x = case trialF (unfix x) #mulF of
    Right other -> Fix $ fmap d other
    Left (MulF' α (v1,v2)) ->
      case (d v1, d v2) of
        (HError _ e, _) -> HError (d α) e
        (_, HError _ e) -> HError (d α) e
        (Val _ i1, v2') ->
                if  | i1 < 0 -> HError (d α) $
                                format "Error: can't multiply by negative number {}" i1
                    | i1 == 0 -> Val (d α) 0
                    | i1 == 1 -> v2'
                    | otherwise -> Add (d α) (d v2, d $ Mul α ((Val α $ i1-1), v2))
        (_, Val _ _) -> d (Mul α (v2,v1))
        (_, _) -> HError (d α) $ format "Can't multiply by {}" (showAST v1)
    where
      d = demultiply
```

## Other

You could further extend H-Calc with the following features:
- keep track of the location of error in the source text;
- evaluate `n` as `1+1+...` (n times);
- support `let x = expr1 in expr2`;

Feel free to give it a try !

(See [Interpreter.README](https://github.com/pcarbonn/H-Calc/tree/master/src/Interpreter) for some additional technical note on the implementation).


# Footnote


<a name="myfootnote1">1</a>: GHC comes with a set of libraries ("[base](http://hackage.haskell.org/package/base)"), some of which are imported by default in your program ("[Prelude](http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html)").  For historical reason, some choices that Prelude made are not optimal anymore.  Relude is an alternative Prelude that uses Text instead of String and avoids partial functions, among other benefits.
