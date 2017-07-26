% F-algebras

# The roadmap

The aim is to understand this:

> In category theory, the concept of catamorphism denotes the unique
> homomorphism from an initial algebra into some other algebra.

To better understand this:

> In functional programming, catamorphisms provide generalizations of folds of
> lists to arbitrary algebraic data types, which can be described as initial
> algebras. The dual concept is that of anamorphism that generalize unfolds. A
> hylomorphism is the composition of an anamorphism followed by a catamorphism.

# The outline

* Basic category theory
* Haskell as a category (if you squint)
* Category theoretic functors, Haskell Functors endofunctors
* Algebras over an endofunctor
* F-Algebra homomorphisms, or arrows in the category of F-algebras
* Initial objects
* Catamorphisms as unique homomorphisms from initial objects
* Flip the arrows

# What's a category?

[columns]

[column=0.5]

Pick some things:

* Objects (X,Y,Z)
* Arrows between objects (f,g,$g \circ f$)

Assert some properties:

* All arrows compose associatively
* Every object has an identity arrow

Commutative diagram: $$ hom \circ alg \equiv alg' \circ fmap\;hom $$


[column=0.5]

. . .

```commute
\node (X) {$X$};
\node (Y) [right of=X] {$Y$};
\node (Z) [below of=Y] {$Z$};
\draw[->] (X) to node {$f$} (Y);
\draw[->, loop left] (X) to node {$id_X$} (X);
\draw[->] (Y) to node {$g$} (Z);
\draw[->, loop right] (Y) to node {$id_Y$} (Y);
\draw[->] (X) to node {$g \circ f$} (Z);
\draw[->, loop below] (Z) to node {$id_Z$} (Z);
```

[/columns]

# Haskell as sort of a category

Pick some things:

* Objects are types (not values!)
* Arrows are functions between types

Assert some properties:

* All arrows compose associatively
* Every object has an identity arrow

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
id :: a -> a
```

NB: it's lies, all lies!

```haskell
seq undefined () = undefined
seq (undefined . id) () = ()
```

# Functors, endofunctors


A functor is a mapping between categories that sends objects to objects (types
to types) and
arrows to arrows (terms to terms), preserving identity arrows and composition, possibly across
categories.

```haskell
fmap id = id
fmap f . fmap g = fmap (f . g)
```

Endofunctors map from a category to the same category.

In the case of Hask: 

```haskell
class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b
```

# Algebra "over" an endofunctor

[columns]

[column=0.5]

For a category C and endofunctor F an algebra of F is an object X in C and a
morphism: $$ alg : F(X) \to X $$

X is called the "carrier" of the algebra.

[column=0.5]

```haskell
-- For a category and endofunctor
data F a = Zero | Succ a

instance Functor F where
  fmap _ Zero     = Zero
  fmap f (Succ a) = Succ (f a)

-- An algebra of F is an X in C
type X = Natural

-- And a morphism
alg :: F X -> X
alg Zero = 0
alg (Succ n) = n + 1
```

[/columns]

# F Natural -> Natural

```haskell
data F a = Zero | Succ a

alg :: F Natural -> Natural
alg Zero = 0
alg (Succ n) = n + 1


> alg Zero
0
> alg $ Succ $ alg Zero
1
> alg $ Succ $ alg $ Succ $ alg Zero
2
```

# An alternate algebra, same F and C, different X

```haskell
alg' :: F String -> String
alg' Zero     = "!"
alg' (Succ s) = "QUACK" ++ s

> alg' $ Succ $ alg' $ Succ $ alg' Zero
"QUACKQUACK!"
```

# Prescient fun fact

> An initial object of a category C is an object I in C such that for every
> object X in C, there exists precisely one morphism $I \to X$. - Wikipedia

(up to isomorphism)

# Initial, in the category of algebras

An initial algebra for an endofunctor F on a category C is an initial object in
the category of algebras of F.

. . .

Category of algebras of F:

* Objects: alg, alg', ...
* Arrows : structure preserving maps (homomorphisms) from an algebra to another

# Homomorphisms between two algebras.

An arrow in the category of F-algebras of a given endofunctor e.g. between
(Natural, alg) and (String, alg') is a function mapping the carrier in the
underlying category (Hask, hom : Natural -> String), such that the following
square commutes:

```commute
\node (FX) {$F(Natural)$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$Natural$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FX) to node {$F(hom)$} (FY);
\draw[->] (FX) to node [swap] {$alg$} (X);
\draw[->] (X) to node [swap] {$hom$} (Y);
\draw[->] (FY) to node {$alg'$} (Y);
```

# That is to say that

[columns]

[column=0.5]

```haskell
fNat :: F Natural
fNat = Succ 1

hom :: Natural -> String
hom n = timesN n "QUACK" ++ "!"

> alg fNat             -- 2
> fmap hom fNat        -- Succ "QUACK!"
> hom $ alg fNat       -- "QUACKQUACK!"
> alg' $ fmap hom fNat -- "QUACKQUACK!"
```

[column=0.5]

$$ hom \circ alg \equiv alg' \circ fmap\;hom $$

```commute
\node (FX) {$F(Natural)$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$Natural$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FX) to node {$F(hom)$} (FY);
\draw[->] (FX) to node [swap] {$alg$} (X);
\draw[->] (X) to node [swap] {$hom$} (Y);
\draw[->] (FY) to node {$alg'$} (Y);
```

[/columns]

# Initial, in the category of algebras

An initial algebra for an endofunctor F on a category C is an initial object in
the category of algebras of F.

Category of algebras of F:

* Objects: alg, alg', ...
* Arrows : structure preserving maps (homomorphisms) from an algebra to another

. . .

Initial:

* There is a unique morphism from the initial algebra to *all other algebras*.

# What fits?

[columns]

[column=0.5]

* The carrier must not "lose" any information, or there is some algebra that it
  cannot map to.
* The carrier can't add information, or the morphism won't be unique.
* The algebra must have type: F InitF -> InitF
* Lambek's theorem says that if there is an initial object, it is isomorphic to
  the carrier via the algebra
* data InitF = InitF (F InitF)

[column=0.5]

$$ X\ initial \implies  F(X) \cong X $$

```commute
\node (FX) {$F\ InitF$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$InitF$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FX) to node {$F(hom)$} (FY);
\draw[->] (FX) to node [swap] {$alg$} (X);
\draw[->] (X) to node [swap] {$hom$} (Y);
\draw[->] (FY) to node {$alg'$} (Y);
```

[/columns]

# More generally...

. . .

```haskell
data Fix f = Roll { unRoll :: f (Fix f) }
type InitF = Fix F
Roll :: F InitF -> InitF
unRoll :: InitF -> F InitF
```

. . . 

```haskell
fix2 :: InitF
fix2 = Roll $ Succ $ Roll $ Succ $ Roll Zero
```

. . .

If this is the initial object in the category of algebras, there must be a
unique arrow from InitF to *every* algebra:

$$ \forall algebras\ \exists hom\ : InitF \to carrier\ of\ algebra$$

# The unique homomorphism

$$ \forall algebras\ \exists hom\ : InitF \to carrier\ of\ algebra$$

[columns]

[column=0.5]

```commute
\node (FX) {$F\ InitF$};
\node (FY) [right of=FX] {$F(Natural)$};
\node (X) [below of=FX] {$InitF$};
\node (Y) [below of=FY] {$Natural$};
\draw[->] (FX) to node {$F(hom)$} (FY);
\draw[->] (FX) to node [swap] {$Roll$} (X);
\draw[->] (X) to node [swap] {$hom$} (Y);
\draw[->] (FY) to node {$alg$} (Y);
```

[column=0.5]

. . .

``` haskell
Roll :: F InitF -> InitF
```
. . . 

```haskell
hom :: InitF -> Natural
```

[/columns]

# The unique homomorphism

$$ \forall algebras\ \exists hom\ : InitF \to carrier\ of\ algebra$$

[columns]

[column=0.5]

```commute
\node (FX) {$F\ InitF$};
\node (FY) [right of=FX] {$F(Natural)$};
\node (X) [below of=FX] {$InitF$};
\node (Y) [below of=FY] {$Natural$};
\draw[->] (FX) to node {$F(hom)$} (FY);
\draw[->] (X) to node {$unRoll$} (FX);
\draw[->] (X) to node [swap] {$hom$} (Y);
\draw[->] (FY) to node {$alg$} (Y);
```

[column=0.5]

``` haskell
unRoll :: InitF -> F InitF
```

. . . 

```haskell
hom :: InitF -> Natural
hom = alg . fmap hom . unRoll
```

. . .

```haskell
cata :: Functor f
     => (f a -> a) -> Fix f -> a
cata alg =
  alg . fmap (cata alg) . unRoll

hom = cata alg

```

[/columns]

# Evaluation of cata

[columns]

[column=0.5]

```haskell
cata :: Functor f
     => (f a -> a) -> Fix f -> a
cata alg =
  alg . fmap (cata alg) . unRoll
```

[column=0.5]

```haskell
alg :: F Natural -> Natural
alg Zero = 0
alg (Succ n) = n + 1
```

[/columns]

\medskip

\hrule

. . .

```haskell
cata alg (Roll $ Succ $ Roll Zero)
```

. . .

```haskell
alg $ fmap (cata alg) (Succ $ Roll Zero)
```

. . .

```haskell
alg $ Succ $ cata alg $ Roll Zero
```
. . .

```haskell
alg $ Succ $ alg $ fmap (cata alg) Zero
```
. . .

```haskell
alg $ Succ $ alg $ Zero
```

# This is recursion in a general sense

```haskell
data Nat a = Succ a | Zero
```
. . .

```haskell
data String a = Cons Char a | End
```

. . .

```haskell
data BinaryTree a = Branch a a | Tip
```
. . .

```haskell
data RoseTree a = Branches [a] | Tip
```

. . .

```haskell
data Group a = Action a a | Inv a | Unit
```

# Hutton's razor - final tagless

```haskell
class Calculator a where
    lit :: Int -> a
    add :: a -> a -> a
    mult :: a -> a -> a

instance Calculator Int where
    lit = id
    add = (+)
    mult = (*)

instance Calculator String where
    lit = show
    add s1 s2 = s1 ++ " + " ++ s2
    mult s1 s2 = s1 ++ " x " ++ s2
```

# Hutton's razor - F algebra

```haskell
data Calculator a = Lit Int | Add a a | Mult a a deriving Functor

evalAlg :: Calculator Int -> Int
evalAlg (Lit i) = i
evalAlg (Add i1 i2) = i1 + i2
evalAlg (Mult i1 i2) = i1 * i2

ppAlg :: Calculator String -> String
ppAlg (Lit i) = show i
ppAlg (Add s1 s2) = s1 ++ " + " ++ s2
ppAlg (Mult s1 s2) = s1 ++ " x " ++ s2

pp :: Fix Calculator -> String
pp = cata ppAlg
```

# Damn the torpedos, flip the arrows

[columns]

[column=0.5]

```haskell
alg :: F Natural -> Natural
alg Zero = 0
alg (Succ n) = n + 1
```
. . .

```haskell
coalg :: Natural -> F Natural
coalg 0 = Zero
coalg n = Succ (n - 1)
```

. . .

[column=0.5]

For a category C and endofunctor F a co-algebra of F is an object X in C and a
morphism: $$ coalg : X \to F(X) $$


[/columns]

# Morphisms on coalgebras: co all of the things!

[columns]

[column=0.5]

```commute
\node (FX) {$F(Natural)$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$Natural$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FX) to node {$F(hom)$} (FY);
\draw[->] (FX) to node [swap] {$alg$} (X);
\draw[->] (X) to node [swap] {$hom$} (Y);
\draw[->] (FY) to node {$alg'$} (Y);
```

[column=0.5]

. . .

```commute
\node (FX) {$F(Natural)$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$Natural$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FY) to node [swap] {$F(hom')$} (FX);
\draw[->] (X) to node {$coalg$} (FX);
\draw[->] (Y) to node {$hom'$} (X);
\draw[->] (Y) to node [swap] {$coalg'$} (FY);
```

[/columns]

# E.g.

[columns]

[column=0.5]

```commute
\node (FX) {$F(Natural)$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$Natural$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FY) to node [swap] {$F(hom')$} (FX);
\draw[->] (X) to node {$coalg$} (FX);
\draw[->] (Y) to node {$hom'$} (X);
\draw[->] (Y) to node [swap] {$coalg'$} (FY);
```

[column=0.5]

```haskell
coalg :: Natural -> F Natural
coalg 0 = Zero
coalg n = Succ (n - 1)
```

. . . 

```haskell
coalg' :: String -> F String
coalg' "!" =
  Zero
coalg' ('Q':'U':'A':'C':'K':xs) =
  Succ xs
```

[/columns]

# E.g.

[columns]

[column=0.5]

```commute
\node (FX) {$F(Natural)$};
\node (FY) [right of=FX] {$F(String)$};
\node (X) [below of=FX] {$Natural$};
\node (Y) [below of=FY] {$String$};
\draw[->] (FY) to node [swap] {$F(hom')$} (FX);
\draw[->] (X) to node {$coalg$} (FX);
\draw[->] (Y) to node {$hom'$} (X);
\draw[->] (Y) to node [swap] {$coalg'$} (FY);
```

[column=0.5]

```haskell
hom :: Natural -> String
hom n = timesN n "QUACK" ++ "!"

```
. . .

``` haskell
hom' :: String -> Natural
hom' str =
  (fromIntegral (length str) - 1)
    `div` 5
```

[/columns]

. . . 

```haskell
> (hom' "QUACKQUACK!", coalg' "QUACKQUACK!")
(2, Succ "QUACK!")
> (coalg $ hom' "QUACKQUACK!", fmap hom' $ coalg' "QUACKQUACK!")
(Succ 1, Succ 1)
```

# The unique homomorphism

$$ \forall algebras\ \exists hom\ : carrier\ of\ algebra \to TermF$$

[columns]

[column=0.5]

```commute
\node (FX) {$F\ TermF$};
\node (FY) [right of=FX] {$F(Natural)$};
\node (X) [below of=FX] {$TermF$};
\node (Y) [below of=FY] {$Natural$};
\draw[->] (FY) to node [swap] {$F(hom')$} (FX);
\draw[->] (X) to node {$unRoll$} (FX);
\draw[->] (Y) to node {$hom'$} (X);
\draw[->] (Y) to node [swap] {$coalg$} (FY);
```

[column=0.5]

. . .

``` haskell
type TermF = InitF
unRoll :: TermF -> F TermF
```

. . .

```haskell
hom' :: Natural -> TermF
```

[/columns]

# The unique homomorphism

$$ \forall algebras\ \exists hom\ : carrier\ of\ algebra \to TermF$$

[columns]

[column=0.5]

```commute
\node (FX) {$F\ TermF$};
\node (FY) [right of=FX] {$F(Natural)$};
\node (X) [below of=FX] {$TermF$};
\node (Y) [below of=FY] {$Natural$};
\draw[->] (FY) to node [swap] {$F(hom')$} (FX);
\draw[->] (FX) to node [swap] {$Roll$} (X);
\draw[->] (Y) to node {$hom'$} (X);
\draw[->] (Y) to node [swap] {$coalg$} (FY);
```

[column=0.5]

``` haskell
Roll :: F TermF -> TermF
```

. . . 

```haskell
hom' :: Natural -> TermF
```
. . . 

```haskell
hom' = Roll . fmap hom' . coalg
```

. . .

```haskell
ana :: Functor f
    => (a -> f a) -> a -> Fix f
ana coalg =
  Roll . fmap (ana coalg) . f

hom' = ana coalg
```

[/columns]


# Whilst we're here

```haskell
cata :: Functor f => (f a -> a) -> InitF -> a
cata alg = alg . fmap (cata alg) . unRoll

ana :: Functor f => (a -> f a) -> a -> TermF
ana coalg = Roll . fmap (ana coalg) . coalg
```

. . . 

```haskell
cata alg' $ ana coalg' $ hom 3
> "QUACKQUACKQUACK!"
```

. . . 


```haskell
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg
```

. . . 

```haskell
hylo alg' coalg' $ hom 3
> "QUACKQUACKQUACK!"
```
