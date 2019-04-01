module Haskell where

import Data.List (foldl')

diffSq x y = (x - y) * (x + y)

------------------------------
-- Local definition constructs
------------------------------

diffSqV3a x y =
    let minus = x - y
        plus = x + y
    in minus * plus

diffSqV3b x y = minus * plus
  where
    minus = x - y
    plus = x + y

{-
The difference:

* "let-in" is an expression.

* "where" is not part of an expression; it is part of a definition.
  "x where x=5" does not make sense; "y = ... where x=5" does.
-}


--------------------------------------------------
-- Basic example of pattern matching and recursion
--------------------------------------------------

slowFactorial 0 = 1
slowFactorial n = n * slowFactorial (n - 1)

{- Long form:

slowFactorial x = case x of
    0 -> 1
    n -> n * slowFactorial (n - 1)

Can you use if-then-else? Yes:

slowFactorial n = if n==0 then 1 else n * slowFactorial (n-1)

But usually pattern matching is better.  (Not always.)
-}


{- Synthesis view (how to write code) vs evaluation view (how to run code).

Everyone teaches how to run recursive code.  That still doesn't help you with
writing.  (Probably impedes you actually --- hand-running recursive code is
distracting.)

I teach you both.  I show you that writing recursive code can be easier if you
don't try to run it.

Synthesis view (how I write recursive code): Pretend induction.  (Use induction
to prove something that still contains unknowns, ah but during the proof you
find out how to solve for the unknowns!)

WTP: For all natural n: slowFactorial n = n!

Base case:

  WTP: slowFactorial 0 = 0!

  Notice 0! = 1, so if I code up

    slowFactorial 0 = 1

  I get slowFactorial 0 = 0!

Induction step:

   Let natural n ≥ 1 be given.
   Induction hypothesis: slowFactorial (n-1) = (n-1)!
   WTP: slowFactorial n = n!

   Notice n! = n*(n-1)!
             = n * slowFactorial (n-1)  by I.H.
   So if I code up

     slowFactorial n = n * slowFactorial (n-1)

   I get slowFactorial n = n!

   Comments:

   * The proof structure guides the code structure.

   * I refuse to imagine "what if I unfold slowFactorial (n-1) by hand?".  The
     I.H. already tells me the answer so I just use it.  This helps my focus.

   * The catch: I need to make up my mind and carefully write down the
     specification --- what answer my function should give.  I need the I.H. to
     be clear, not muddy.

   * You get both code and correctness proof.  Buy 1 get 1 free.  Why don't
     people do this more?!


Evaluation view (how a computer or an enslaved student runs code): Plug and chug:

  slowFactorial 3
→ 3 * slowFactorial (3 - 1)
→ 3 * slowFactorial 2
→ 3 * (2 * slowFactorial (2 - 1))
→ 3 * (2 * slowFactorial 1)
→ 3 * (2 * (1 * slowFactorial (1 - 1)))
→ 3 * (2 * (1 * slowFactorial 0))
→ 3 * (2 * (1 * 1))
→ 3 * (2 * 1)
→ 3 * 2
→ 6

-}

-- Slow Fibonacci (yawn) to show you can have more patterns.
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)


-------------------------------------------
-- User-defined types: Algebraic data types
-------------------------------------------

-- First cut: Real enumerations. (C has fake enumerations.)
data Direction = North | East | South | West
    deriving (Eq, Show)  -- To enable equality and printing. More on this later.
{-
The name of the type is Direction.

The possible values of that type are North, East, South, West.
In other words:

  North :: Direction
  East :: Direction
  South :: Direction
  West :: Direction
  (Nothing else.)

North, East, South, West are also the "data constructors" of that type.
If you think "tags", you're right, I think that too.

All 5 names are your choice. Only requirement: capitalized.

WARNING: NOT OOP constructors. You are only choosing a name for a tag, you are
not writing arbitrary initialization code.

WARNING: NOT OOP subclasses/subtypes either. North is not a type, it's a value.

Similarly, the standard library defines Bool by

data Bool = False | True

Real enumeration because you can't mix up False and North. (Contrast with C.)
Everything is not an integer.
-}

-- Data constructors can be used in patterns. Fundamental way of consuming and
-- discerning parameters of an algebraic data type.

bearing :: Direction -> Integer
bearing North = 0
bearing East = 90
bearing South = 180
bearing West = 270

-- inverse of the above, but has to be a partial function
direction :: Integer -> Direction
direction 0 = North
direction 90 = East
direction 180 = South
direction 270 = West
direction _ = error "unsupported bearing"

-- Second cut: But each data constructor can have fields.

{-
Imagine a feudal country Tetrastrata with four kinds of persons:

* The monarch. 'Nuf said (specified by 0 fields).
* Lords. Each specified by 3 fields:
  * title (string), e.g., "Duke", "Earl"
  * territory (string), e.g., "Sussex"
  * succession number (integer), e.g., the 9th Duke of Sussex has 9 here
* Knights. Each specified by 1 field: name (a string)
* Peasants. Each specified by 1 field: name (a string)

They can be represented as:
-}
data Tetrastratan
    = Monarch
    | Lord String String Integer
    | Knight String
    | Peasant String
    deriving (Eq, Show)
{-
The type name is Tetrastratan.

The possible values are:
* Monarch
* Lord d t i, provided d::String, t::String, i::Integer
* Knight n, provided n::String
* Peasant n, provided n::String
* (Nothing else)

The data constructors (tags) are: Monarch, Lord, Knight, Peasant.

Mathematically, Tetrastratan is a tagged disjoint union:

   (singleton set) ⊎ String×String×Integer ⊎ String ⊎ String

analogous to a sum of products --- polynomial --- "algebraic".
-}

ninthDukeOfSussex = Lord "Duke" "Sussex" 9

-- How to address a Tetrastratan:
addressTetra Monarch = "H.M. The Monarch"
addressTetra (Lord d t i) = "The " ++ show i ++ "th " ++ d ++ " of " ++ t
addressTetra (Knight n) = "Sir " ++ n
addressTetra (Peasant n) = n

-- Social order:
superior Monarch (Lord _ _ _) = True
superior Monarch (Knight _) = True
superior Monarch (Peasant _) = True
superior (Lord _ _ _) (Knight _) = True
superior (Lord _ _ _) (Peasant _) = True
superior (Knight _) (Peasant _) = True
superior _ _ = False

{-
Exercise: String for lord titles is too broad. Replace by an enumeration type.
Everything is not a string.

Design guide: Design your data type to avoid invalid data in the first place.
This reduces bugs.  Not always possible, and when possible not always
worthwhile, but you strive to.  Disallow invalid data unless you can justify why
it is not worthwhile.  Read this from Scala practitioners:
http://rea.tech/the-abject-failure-of-weak-typing/
-}

-- Third cut: Recursion (self, mutual) is also supported.

-- [Singly linked] list of integers.
data MyIntegerList = INil | ICons Integer MyIntegerList
    deriving Show
    -- I will hand-code the Eq part later.

exampleMyIntegerList = ICons 4 (ICons (-10) INil)

myISum :: MyIntegerList -> Integer
myISum INil = 0
myISum (ICons x xs) = x + myISum xs

-- Binary [search] tree of integers.
data IntegerBST = IEmpty | INode IntegerBST Integer IntegerBST
    deriving Show

exampleIntegerBST = INode (INode IEmpty 3 IEmpty) 7 (INode IEmpty 10 IEmpty)

{-
BST insert.  Since this is functional programming with immutable trees, "insert"
means produce a new tree that is like the input tree but with the new key.  May
be better to say "the tree plus k".
-}
ibstInsert :: Integer -> IntegerBST -> IntegerBST
ibstInsert k IEmpty =
    -- Base case: empty tree plus k = singleton tree with k
    INode IEmpty k IEmpty

ibstInsert k inp@(INode left key right) -- "as-pattern", "inp as (Node left key right)"
    -- Induction step: The input tree has the form INode left key right.
    -- Induction hypothesis (strong induction):
    --   If t is a smaller tree than the input tree, e.g., t=left or t=right,
    --   then ibstInsert k t computes t plus k.
    -- Can you use this to help compute input tree plus k?

    -- If k<key, the correct answer is a node consisting of:
    -- new left child = left plus k = ibstInsert k left (by IH)
    -- new key = key
    -- new right child = right

    | k < key = INode (ibstInsert k left) key right

    -- If k>key, mirror image of the above.

    | k > key = INode left key (ibstInsert k right)

    -- If k==key, nothing new to compute, the correct answer is the input tree.
    | otherwise = inp

    -- Also rewrite to use compare k key

-- Fourth cut: Parametric polymorphism (aka Java generics).

-- Binary [search] tree of arbitrary key type.
data BST a = Empty | Node (BST a) a (BST a)
    deriving Show

exampleBSTChar :: BST Char
exampleBSTChar = Node (Node Empty 'c' Empty) 'g' (Node Empty 'j' Empty)

bstInsert :: Ord a => a -> BST a -> BST a
-- "Ord a =>" to be explained later. For now it means the code can do <, <= comparison.
bstInsert k Empty = Node Empty k Empty
-- The commented-out version shows guard syntax.
-- bstInsert k inp@(Node left key right)
--     | k < key = Node (bstInsert k left) key right
--     | k > key = Node left key (bstInsert k right)
--     | otherwise = inp
bstInsert k inp@(Node left key right)   -- "as-pattern", "inp as (Node left key right)"
    = case compare k key of
        LT -> Node (bstInsert k left6) key right
        GT -> Node left key (bstInsert k right)
        EQ -> inp

{-
Exercise: Rewrite BST and bstInsert for dictionary, not just set. I.e., not just
key, but also value.  Start like this:

data BST k v = Empty | Node ...
-}

-- My list type of arbitrary element type. (Just for teaching purpose. The
-- standard library has a list type like this already; read on.)
data MyList a = Nil | Cons a (MyList a)
    deriving Show

exampleMyListI :: MyList Integer
exampleMyListI = Cons 4 (Cons (-10) Nil)

exampleMyListS :: MyList String
exampleMyListS = Cons "albert" (Cons "bart" Nil)

{-
Homogeneous list --- can't have different item types in the same list, e.g.,

  Cons "albert" (Cons True Nil)

is illegal. Because what would be its type, MyList String? MyList Bool?

But look up "Either" and have:

  Cons (Left "albert") (Cons (Right True) Nil) :: MyList (Either String Bool)

Similarly for the BST type above.
-}

{-
Standard library list syntax:
Type: [a], [] a
Empty list: []
Cons: x : xs
Nice notation: [a, b, c] = a : (b : (c : [])) = a : b : c : []
List comprehension also available.
-}

-- Insertion sort.
-- Strategy: Have a helper function "insert":
-- Take an element x and a list lst. lst is assumed to have been sorted.
-- Put x into the right place in lst so the whole is still sorted.
-- E.g., insert 4 [1,3,5,8,9,10] = [1,3,4,5,8,9,10]
-- (Or rather, produce a new list that's like lst but also with x at the right place.)

-- Structural induction on lst.
-- Base case: lst is empty. Answer is [x].
insert x [] = [x]
-- Induction step: Suppose lst has the form hd:tl (and tl is shorter than lst).
-- E.g., lst = [1,3,5,8], hd = 1, tl = [3,5,8].
-- Induction hypothesis: insert x tl = put x into the right place in tl.
insert x lst@(hd:tl)
    -- If x <= hd, then x should be put before hd, in fact all of lst, and be done.

    | x <= hd = undefined

    -- Otherwise, the answer should go like:
    -- hd, followed by whatever is putting x into the right place in tl.
    -- i.e.,
    -- hd, followed by insert x tl (because IH)
    -- E.g., insert 25 (10:tl) = 10 : insert 25 tl

    | otherwise = undefined

-- The main insertion sort function. Exercise: Your turn to use induction on this.
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Another exercise: Evaluate insert 15 (10 : (20 : (30 : []))).


{---------------------------------------------------------
Interlude: Nested lambdas and nested function applications
----------------------------------------------------------

Lambda = anonymous function = how do you write a function without giving it a
name?

Python:      lambda x : x+1
Javascript:  function (x) { return x+1; }
             x => x+1
Haskell:     \x -> x+1

\ was chosen because it looks like the Greek letter lambda: λ.

Next, if you intend 2 parameters, the Haskell community culture is to model it
as a nested function: \x -> (\y -> 2*x - 3*y) (those parentheses can be omitted)
--- a function that maps the 1st parameter to a function that takes the 2nd
parameter.

Shorthand: \x y -> 2*x - 3*y

   f x y = ...
can be written as
   f = \x y -> ...
or even
   f x = \y -> ...

What about applying a function to 2 parameters:
   f foo bar
is shorthand for
   (f foo) bar
It is possible use "f foo" alone, under the right circumstance.

Type-wise:
   X -> Y -> A
is shorthand for
   X -> (Y -> A)

Example: The standard library has a function "map" for this:

    map f [1, 2, 3] = [f 1, f 2, f 3]

It is possible to use "map f" alone:

    map (map f) [[1,2,3], [4,5,6]]
  = [map f [1,2,3], map f [4,5,6]]
  = [[f 1, f 2, f 3], [f 4, f 5, f 6]]

Try these:
  map (\x -> 10*x+8) [1,2,3]
  map (map (\x -> 10*x+5)) [[1,2,3], [4,5,6]]
-}


--------
-- foldr
--------

-- One way to sum or multiply a list. This code is not for real use (the
-- standard library already has "sum" and "product"), but to make a bigger
-- point.
mySumV1 [] = 0
mySumV1 (x:xs) = x + mySumV1 xs
myProductV1 [] = 1
myProductV1 (x:xs) = x * myProductV1 xs
-- What if you want to use yet another binary operator?  Do we keep writing the
-- same code again?  Surely there is something to refactor out.  One more
-- example (less obvious):

-- This function is also already in the standard library, under the name "map".
-- We write it again to make a bigger point.
-- E.g., myMap f [1, 2, 3] = [f 1, f 2, f 3]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
-- Or:
-- myMap f lst = g lst
--   where
--     g [] = []
--     g (x:xs) = f x : g xs

{- The common theme is a function that looks like
     g [] = z
     g (x:xs) = a function of x and g xs

   g = mySumV1:
     z = 0
     function: x + g xs --- the function is (+)

   g = myProductV1:
     z = 1
     function: x * g xs --- the function is (*)

   g = myMap f:
     z = 0
     function: f x : g xs --- the function is \x r -> f x : r

   All these are refactored to:

   myFoldr op z lst = g lst
     where
       g [] = z
       g (x:xs) = op x (g xs)

   The standard library has "foldr" for this.

   More commonly we expand g back to myFoldr op z:
-}
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op z [] = z
myFoldr op z (x:xs) = op x (myFoldr op z xs)
-- So here are mySumV1, myProductV1, myMap re-expressed as foldr's:
mySumFoldr xs = undefined
myProductFoldr xs = undefined
myMapFoldr f xs = undefined

{- The above shows one way to detect that you can use foldr.  Below is another way
   using pretend induction to solve for op and z and get a complete proof!  From
   http://ertes.eu/tutorial/foldr.html

   (Unfortunately the author died May 12, 2018.  I knew him from IRC.)

   I'll show working on myMap.  Prove
     myMap f lst = foldr op z lst
   by pretend induction and find op and z.

   Base case: lst = []
       LHS = myMap f [] = []
       RHS = foldr op z [] = z
     Define z = [], then
       LHS = [] = RHS

   Induction step: lst has the form x:xs.
     Induction hypothesis: myMap f xs = foldr op [] xs
     WTP: myMap f (x:xs) = foldr op [] (x:xs)

       LHS
     = f x : myMap f xs         by myMap code

       RHS
     = op x (foldr op [] xs)    by foldr code
     = op x (myMap f xs)        by IH

     Need op to do: op x (myMap f xs) = f x : myMap f xs

     IMPORTANT TRICK: Generalize from myMap f x to arbitrary r.

     Need op to do: op x r = f x : r

     Define op to do just that!  Or use (\x r -> f x : r) directly.

     Then LHS = RHS by definition of op.

   Advice: DO NOT THINK. DO NOT USE INTUITION. NEVER WORKED FOR PAST STUDENTS.

   Instead, look for the common theme or do the pretend induction.
-}


--------
-- foldl
--------

-- Another version of summing a list. This version is closer to the
-- for-loop-and-accumulator you normally write.
--
-- The trick is to conjure a helper function with this more general specification:
--
-- for all a, for all xs: g a xs = a + sum of xs
mySumV2 xs = g 0 xs
  where
    g accum [] = accum
    {- Induction step: The list is x:xs
       Induction hypothesis: for all a: g a xs = a + sum of xs

       How to compute accum + sum of (x:xs)?

       accum + sum of (x:xs)
     = accum + x + sum of xs
     = (accum + x) + sum of xs
     = g (accum + x) xs               by IH
    -}
    g accum (x:xs) = g (accum + x) xs

{-
Reverse a list. The standard library already has it, but again I'm writing my
own for a purpose.

First, this is too slow. Why? (How much time does ++ take?)

    slowReverse [] = []
    slowReverse (x:xs) = slowReverse xs ++ [x]

(Answer: ++ takes linear time, slowReverse takes quadratic time.)

The trick is to conjure a helper function with this specification:

for all a, for all xs: g a xs = (reversal of xs) ++ a

You can also think of using an accumulator for the reversal of a prefix of the
input list.
-}
myReverse xs = g [] xs
  where
    g accum [] = accum
    {- Induction step: The list is x:xs
       IH: for all a: g a xs = (reversal of xs) ++ a

       How to compute (reversal of (x:xs)) ++ a ?

       (reversal of (x:xs)) ++ accum
     = (reversal of xs) ++ [x] ++ accum
     = (reversal of xs) ++ ([x] ++ accum)
     = g ([x] ++ accum) xs                    by IH
     = g (x : accum) xs
    -}
    g accum (x:xs) = g (x : accum) xs

{-
Common theme: a function g that looks like

  g accum [] = accum
  g accum (x:xs) = g (a function of accum and x) xs

mySumV2:
  initial accum: 0
  function: (+)

myReverse:
  initial accum: []
  function: (\a x -> x : a) = flip (:)

This is abstracted into:

myFoldl op z xs = g z xs
  where
    g accum [] = accum
    g accum (x:xs) = g (op accum x) xs

The standard library has "foldl" for this.

More commonly we expand g back to foldl op:
-}
myFoldl op z [] = z
myFoldl op z (x:xs) = myFoldl op (op z x) xs

mySumFoldl xs = myFoldl (+) 0 xs

myReverseFoldl xs = myFoldl (flip (:)) [] xs

{- Again you can also use pretend induction to detect that you can use foldl
   and find f and z.
-}


{-----------------
-- Lazy evaluation
------------------

Let me mess with you first.

"take" is in the standard library defined like this:

  take 0 _ = []
  take _ [] = []
  take n (x:xs) = x : take (n-1) xs

IOW the first n items of the input list, or as many as available, e.g.,

  take 3 [a,b,c,d,e] = [a,b,c]
  take 3 [a,b] = [a,b]

So does the following terminate?
-}
doITerminate = take 2 (foo 0)
  where
    foo n = n : foo (n + 1)

{- And this? -}
doIEvenMakeSense = take 2 foo
  where
    foo = 0 : foo
    -- In the computer this is one cons cell pointing back to itself. O(1)-space.

{- Lazy evaluation.

Recall eager evaluation in Racket (and everything else basically):

To evaluate (f foo bar): evaluate foo and bar first, then substitute into f's
body.

Lazy evaluation in Haskell (sketch):

To evaluate f foo bar: Plug into f's body first, then evaluate that body.

If that runs you into pattern matching (because either "f (x:xs) = ..." or "case
... of ..."): evaluate parameter(s) just enough to decide whether it's a match
or non-match.  If match, evaluate that branch.  If non-match, try the next
pattern.  (If run out of patterns, declare "undefined" aka "error".)

To evaluate arithmetic operations e.g. foo + bar: eager evaluation.

Evaluation of doITerminate (until the answer is fully printed --- the REPL wants
this):

   take 2 (foo 0)
→ take 2 (0 : foo (0 + 1))
→ 0 : take (2-1) (foo (0 + 1))
→ 0 : take 1 (foo (0 + 1))
→ 0 : take 1 (n : foo (n + 1))          where n = 0 + 1
→ 0 : n : take (1-1) (foo (n + 1))
→ 0 : n : take (1-1) (foo (n + 1))      where n = 1
→ 0 : n : take 0 (foo (n + 1))      where n = 1
→ 0 : n : []                        where n = 1

Very drastic example:

The standard library has

  const x y = x

Evaluation of const (3+4) (div 1 0):

  const (3+4) (div 1 0)
→ 3+4
→ 7

(No error about dividing by zero.)
-}

{-
Newton's method with lazy lists. Like in Hughes's "why FP matters".
Approximately solve x^3 - b = 0, i.e., cube root of b.
So f(x) = x^3 - b, f'(x) = 3x^2
x1 = x - f(x)/f'(x)
   = x - (x^3 - b)/(3x^2)
   = x - (x - b/x^2)/3
   = (2x + b/x^2)/3
The local function "next" below is responsible for computing x1 from x.
-}
cubeRoot b = within 0.001 (iterate next b)
  where
    next x = (2*x + b/x^2) / 3
    within eps (x : (x1 : rest))
        -- Some parethenses optional.
        -- Maybe show off @-pattern here.(f z
        | abs (x - x1) <= eps = x1
        | otherwise = within eps (x1 : rest)
    -- From the standard library:
    -- iterate f z = z : iterate f (f z)
    --             = [z, f z, f (f z), f (f (f z)), ...]

{-
Equivalently

  cubeRoot = within 0.001 . iterate next

Function composition (.) is defined by

  (f . g) x = f (g x)

With this, you really have a pipeline as in Unix pipelines.
-}

{-
Singly-linked list is a very space-consuming data structure (all languages).
And if you have to ask for "the ith item" you're doing it wrong.

However, if you use list lazily in Haskell, it is an excellent *control*
structure --- a better for-loop than for-loops.  Then list-processing functions
become pipeline stages.  If you do it carefully, it is even O(1)-space.  If
furthermore you're lucky (or if you know the compiler can optimize it), it can
even fit entirely in registers.

Thinking in high-level pipeline stages is both more sane and more efficient ---
with the right languages.

Some very notable list functions when you use lists lazily as for-loops, or when
you think in terms of pipeline stages:

Sources: repeat, cycle, replicate, iterate, unfoldr,
  the [x..], [x..y] notation (backed by enumFrom, enumFromTo)

Transducers: map, filter, scanl, scanr, (foldr too, sometimes)
  take, drop, splitAt, takeWhile, dropWhile, span, break, partition,
  zip, zipWith, unzip

Sinks: foldr, foldl, foldl', sum, product, maximum, minimum, and, all, or, any, ...

Sometimes you have to custom-make your own, of course.

And don't forget that list comprehension helps a lot too.
-}


{----------------------------
-- When lazy evaluation hurts
-----------------------------

Evaluation of mySumV2 [1,2,3] (similarly foldl (+) 0 [1,2,3]):

  mySumV2 (1 : 2 : 3 : [])
→ g 0 (1 : 2 : 3 : [])
→ g (0 + 1) (2 : 3 : [])
→ g ((0 + 1) + 2) (3 : [])
→ g (((0 + 1) + 2) + 3) []
→ ((0 + 1) + 2) + 3
→ (1 + 2) + 3
→ 3 + 3
→ 6

This takes Ω(n) space for the postponed arithmetic.

Summing a list does not need lazy evaluation for the accumulator.  There is
"seq" for killing it when you deem lazy evaluation unsuitable.

To evaluate "seq x y": evaluate x to "weak head normal form", then continue with y.

Weak head normal form (WHNF) means:

* for built-in number types: until you have the number
* for algebraic data types: until you have a data constructor
* for functions: until you have a lambda
-}
mySumV3 xs = g 0 xs
  where
    g accum [] = accum
    g accum (x:xs) = seq accum (g (accum + x) xs)
    {-
    Alternative:

    g accum (x:xs) = let a1 = accum + x
                     in seq a1 (g a1 xs)

    But not:

    g accum (x:xs) = seq (accum + x) (g (accum + x) xs)

    Because the two copies of accum+x are independent.  Evaluating one of them
    does no good to the other.
    -}

{- Data.List has foldl' for this as well:

foldl' f z [] = z
foldl' f z (x:xs) = seq z (foldl' f (f z x) xs)
-}


{-
How Haskell overloads ==, <, + for various types, and how you can extend them to
your own types, and how you can declare your own overloadable operations:

A "type class" declares a group of overloaded operations ("methods").

Take == and /= for example.  The type class is Eq from the standard library:

class Eq a where
    (==), (/=) :: a -> a -> Bool

To implement these methods for a type, e.g., the standard library has this for
Bool:

instance Eq Bool where
    False == False    = True
    True == True      = True
    _ == _            = False

We say "Bool is an instance of Eq".

WARNING:

* A class is not a type.  Eq is not a type.  These are nonsense:

      foo :: Eq -> Eq -> Bool
      bar :: Eq a -> Eq a -> Bool

* A type is not a "subclass".  Bool is not a "subclass" of Eq.

If you write a polymorphic function that uses a method, then its type will carry
the corresponding class names to mark the fact that your function is only good
for instances of the classes.  (This also happens to the types of the methods
themselves.)
-}
eq3 x y z = x==y && y == z
eq3 :: Eq a => a -> a -> a -> Bool
-- NOT a -> a -> a -> Bool

{-
Recall we defined our own types:

data MyIntegerList = INil | ICons Integer MyIntegerList
data MyList a = Nil | Cons a (MyList a)

Let's make them instances of Eq so we can use == on them too:
-}
instance Eq MyIntegerList where
    INil == INil              = True
    ICons x xs == ICons y ys  = x == y && xs == ys
    _ == _                    = False

instance Eq a => Eq (MyList a) where
    Nil == Nil              = True
    Cons x xs == Cons y ys  = x == y && xs == ys
                            -- "x == y" is when we need to assume Eq a.
    _ == _                    = False

{-
Check out these other type classes: Ord, Bounded, Enum (its methods are behind
the [1..n] notation), Show, Read.

Reference:
https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270006.3

Often it is straightforward but boring to write instances for these classes,
so the computer offers to auto-gen for you.  Restrictions apply.  You request
it at the definition of your algebraic data type like this:

data MyType = ... deriving (Eq, Ord, Bounded, Enum, Show, Read)

For example the computer would write the same == algorithms above for
MyIntegerList and MyList.

Reference:
https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18200011
-}

{-
Number types:

Haskell has Int, Integer, Rational, Float (single-precision floating point),
Double (double-precision floating point), Complex a.

Number operations are grouped into several type classes, e.g.,

Num:
   some methods: +, -, *
   instances: all number types

Integral:
   some methods: div, mod
   instances: Int, Integer

Fractional:
   some methods: /
   instances: Rational, Float, Double, Complex a

You can add your own number type by making it an instance of the relevant
classes.  (E.g., you could have written your own Rational and Complex.)

Reference:
https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350006.4

Exercise: Why is this a type error?

  let xs :: [Double]
      xs = [1, 2, 3]
  in sum xs / length xs

Answer:

sum xs :: Double
length xs :: Int

(/) wants the two operands to be of the same type.

How to fix: sum xs / fromIntegral (length xs)
-}

{-
Machine epsilon.

https://gist.github.com/AndrewBarfield/2557034 is forced to write two copies of
the same code for two number types because C# is a stupid language.

This is *criminal* in this the 21st Century.

A passable language allows you to hand in just one copy:
-}
epsilon :: (Ord a, Fractional a) => a
epsilon = last notTooSmall
  where
    halves = iterate (/ 2) 1
    notTooSmall = takeWhile (\e -> 1 + e > 1) halves

epsilonDouble :: Double
epsilonDouble = epsilon

epsilonFloat :: Float
epsilonFloat = epsilon
