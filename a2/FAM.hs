module FAM where

{---------
-- Functor
----------}

fmap_List :: (a -> b) -> [] a -> [] b
-- "[] a" means "[a]" in types.
fmap_List f [] = []
fmap_List f (x:xs) = f x : fmap_List f xs
-- In other words, fmap_List = map.

{- data Maybe a = Nothing | Just a
   from the standard library.  Two perspectives:

   * It's like list of length 0 or 1.
   * It models two possibilities: "no answer" and "here's the answer".
-}
fmap_Maybe :: (a -> b) -> Maybe a -> Maybe b
fmap_Maybe f Nothing = Nothing
fmap_Maybe f (Just a) = Just (f a)

{- data Either e a = Left e | Right a
   from the standard library.

   It's like Maybe, but the "no answer" case carries extra data, perhaps some kind
   of reason for why "no answer".
-}
fmap_Either :: (a -> b) -> (Either e) a -> (Either e) b
fmap_Either f (Left e) = Left e
fmap_Either f (Right a) = Right (f a)

{- Also arrays, dictionaries, tree structures, ... most data structures you can
   think of, and then some.

   Common theme: (a -> b) -> (F a -> F b), where F is a parametrized type.

   There is a class for that!

   class Functor f where
       fmap :: (a -> b) -> (f a -> f b)

   Note: NOT "class Functor (f a)"

   instance Functor [] where
       fmap = map

   instance Functor Maybe where
       ... like fmap_Maybe above

   instance Functor (Either e) where
       ... like fmap_Either above

   Note: NOT "instance Functor [a]", "instance Functor (Maybe a)",
   "instance Functor (Either e a)".

   This is nothing like you have ever seen.  You have only seen: Generalize from
   "list of Integer" to "list of a".  I'm now telling you: Generalize from "list
   of Integer" to "f of Integer".

   This is like saying: In Java, generalize from List<Integer> to... what?
   (Can't be done. C++ neither.)

   Warning exercise: NOT Collection<Integer>. Why?

   Some data structure examples:
-}

data BinTree a = BTNil | BTNode a (BinTree a) (BinTree a) deriving Show

instance Functor BinTree where
    -- fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f BTNil = BTNil
    fmap f (BTNode a lt rt) = BTNode (f a) (fmap f lt) (fmap f rt)

{- Functor discussion:

   If you notice that, for data structures, fmap seems to always clone the shape
   and apply f elementwise to the elements, you're right.

   Functor on its own does not have much basic practical use, apart from
   providing a common name "fmap".  But it is much more useful when combined
   with the things below.  It also has an advanced practical use.

   On the other hand, Functor is extremely important in theory.  If you want
   that rabbit hole: Whenever you have an endofunctor F, it induces F-algebras
   and possibly a catamorphism.  The catamorphism of the [] functor is foldr.
-}


{-------------
-- Applicative
--------------

   fmap can apply a unary function elementwise to one list (or a maybe, or...).

   What if you want to apply a binary function to all pairs of elements of two
   lists (or two maybes, or...)?

   Example:
     map2_List (-) [10,20,30] [1,2,3]
   = [9,8,7] ++ [19,18,17] ++ [29,28,27]
   = [9,8,7,19,18,17,29,28,27]
-}
map2_List :: (a -> b -> c) -> [a] -> [b] -> [c]
map2_List f [] _ = []
map2_List f (a:as) bs = map (f a) bs ++ map2_List f as bs

map2_Maybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2_Maybe f Nothing _ = Nothing
map2_Maybe f (Just a) Nothing = Nothing
map2_Maybe f (Just a) (Just b) = Just ((f a) b)
-- Or: map2_Maybe f (Just a) mb = fmap (f a) mb

map2_Either :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
map2_Either f (Left e) _ = Left e
map2_Either f (Right a) (Left e) = Left e
map2_Either f (Right a) (Right b) = Right (f a b)
-- Or: map2_Either f (Right a) eb = fmap (f a) eb

{- And what if you have a 3-ary function and three lists? Four?...  Do you have to
   code them up separately?  Or is there a generalization that covers them all.?

   Try this:
-}
ap_List :: [a -> b] -> [a] -> [b]
-- E.g., ap_List [f,g] [1,2,3] = [f 1, f 2, f 3, g 1, g 2, g 3]
--                             = map f [1,2,3] ++ (map g [1,2,3] ++ [])
ap_List [] as = []
ap_List (f:fs) as = map f as ++ ap_List fs as

-- Or: ap_List fs as = map2_List (\f -> \a -> f a) fs as
-- So ap_List can be derived from map2_List.

{- Now the converse: map2_List can be derived from ap_List. -}
map2_Listv2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2_Listv2 f as bs = ap_List (fmap f as) bs
-- Or: fmap f as `ap_List` bs

{- One way to understand is to work out some examples. Left as an exercise.

   Another way though is to work out the intermediate types:

   ap_List    (fmap       f          as)         bs
                      a->(b->c)      [a]
              |<--    [b -> c]      -->|         [b]
   |<---                   [c]                 --->|
-}

{- And 3-ary function and three lists, too.  And so on. -}
map3_List :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3_List f as bs cs = ((fmap f as) `ap_List` bs) `ap_List` cs

{- Again, exercise: work out some examples.

   But here are the intermediate types:

ap_List (ap_List (fmap f as)   bs)   cs
                 [b->(c->d)]  [b]   [c]
         |<---    [c->d]    --->|
|<---          [d]                --->|
-}

{- Analogous stories for Maybe and Either.  (Unfortunately not BinTree.)

   There is a class for this too!

   class Functor f => Applicative f where
       pure :: a -> f a
       (<*>) :: f (a -> b) -> f a -> f b

   "pure" plays two roles:
   * The degenerate case when you have a 0-ary function, kind of.
   * fmap f xs = pure f <*> xs

   Control.Applicative has "liftA2" and "liftA3" which are my "map2" and "map3".

   liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
   liftA2 binop as bs = fmap binop as <*> bs

   liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
   liftA3 triop as bs cs = (fmap triop as <*> bs) <*> cs
                         = fmap triop as <*> bs <*> cs

   Applicative is pretty useful in practice, beyond merely common names.
   Examples include liftA2, liftA3, and variations and generalizations, which
   work for all instances of Applicative.

   On the theory side, people are pretty sure that Applicative is important too,
   but it's a fairly recent invention (mathematicians say "discovery"), so there
   is little to show for now.

   * * *

   I now want you to think of [], Maybe, Either e as kinds of programs, not as
   kinds of data structures.

   * Maybe

     foo :: Maybe Int --- This is a program that may suceed and return an Int,
     or may fail.

     `fmap f foo :: Maybe Y` now means: A program that runs foo, then applies f
     to the answer in case of success.

     `pure 4 :: Maybe Int` now means: A program that succeeds and returns 4.

     `bar <*> foo :: Maybe Y` now means: A composite program that runs bar to
     try to obtain a function (call it f); if success, runs foo to try to obtain
     an Int (call it i); if success too, the overall answer is f i.

     `fmap (+) foo <*> foo2` aka `liftA2 (+) foo foo2`: A composite program that
     runs foo for an Int, runs foo2 for another Int, and adds them (if successes
     throughout).

     (Assume f :: Int -> Y, bar :: Maybe (Int -> Y), foo2 :: Maybe Int.)

     Example:
-}
addRecip :: Double -> Double -> Maybe Double
addRecip x y = fmap (+) (recipMay x) <*> recipMay y
  where
    recipMay a | a == 0 = Nothing
               | otherwise = Just (1 / a)
{- Much nicer than:

   addRecip x y =
       case recipMay x of
         Nothing -> Nothing
         Just a -> case recipMay y of
           Nothing -> Nothing
           Just b -> Just (1/a + 1/b)
-}

{-
   * Either e

     foo :: Either String Int --- Like Maybe, but if it fails, it gives you a
     String error message (say).

     `fmap f foo :: Either String Y` now means: Like Maybe, but preserves the
     error message if failure.

     `bar <*> foo :: Either String Y` now means: Like Maybe, but preserves the
     error message of the first failure.

     `fmap (+) foo <*> foo2` aka `liftA2 (+) foo foo2`: Likewise.

   * []

     foo :: [Int] --- This is a non-deterministic program that spawns parallel
     universes and returns a number in each universe.  (Perhaps 0 universes ---
     failure).

     `fmap f foo :: [Y]` now means: A program that runs foo, then in each
     universe, applies f to the answer.

     `pure 4 :: [Int]` means a program that just has one universe and returns 4
     in it.

     `bar <*> foo :: [Y]` now means: A composite program that runs bar (each
     universe has a function), runs foo (each universe has an Int), then for
     each pair of universes, applies the function to the Int.

     `fmap (+) foo <*> foo2` aka `liftA2 (+) foo foo2: A composite program that
     runs foo, runs foo2, then for each pair of universes, adds the respective
     pair of numbers.

   This is pretty versatile already, but there is a limitation.  In `liftA2 (+)
   foo foo2`, foo2 cannot say "I want to know what foo returns before deciding
   what I will return", or vice versa.

   What if you want that kind of data dependence?  Like

   bind :: F a -> (a -> F b) -> F b

   so that you can write `bind foo quaz` to mean: A composite program that runs
   foo, then passes the answer to quaz, and let it decide what to do next?

   And of course sometimes there is no answer --- failure, can't continue with
   quaz; or there are parallel universes --- so one copy of quaz per universe.
-}

bind_Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bind_Maybe Nothing _ = Nothing
bind_Maybe (Just a) k = k a

bind_Either :: Either e a -> (a -> Either e b) -> Either e b
bind_Either (Left e) _ = Left e
bind_Either (Right a) k = k a

bind_List :: [a] -> (a -> [b]) -> [b]
bind_List [] _ = []
bind_List (x:xs) k = k x ++ bind_List xs k

{- And there is a class for that too!

   class Applicative f => Monad f where
       return :: a -> f a
       (>>=) :: f a -> (a -> f b) -> f b

   "return" means the same as "pure".  (Do NOT think in terms of control
   flow. "return" does not exit anything.)  It is here for historical reasons
   (because Applicative is more recent).  It will become an alias for "pure" in
   the near future.

   (<*>) can be derived from pure and (>>=):

   fs <*> as =
       fs >>= (\f -> as >>= (\a -> pure (f a)))

   There is also "do-notation" so code looks nicer and the computer emits
   ">>= \v ->" for you:

   fs <*> as =
       do
           f <- fs
           a <- as
           pure (f a)

   Example (based on a true story):
-}

-- Intranet gives me student number, tutorial section
data IntranetRecord = I Integer String deriving Show

-- Blackboard gives me student number, UTORid
data BBRecord = B Integer String deriving Show

-- But MarkUs wants UTORid, tutorial section
data MarkUsRecord = M String String deriving Show

-- Obviously, do a relational join!
myJoin :: [IntranetRecord] -> [BBRecord] -> [MarkUsRecord]
myJoin is bs = [ M u t | I n1 t <- is,
                         B n2 u <- bs,
                         n1 == n2      ]

-- Equivalently:
myJoin2 is bs = do
    I n1 t <- is
    B n2 u <- bs
    if (n1 == n2)
        then pure (M u t)
        else []

{- Monad is very important in theory, in both advanced mathematics and
   programming language semantics.

   On the practical side, here are some uses (in addition to providing common
   names):

   * do-notation is a generalization of list comprehension, and it works for all
     Monad instances.

   * Before there was Applicative, Monad was behind liftA2 etc., e.g.,

         liftM2 binop as bs = do
             a <- as
             b <- bs
             return (binop a b)

   * There is another use shown later.  (Teaser: Dependency injection, template
     method, mock tests.)

   * Here is something that can't be done with just Applicative but can be done
     with Monad:
-}
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
{- It is like foldl but the "binary operator" is a "program" instead.  E.g.,

     foldM foo 0 [1,2,3] = do
         x <- foo 0 1
         y <- foo x 2
         z <- foo y 3
         pure z

   Here, think of [a] as the list data structure, no need to think of it as a
   non-deterministic program.  (In Control.Monad, there is a generalization from
   lists to other data structures.)
-}
foldM foo z [] = pure z
foldM foo z (a:as) = do
    z' <- foo z a
    foldM foo z' as

{- The state monad, or: how to fake state in functional programming.

   The trick is to have a state transition function instead, like s -> s, and
   have some starter function that feeds it the initial value.

   Except I also want it to give an answer.  So actually s -> (s, a): function
   from state-before to pair of state-after and answer.
-}
newtype State s a = StateOf (s -> (s, a))

deState :: State s a -> (s -> (s, a))
deState (StateOf stf) = stf

instance Functor (State s) where
    -- fmap :: (a -> b) -> State s a -> State s b
    fmap f (StateOf stf) = StateOf (\s0 -> case stf s0 of (s1, a) -> (s1, f a))

instance Applicative (State s) where
    -- pure :: a -> State s a
    pure a = StateOf (\s -> (s, a))

    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    StateOf stf1 <*> StateOf stf2 = StateOf
        (\s0 -> case stf1 s0 of (s1, f) -> case stf2 s1 of (s2, a) -> (s2, f a))

instance Monad (State s) where
    return = pure
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    StateOf stf1 >>= k = StateOf
        (\s0 -> case stf1 s0 of (s1, a) -> deState (k a) s1)

put :: s -> State s ()
put s = StateOf (\s0 -> (s, ()))

get :: State s s
get = StateOf (\s0 -> (s0, s0))

statefulSum :: [Integer] -> State Integer Integer
statefulSum [] = get
statefulSum (x:xs) = do
    s <- get
    put (s+x)
    statefulSum xs


{- Dependency inject / template method / mock test example. -}

-- File format checker for my toy file format:
-- First three characters should be A, L, newline.
-- Version 1.  To be superceded.
toyCheck :: IO Bool
toyCheck = do
    c1 <- getChar
    c2 <- getChar
    c3 <- getChar
    return ([c1, c2, c3] == "AL\n")

-- How do I test this?
-- And/Or how do I restrict it to getChar and nothing funny behind my back?
-- Answer: Dependency Injection (or is it Template Method?)

-- Several ways to do Dependency Injection in Haskell. Here's one.

class Monad f => MonadToyCheck f where
    toyGetChar :: f Char
-- Simplifying assumptions: Enough characters, no failure.  A practical version
-- should add methods for raising and catching EOF exceptions.

toyCheck2 :: MonadToyCheck f => f Bool
toyCheck2 = do
    c1 <- toyGetChar
    c2 <- toyGetChar
    c3 <- toyGetChar
    return ([c1, c2, c3] == "AL\n")

-- Only things toyCheck2 can do: toyGetChar, monad methods, purely functional
-- programming.  Because USER chooses f.  And toyCheck2 doesn't even know what
-- it is.  All it knows is it can call toyGetChar.

-- The real McToy
instance MonadToyCheck IO where
    toyGetChar = getChar

realProgram :: IO Bool
realProgram = toyCheck2

-- Fake news for purely functional testing.

newtype Feeder a = F (String -> (String, a))
-- Again, simplifying assumptions etc.  But basically like the state monad, with
-- the state being what's not yet consumed in the string.

unF (F sf) = sf

instance Monad Feeder where
    return a = pure a
    prog1 >>= k = F (\s0 -> case unF prog1 s0 of
                              (s1, a) -> unF (k a) s1)

instance MonadToyCheck Feeder where
    toyGetChar = F (\(c:s) -> (s, c))

instance Functor Feeder where
    fmap f p = do a <- p
                  return (f a)

instance Applicative Feeder where
    pure a = F (\s -> (s, a))
    pf <*> pa = do
        f <- pf
        a <- pa
        return (f a)

testToyChecker2 :: String -> Bool
testToyChecker2 str = case unF toyCheck2 str of
  (_, b) -> b

-- https://www.slideshare.net/ScottWlaschin/fp-patterns-buildstufflt
-- slide 13
