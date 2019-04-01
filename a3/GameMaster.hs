module GameMaster where

import Control.Monad (ap, liftM)

import GameMasterDef
import Data.Maybe

-- Question 1.

-- The game master for the guessing game.  The parameter is the secret number
-- the player has to guess.
guessingGame :: MonadGameMaster m => Integer -> m Ending
guessingGame secret
    | secret < 1 || secret > 100 = error "invalid game"
    | otherwise = guessingGameHelper 1 100 secret

guessingGameHelper ::  MonadGameMaster m => Integer -> Integer -> Integer -> m Ending
guessingGameHelper lowerBound upperBound secret = do
    req1 <- gmAction lowerBound upperBound
    case req1 of
      Surrender -> return (Lose secret)
      Guess i
            | (i > upperBound) || (i < lowerBound) -> guessingGameHelper lowerBound upperBound secret
            | i == secret -> return Win
            | i < secret -> guessingGameHelper (i+1) upperBound secret
            | i > secret -> guessingGameHelper lowerBound (i-1) secret

        
            

-- Question 2.
-- data FreeGameMaster a
--     = Pure a
--     | GMAction Integer Integer (PlayerMsg -> FreeGameMaster a)
instance Functor FreeGameMaster where
    -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- fmap f (Pure a) = Pure (f a)
    -- fmap f (GMAction lowerBound upperBound (next)) = fmap f next
    -- fmap f (GMAction lowerBound upperBound (next)) = (GMAction lowerBound upperBound (\s0 -> case next s0 of 
    --                                                                                                     Pure a -> fmap f (Pure a)    
    --                                                                                                     w -> fmap f w))

    -- If you are confident with your Monad instance, you can just write
    fmap = liftM

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- If you are confident with your Monad instance, you can just write
    pure = return

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a
    return a = Pure a
    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    (GMAction lowerBound upperBound (next)) >>= f = (GMAction lowerBound upperBound (\s0 -> case next s0 of 
                                                                                                    Pure a -> (f a)    
                                                                                                    w -> (w >>= f)))
    
instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    gmAction lowerBound upperBound = (GMAction lowerBound upperBound (\s0 -> case s0 of 
                                                                            Surrender -> (Pure Surrender)    
                                                                            Guess i -> (Pure (Guess i))))

-- Question 3.

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame f 
    | testGameHelper (f 45) == Just False = False
    | otherwise = True




testGameHelper:: (FreeGameMaster Ending) -> Maybe Bool
testGameHelper gm = do
    case checkBound 1 100 gm of
        Nothing -> Just False
        (Just next_0)-> case checkBound 11 100 (next_0 (Guess 10)) of
                            Nothing -> Just False
                            (Just next_1) -> case winHelper (next_1 (Guess 45)) 45 45 of
                                Nothing -> Just False
                                (Just x) -> case checkBound 1 100 gm of
                                                Nothing -> Just False
                                                (Just next_0)-> case checkBound 1 49 (next_0 (Guess 50)) of
                                                                    Nothing -> Just False
                                                                    (Just next_1) -> case loseHelper (next_1 (Surrender)) Surrender 45 of
                                                                        Nothing -> Just False      
                                                                        (Just x) -> Just True                     


        


checkBound:: Integer -> Integer -> (FreeGameMaster Ending) -> Maybe (PlayerMsg-> FreeGameMaster Ending)
checkBound lowerBound upperBound  (GMAction l u next) 
                                                    | (l == lowerBound && u == upperBound) = Just (next)
                                                    | otherwise = Nothing
checkBound lowerBound upperBound _ = Nothing

winHelper:: (FreeGameMaster Ending) -> Integer ->Integer -> Maybe (FreeGameMaster Ending)
winHelper (Pure ending) i secret
            | (ending == Win && i == secret) = Just (Pure ending)
            | otherwise = Nothing
winHelper _ i secret = Nothing

loseHelper:: (FreeGameMaster Ending) -> PlayerMsg ->Integer -> Maybe (FreeGameMaster Ending)
loseHelper (Pure (Lose n)) Surrender secret
            | (n == secret) = Just (Pure (Lose n))
            | otherwise = Nothing
loseHelper _ _ secret = Nothing