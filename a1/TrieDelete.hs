module TrieDelete where

import           TrieDef

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


trieDelete :: [Char] -> Trie a -> Trie a
-- base case
trieDelete "" (TrieNode maybe (childrenMap)) = (TrieNode Nothing childrenMap)
trieDelete str@(x:xs) (TrieNode maybe childrenMap)
    = case Map.lookup x childrenMap of
        Nothing -> (TrieNode maybe childrenMap)
        Just node@(TrieNode child_maybe childMap) -> case trieIsEmpty (trieDelete xs (myUnwrapper(Map.lookup x childrenMap))) of
                                                    True -> (TrieNode maybe (Map.delete x childrenMap))
                                                    False -> (TrieNode maybe (Map.insert x (trieDelete xs (myUnwrapper(Map.lookup x childrenMap))) childrenMap))
--helper myUnwrapper
myUnwrapper:: Maybe a -> a
myUnwrapper (Just node) = node





-- printTrie (trieDelete "" albertTrie)











