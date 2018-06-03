{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Functions where

import Control.Lens
import GHC.Generics
import Test.Tasty.HUnit

-- an example of complex code.
-- We are actually not testing that code but the compilation of it.

data T = T { _hello :: String
           , _world :: Int } deriving (Show, Eq, Generic)

makeLenses ''T

f :: [T] -> [T]
f = over (traverse.world) (+ 1)

-- minLazy creates a thunk
minLazy :: Int -> Int -> Int
minLazy x y = min x y

minStrict :: Int-> Int -> Int
minStrict x y = id $! min x y

-- example from https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode
build_data :: Int -> Maybe Int
build_data x = Just (x + 1)

build_data_strict :: Int -> Maybe Int
build_data_strict x = Just $! (x + 1)
