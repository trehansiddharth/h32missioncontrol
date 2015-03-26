module Applicative.Reader where

type alias Reader r a = r -> a

pure : a -> Reader r a
pure = always

(<*>) : Reader r (a -> b) -> Reader r a -> Reader r b
f <*> x = \r -> (f r) (x r)

(<$>) : (a -> b) -> Reader r a -> Reader r b
f <$> x = pure f <*> x

env : Reader r r
env = identity