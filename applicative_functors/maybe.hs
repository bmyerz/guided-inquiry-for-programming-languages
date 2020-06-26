import Prelude hiding (Maybe, Just, Nothing)
-- we hide Maybe and its constructors so that we
-- can define it ourselves and thereby
-- define its instances of Functor and Applicative ourselves

data Maybe a = Nothing | Just a
  deriving Show

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where 
    -- pure :: a -> Maybe a


    -- <*> :: Maybe (a -> b) -> Maybe a -> Maybe b

