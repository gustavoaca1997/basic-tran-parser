module Parsed where
import Control.Applicative
-- Monad para manejar errores
data Parsed a = Ok a | Failed String deriving Show

-- Instanciamos el typeclass Functor
instance Functor Parsed where
    fmap f (Failed msg) = Failed msg
    fmap f (Ok a) = Ok (f a)

-- Instanciamos el typeclass Applicative
instance Applicative Parsed where
    pure a = Ok a
    (Failed msg) <*> _ =  Failed msg
    (Ok f) <*> m = fmap f m     

-- Instanciamos el typeclass Monad
instance Monad Parsed where
    return a = Ok a
    fail msg = Failed msg
    (Failed msg) >>= f = Failed msg
    (Ok a) >>= f = f a
-- manejador de errores
catchParsed :: Parsed a -> (String -> Parsed a) -> Parsed a
catchParsed m k =
    case m of
        Ok a -> Ok a
        Failed e -> k e