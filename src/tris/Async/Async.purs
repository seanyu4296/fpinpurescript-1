module Async where 

import Prelude

-- import Control.Monad.Cont (ContT(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Effect (Effect)
-- import Data.Newtype (class Newtype, unwrap)

-- foreign import data FS :: Type -> Type

type ErrorCode = String
type FilePath = String

-- effect is just something wrapped in function()

foreign import readFileImpl :: Fn3 FilePath (String -> Effect Unit) (ErrorCode -> Effect Unit) (Effect Unit)

foreign import writeFileImpl :: Fn4 FilePath String (Effect Unit) (ErrorCode -> Effect Unit) (Effect Unit)


readFile :: FilePath -> (Either ErrorCode String -> Effect Unit) -> Effect Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: FilePath -> String -> (Either ErrorCode Unit -> Effect Unit) -> Effect Unit
writeFile path d k = runFn4 writeFileImpl path d (k $ Right unit) (k <<< Left) 

newtype ContT r m a = ContT ((a -> m r) -> m r)
-- Possible meanings of type var:
-- return
-- monad 
-- anything 


-- do this effect Unit asynchronous tasks
type Async a = ContT Unit Effect a -- this is partially applied it is missing the "a" of ContT




instance functorContT :: Functor m => Functor (ContT r m) where
  map :: forall a b. (a -> b) -> ContT r m a -> ContT r m b
  map f (ContT x) = ContT (\cb -> x $ (\a -> cb $ f a))

-- x is function accept (a -> m r)
-- the role of ContT functor is to transform a to b when passed to the callback
-- instance functorContT :: Functor m => Functor (ContT r m) where
  -- map :: a -> b -> ContT r m a -> Cont r m b
  -- x is ((a -> m r) -> m r) a function accepting a callback
  -- we need it to transform to (b -> mr -> mr b)
  -- how: change the value passed to cb to pass b not a
  -- fa = b
  -- cb $ fa = b -> m r
--   map f (ContT x) = ContT (\cb -> x (\a -> cb $ f a))
{- 
function(transformer, functionThing) {
  return function(cb) {
    return function() {
      functionThing(function() {
        cb(f(a))
      })
    }
  }
}
 -}

instance applyContT :: Apply m => Apply (ContT r m) where 
  apply :: forall a b. ContT r m (a -> b) -> ContT r m a -> ContT r m b
  -- y = (((a ->b) -> m r) -> m r)
  apply (ContT y) (ContT x) = ContT (\cb -> x $ (\a -> y $ (\f -> cb $ f a)))


--instance applyContT :: Apply m => Apply (ContT r m) where
  -- apply :: ContT r m (a -> b) -> ContT r m a -> ContT r m b
  -- ContT (((a ->b) -> m r) -> m r)
  -- g = a -> b
  --apply (ContT f) (ContT x) = ContT (\cb -> f (\g -> x (\a -> cb $ g a)))


instance applicativeContT :: Applicative m => Applicative (ContT r m) where
  pure :: forall a. a -> ContT r m a
  pure a = ContT (\cb -> cb a)

-- given a thing pass it to the callback in ContT 
-- instance applicativeContT :: Applicative m => Applicative (ContT r m) where
--  pure a = ContT (\cb -> cb a)

instance bindContT :: Bind m => Bind (ContT r m ) where
  bind :: forall a b. ContT r m a -> (a -> ContT r m b) -> ContT r m b
  bind (ContT x) f = ContT $ \cb -> x $ \a -> case f a of ContT y -> y cb
{- 
instance bindContT :: Bind m => Bind (ContT r m) where 
  bind :: forall a b. ContT r m a -> (a -> ContT r m b) -> ContT r m b
-- cb needs to accept b
  bind (ContT x) f = ContT (\cb -> x (\a -> case f a of
      -- y is ((b -> m r) ->  m r) 
      ContT y -> y cb
    )
  ) -}

readFileCont :: FilePath -> Async (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: FilePath -> String -> Async (Either ErrorCode Unit)
writeFileCont path d = ContT $ writeFile path d

copyFileCont :: FilePath -> FilePath -> Async (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err -> pure $ Left err
    Right content -> writeFileCont dest content
