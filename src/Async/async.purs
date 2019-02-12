module Async where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

foreign import data Async :: Type -> Type
foreign import fromCb :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Async a
foreign import runAsync :: forall a. Async a -> Effect Unit

-- doGet url = fromCb :: ajaxGet url
-- ajaxGet :: String -> (Resp -> Effect Unit) -> Effect Unit 


x :: Async String
x = fromCb (\_ -> log "hi")