{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- Adapted from https://github.com/haskell-servant/servant-ekg
--
-- Copyright (c) 2015, Anchor Systems
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Anchor Systems nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module OpenTelemetry.Instrumentation.Servant.Internal
  ( ServantEndpoint (..),
    HasEndpoint (..),
  )
where

import Control.Monad (mplus)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (Method)
import Network.Wai (Request (..))
import Servant.API

data ServantEndpoint = ServantEndpoint
  { method :: Method,
    pathSegments :: [Text]
  }

class HasEndpoint a where
  getEndpoint :: Proxy a -> Request -> Maybe ServantEndpoint

instance HasEndpoint EmptyAPI where
  getEndpoint _ _ = Nothing

instance (HasEndpoint (a :: Type), HasEndpoint (b :: Type)) => HasEndpoint (a :<|> b) where
  getEndpoint _ req =
    getEndpoint (Proxy :: Proxy a) req
      `mplus` getEndpoint (Proxy :: Proxy b) req

instance
  (KnownSymbol (path :: Symbol), HasEndpoint (sub :: Type)) =>
  HasEndpoint (path :> sub)
  where
  getEndpoint _ req =
    case pathInfo req of
      p : ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
        ServantEndpoint {pathSegments, method} <- getEndpoint (Proxy :: Proxy sub) req {pathInfo = ps}
        return (ServantEndpoint method (p : pathSegments))
      _ -> Nothing

instance
  (KnownSymbol (capture :: Symbol), HasEndpoint (sub :: Type)) =>
  HasEndpoint (Capture' mods capture a :> sub)
  where
  getEndpoint _ req =
    case pathInfo req of
      _ : ps -> do
        ServantEndpoint {pathSegments, method} <- getEndpoint (Proxy :: Proxy sub) req {pathInfo = ps}
        let p = T.pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
        return (ServantEndpoint method (p : pathSegments))
      _ -> Nothing

instance (HasEndpoint (sub :: Type)) => HasEndpoint (Summary d :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (Description d :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (Header' mods h a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

#if MIN_VERSION_servant(0,18,2)
instance HasEndpoint (sub :: Type) => HasEndpoint (Fragment a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

instance (HasEndpoint (sub :: Type)) => HasEndpoint (QueryParam' mods (h :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (QueryParams (h :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (QueryFlag h :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (ReqBody' mods cts a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

#if MIN_VERSION_servant(0,15,0)
instance HasEndpoint (sub :: Type) => HasEndpoint (StreamBody' mods framing ct a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
#endif

instance (HasEndpoint (sub :: Type)) => HasEndpoint (RemoteHost :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (IsSecure :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (HttpVersion :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (Vault :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (WithNamedContext x y sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (ReflectMethod method) => HasEndpoint (Verb method status cts a) where
  getEndpoint _ req =
    case pathInfo req of
      [] | requestMethod req == method -> Just (ServantEndpoint method [])
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

#if MIN_VERSION_servant(0,17,0)
instance ReflectMethod method => HasEndpoint (NoContentVerb method) where
  getEndpoint _ req =
    case pathInfo req of
      [] | requestMethod req == method -> Just (ServantEndpoint method [] )
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)
#endif

#if MIN_VERSION_servant(0,18,1)
instance ReflectMethod method => HasEndpoint (UVerb method contentType as) where
  getEndpoint _ req =
    case pathInfo req of
      [] | requestMethod req == method -> Just (ServantEndpoint method [] )
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)
#endif

instance (ReflectMethod method) => HasEndpoint (Stream method status framing ct a) where
  getEndpoint _ req =
    case pathInfo req of
      [] | requestMethod req == method -> Just (ServantEndpoint method [])
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint Raw where
  getEndpoint _ _ = Just (ServantEndpoint "RAW" [])

instance (HasEndpoint (sub :: Type)) => HasEndpoint (CaptureAll (h :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance (HasEndpoint (sub :: Type)) => HasEndpoint (BasicAuth (realm :: Symbol) a :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
