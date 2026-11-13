{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

#if MIN_VERSION_servant(0,20,3)
import Control.Monad (unless)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types (Method)
import Network.Wai (Request (..), defaultRequest)
import OpenTelemetry.Instrumentation.Servant.Internal
  ( HasEndpoint (getEndpoint),
    ServantEndpoint (method, pathSegments),
  )
import Servant.API
import Servant.API.MultiVerb (MultiVerb, Respond)
import System.Exit (exitFailure)

-- A record of routes shaped the way applications actually write them:
-- NamedRoutes, MultiVerb responses, and an auth combinator on the field.
data WidgetApi mode = WidgetApi
  { getWidget ::
      mode
        :- Capture "widgetId" Text
          :> MultiVerb 'GET '[JSON] '[Respond 200 "Found" Text] Text,
    createWidget ::
      mode :- MultiVerb 'POST '[JSON] '[Respond 201 "Created" Text] Text
  }
  deriving (Generic)

data ServiceApi mode = ServiceApi
  { widgets :: mode :- "widgets" :> AuthProtect "service-jwt" :> NamedRoutes WidgetApi,
    live :: mode :- "health" :> "live" :> Verb 'GET 200 '[JSON] Text
  }
  deriving (Generic)

serviceApi :: Proxy (NamedRoutes ServiceApi)
serviceApi = Proxy

request :: Method -> [Text] -> Request
request m segs = defaultRequest {requestMethod = m, pathInfo = segs}

-- | @Just (method, route)@ in the same shape the middleware renders it.
route :: Method -> [Text] -> Maybe (Method, Text)
route m segs = fmap render (getEndpoint serviceApi (request m segs))
  where
    render e = (method e, T.intercalate "/" (pathSegments e))

check :: String -> Maybe (Method, Text) -> Maybe (Method, Text) -> IO Bool
check label expected actual
  | expected == actual = pure True
  | otherwise = do
      putStrLn $ "FAIL " <> label
      putStrLn $ "  expected: " <> show expected
      putStrLn $ "  actual:   " <> show actual
      pure False

main :: IO ()
main = do
  results <-
    sequence
      [ check
          "MultiVerb behind a Capture, through AuthProtect and NamedRoutes"
          (Just ("GET", "widgets/:widgetId"))
          (route "GET" ["widgets", "abc"]),
        check
          "MultiVerb directly under a path literal"
          (Just ("POST", "widgets"))
          (route "POST" ["widgets"]),
        check
          "plain Verb still resolves"
          (Just ("GET", "health/live"))
          (route "GET" ["health", "live"]),
        -- The important negative: a MultiVerb whose method does not match must
        -- return Nothing so the alternation keeps trying its siblings, rather
        -- than claiming the request with a wrong or empty route.
        check
          "method mismatch falls through instead of matching"
          Nothing
          (route "DELETE" ["widgets", "abc"]),
        check
          "unrouted path yields no endpoint"
          Nothing
          (route "GET" ["nope"])
      ]
  unless (and results) exitFailure
  putStrLn $ "All " <> show (length results) <> " endpoint tests passed."

#else

main :: IO ()
main = putStrLn "servant < 0.20.3: MultiVerb tests skipped."

#endif
