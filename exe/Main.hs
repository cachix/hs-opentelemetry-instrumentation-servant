{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text (Text)
import Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Instrumentation.Servant (openTelemetryServantMiddleware)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace (TracerProvider, initializeGlobalTracerProvider, shutdownTracerProvider)
import Servant
import UnliftIO.Exception (bracket)

port :: Int
port = 4567

type Api = "hello" :> Capture "name" Text :> Get '[JSON] Text

api :: Proxy Api
api = Proxy

server :: Server Api
server name = return $ "hello, " <> name <> "!"

main :: IO ()
main = withTracerProvider $ \tracerProvider -> do
  openTelemetryWaiMiddleware <- newOpenTelemetryWaiMiddleware

  putStrLn $ "Starting server on port " <> show port

  Warp.run port $
    openTelemetryWaiMiddleware $
      openTelemetryServantMiddleware tracerProvider api $
        serve api server

withTracerProvider :: (TracerProvider -> IO a) -> IO a
withTracerProvider =
  bracket initializeGlobalTracerProvider shutdownTracerProvider
