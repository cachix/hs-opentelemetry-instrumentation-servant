{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.Servant (openTelemetryServantMiddleware) where

import qualified Data.HashMap.Strict as H
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Network.Wai (Middleware)
import OpenTelemetry.Attributes.Key (unkey)
import qualified OpenTelemetry.Context as Context
import OpenTelemetry.Instrumentation.Servant.Internal
  ( HasEndpoint (getEndpoint),
    ServantEndpoint (method, pathSegments),
  )
import OpenTelemetry.Instrumentation.Wai (requestContext)
import qualified OpenTelemetry.SemanticConventions as SC
import OpenTelemetry.SemanticsConfig
  ( StabilityOpt (Old, Stable, StableAndOld),
    getSemanticsOptions,
    httpOption,
  )
import OpenTelemetry.Trace.Core
  ( SpanArguments (attributes, kind),
    SpanKind (Internal, Server),
    Tracer,
    TracerProvider,
    addAttributes,
    defaultSpanArguments,
    inSpan',
    makeTracer,
    toAttribute,
    tracerOptions,
  )

openTelemetryServantMiddleware :: (HasEndpoint api) => TracerProvider -> Proxy api -> Middleware
openTelemetryServantMiddleware tp api = do
  let tracer = makeTracer tp "hs-opentelemetry-instrumentation-servant" tracerOptions
  middleware' tracer
  where
    middleware' :: Tracer -> Middleware
    middleware' tracer application request respond =
      case getEndpoint api request of
        Nothing -> application request respond
        Just endpoint -> do
          let mspan = requestContext request >>= Context.lookupSpan

          semanticsOptions <- getSemanticsOptions
          let routeName = T.intercalate "/" $ pathSegments endpoint
              methodName = decodeLatin1 (method endpoint)
              baseAttributes =
                [ ("http.framework", toAttribute ("servant" :: Text)),
                  (unkey SC.http_route, toAttribute routeName)
                ]
              methodAttributes = case httpOption semanticsOptions of
                Stable ->
                  [(unkey SC.http_request_method, toAttribute methodName)]
                StableAndOld ->
                  [ (unkey SC.http_request_method, toAttribute methodName),
                    (unkey SC.http_method, toAttribute methodName)
                  ]
                Old ->
                  [(unkey SC.http_method, toAttribute methodName)]
              sharedAttributes = H.fromList (baseAttributes <> methodAttributes)
              args =
                defaultSpanArguments
                  { kind = maybe Server (const Internal) mspan,
                    attributes = sharedAttributes
                  }

          case mspan of
            Nothing -> do
              inSpan' tracer routeName args $ \_s ->
                -- TODO: mark the span with error on 5xx
                application request respond
            Just waiSpan -> do
              addAttributes waiSpan sharedAttributes
              application request respond
