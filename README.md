# hs-opentelemetry-instrumentation-servant

Experimental [WAI](wai) middleware to instrument OpenTelemetry spans with [Servant][servant] routing information.

### Why

This middleware adds the `http.route` attribute to the span created by the [WAI OpenTelemetry middleware][hs-opentelemetry-instrumentation-wai].
Dynamic path segments in this attribute are replaced by their placeholder name.

For example, imagine the following Servant API:

```haskell
-- GET /hello/:name
type Api = "hello" :> Capture "name" Text :> Get '[JSON] Text
```

The WAI instrumentation can capture the path segments of the requested URL.
This is recorded as `http.target`.

But by querying Servant, we can figure out which paths segments are dynamic vs static and replace them with the symbol defined in the API.
In this case, `there` is replaced by `:name`.

| `http.target` | `http.route` |
| ------------- | ------------ |
| hello/there   | hello/:name  |

The main use-cases for this are to:

1. Filter traces by endpoint.
2. Compute per-endpoint metrics with the [`spanmetrics`][spanmetricsconnector] connector.

### Usage

See the sample server in [`exe/Main.hs`](./exe/Main.hs).

### Demo

1. Install [`devenv`](https://devenv.sh).

2. Launch the processes.
   This launches the demo server and an instance of the OpenTelemetry Collector.

   ```shell
   devenv up
   ```

3. Query the server on port `4567`.

   ``` shell
   curl http://localhost:4567/hello/there
   ```

   Verify that the trace is recorded in the OpenTelemetry Collector.

[wai]: https://github.com/yesodweb/wai
[servant]: https://github.com/haskell-servant/servant
[hs-opentelemetry-instrumentation-wai]: https://github.com/iand675/hs-opentelemetry/tree/main/instrumentation/wai
[spanmetricsconnector]: https://github.com/open-telemetry/opentelemetry-collector-contrib/tree/main/connector/spanmetricsconnector
