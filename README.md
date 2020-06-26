Consulate
=====

The implementation of `erl_epmd`-compatible module that uses Consul for node
discover instead of EPMD.

## Usage

Add project to your dependencies

Erlang:

```erlang
{deps, [consulate]}.
```

Elixir:

```elixir
defp deps do
  [
    {:consulate, "~> 0.0"}
  ]
end
```

And then run your application with

```
-epmd_module consulate -start_epmd false
```

## Configuration

You can change default `erlang-node` service name via `-consul_service` flag to
`erl`. Beware that all nodes that want to be connected need to use the same
service name.

Consul connection options are set via application environment. Supported options
are:

- `host` (defaults to `127.0.0.1`) - host to Consul agent
- `port` (defaults to `8500`) - port on which agent is listening
- `scheme` (defaults to `http`) - connection scheme, can be `"http"`
   or `"https"`
- `check` - map containing 2 fields:
  + `interval` (defaults to `10`) - time in seconds between each health check
  + `deregister` (defaults to `60`) - time in seconds after which the service
    will be deregistered from catalog after it fails test
- `meta` - map of `atom() => atom() | binary()` pairs that will be submitted as
  service metadata

## License

See [LICENSE](LICENSE)
