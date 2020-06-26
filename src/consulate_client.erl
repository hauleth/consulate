-module(consulate_client).

-include_lib("kernel/include/logger.hrl").

-define(service, case init:get_argument(consul_service) of
                     {ok, [[Name]]} -> list_to_binary(Name);
                     _ -> <<"erlang-service">>
                 end).

-compile({no_auto_import, [get/1, put/2]}).

-export([register/2,
         deregister/1,
         list_local_services/0,
         list_services/1,
         get_local_service/1,
         get_service/2]).

register(Name, Port) ->
    Service = #{
      'ID' => Name,
      'Name' => ?service,
      'Port' => Port,
      'Check' => #{
        'Name' => <<"Check Erlang Distribuition Port">>,
        'Interval' => <<"30s">>,
        'DeregisterCriticalServiceAfter' => <<"1m">>,
        'TCP' => list_to_binary(io_lib:format("localhost:~B", [Port])),
        'Status' => <<"passing">>
       },
      'EnableTagOverride' => false
     },
    put("/v1/agent/service/register", Service).

deregister(Name) ->
    Endpoint = io_lib:format("/v1/agent/service/deregister/~s", [Name]),
    put(Endpoint, #{}).

list_local_services() ->
    get("/v1/agent/services").

list_services(Host) ->
    Path = io_lib:format("/v1/catalog/service/~s", [?service]),
    Filter = io_lib:format("Node == \"~s\"", [Host]),
    Endpoint = {Path, [{"filter", Filter}]},
    get(Endpoint).

get_local_service(Name) ->
    Endpoint = io_lib:format("/v1/agent/service/~s", [Name]),
    get(Endpoint).

get_service(Name, Host) ->
    Path = io_lib:format("/v1/catalog/service/~s", [?service]),
    Filter = io_lib:format("ServiceID == \"~s\" and Node == \"~s\"", [Name, Host]),
    Endpoint = {Path, [{"filter", Filter}]},
    get(Endpoint).

get(Endpoint) ->
    URL = build_url(Endpoint),

    ?LOG_DEBUG("consulate:get(~p) -> ~p", [Endpoint, URL]),

    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(Body, [return_maps])};
        {ok, {200, Body}} ->
            {ok, jsx:decode(Body, [return_maps])};
        Resp -> {error, Resp}
    end.

put(Endpoint, Message) ->
    Body = jsx:encode(Message),
    URL = build_url(Endpoint),

    ?LOG_DEBUG("consulate:put(~p, ~p) -> ~p @ ~p", [Endpoint, Message, URL, Body]),

    case httpc:request(put, {URL, [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {200, _}} -> ok;
        _ -> error
    end.

build_url({Path, Query}) ->
    URI = #{
      host => application:get_env(consulate, host, "127.0.0.1"),
      path => Path,
      port => application:get_env(consulate, port, 8500),
      scheme => application:get_env(consulate, scheme, "http"),
      query => uri_string:compose_query(Query)
     },
    uri_string:recompose(URI);
build_url(Path) ->
    build_url({Path, []}).
