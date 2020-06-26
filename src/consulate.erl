-module(consulate).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(version, 5).

-export([start_link/0,
         register_node/2,
         register_node/3,
         port_please/2,
         port_please/3,
         listen_port_please/2,
         address_please/3,
         names/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

start_link() ->
    ?LOG_DEBUG("consulate:start_link()"),
    gen_server:start_link({local, consulate}, ?MODULE, [], []).

register_node(Name, Port) ->
    ?LOG_DEBUG("consulate:register_node(~p, ~p)", [Name, Port]),
    case consulate_client:register(Name, Port) of
        ok -> gen_server:call(consulate, {set_name, Name});
        error -> {error, cannot_register}
    end.

register_node(Name, Port, _Driver) ->
    register_node(Name, Port).

port_please(Name, Host) ->
    port_please(Name, Host, infinity).

port_please(Name, Host, Timeout) ->
    ?LOG_DEBUG("consulate:port_please(~p, ~p)", [Name, Host, Timeout]),
    case consulate_client:get_service(Name, Host) of
        {ok, #{<<"Port">> := Port}} ->
            {ok, Port, ?version};
        _ ->
            {error, unavailable}
    end.

listen_port_please(Name, Host) ->
    ?LOG_DEBUG("consulate:listen_port_please(~p, ~p)", [Name, Host]),
    Port = case consulate_client:get_local(Name) of
               {ok, #{<<"Port">> := P}} -> P;
               error -> erl_epmd:listen_port_please(Name, Host)
           end,
    {ok, Port}.

address_please(Name, Host, _AddressFamily) ->
    ?LOG_DEBUG("consulate:address_please(~p, ~p)", [Name, Host]),
    case consulate_client:get_service(Name, Host) of
        {ok, [#{<<"Address">> := Addr, <<"ServicePort">> := Port}]} ->
            {ok, IP} = inet:parse_address(binary_to_list(Addr)),
            {ok, IP, Port, ?version};
        _ ->
            {error, unavailable}
    end.

names(localhost) ->
    case consulate_client:list_local_services() of
        {ok, Payload} ->
            Services = maps:map(fun(_K, #{<<"Port">> := P}) -> P end, Payload),
            {ok, [{binary_to_list(K), V} || {K, V} <- maps:to_list(Services)]};
        error ->
            {error, address}
    end;
names(Host) ->
    ?LOG_DEBUG("consulate:names(~p)", [Host]),
    case consulate_client:list_services(Host) of
        {ok, Payload} ->
            {ok, [{binary_to_list(K), V} || #{<<"ServiceID">> := K, <<"ServicePort">> := V} <- Payload]};
        error ->
            {error, address}
    end.

init(_) -> {ok, {0, []}}.

handle_call({set_name, Name}, _Ref, {Num, _}) ->
    {reply, {ok, Num + 1}, {Num + 1, {name, Name}}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, {_, {name, Name}}) ->
    ?LOG_DEBUG("Deregister ~p", [Name]),
    ok = consulate_client:deregister(Name),
    ok;
terminate(_Reason, State) ->
    ?LOG_DEBUG("Not deregistering - ~p", [State]),
    ok.
