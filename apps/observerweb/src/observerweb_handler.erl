%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_handler).
-author("bill@freecnpro.net").

-include("observerweb.hrl").

%% API
-export([init/2]).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

init(Req, State) ->
    handle(Req, State).

handle(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req2 = process(Method, HasBody, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

json_encode(Term) ->
    iolist_to_binary(jsone:encode(Term)).

process(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
process(<<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:read_urlencoded_body(Req),
    {_, Action} = lists:keyfind(<<"action">>, 1, PostVals),
    case Action of
        <<"get_sys">> ->
            Body = do_process(get_sys, get_acc_node()),
            reply(200, Body, Req2);
        <<"get_perf">> ->
            {_, Type} = lists:keyfind(<<"type">>, 1, PostVals),
            Body = do_process(get_perf, {get_acc_node(), binary_to_atom(Type, latin1)}),
            reply(200, Body, Req2);
        <<"get_malloc">> ->
            Body = do_process(get_malloc, get_acc_node()),
            reply(200, Body, Req2);
        <<"get_pro">> ->
            {_, Type} = lists:keyfind(<<"type">>, 1, PostVals),
            Body = do_process(get_pro, Type),
            reply(200, Body, Req2);
        <<"change_node">> ->
            {_, Node} = lists:keyfind(<<"node">>, 1, PostVals),
            Result = do_process(change_node, Node),
            reply(200, Result, Req2);
        <<"connect_node">> ->
            {_, Node} = lists:keyfind(<<"node">>, 1, PostVals),
            {_, Cookie} = lists:keyfind(<<"cookie">>, 1, PostVals),
            Result = case do_process(connect_node, {Node, Cookie}) of
                 pang -> <<"Connect failed">>;
                 pong -> <<"ok">>
               end,
            reply(200, Result, Req2);
        <<"get_nodes">> ->
            Body = json_encode({[{<<"nodes">>, get_bare_nodes()}]}),
            reply(200, Body, Req2);
        <<"del_node">> ->
            {_, Node} = lists:keyfind(<<"node">>, 1, PostVals),
            del_node(Node),
            Req2;
        <<"get_app_vsn">> ->
            Result = do_process(get_app_vsn, undefined),
            reply(200, Result, Req2)
    end;
process(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, Body, Req).

do_process(get_sys, Node) ->
    {Info, Stat} = observerweb_sys:sys_info(Node),
    [{_SysName, SysValue},{_CPUName, CPUValue}] = Info,
    [{_MemName, MemValue},{_StatName, StatValue}] = Stat,
    json_encode({[{<<"system">>, wrap_info(info, SysValue)},
        {<<"cpu">>, wrap_info(info, CPUValue)},
        {<<"memory">>, wrap_info(info,MemValue)},
        {<<"statistics">>, wrap_info(info, StatValue)}]});

do_process(get_perf, {Node, Type}) ->
    Data0 = observerweb_perf:perf_info(Node, Type),
    case Type of
        scheduler ->
            Data = wrap_info(scheduler, Data0),
            json_encode({[{<<"scheduler">>, Data}]});
        _ ->
            json_encode({Data0})
    end;

do_process(get_malloc, Node) ->
    Data = observerweb_alloc:memory_alloc_info(Node),
    json_encode({[{<<"allocator">>, wrap_info(alloc, Data)}]});

do_process(get_pro, Type) ->
    case Type of
        <<"all">> ->
            Data = observerweb_pro:update(),
            json_encode(Data);
        _ ->
            io:format("Type: ~p~n", [Type])
    end;

do_process(change_node, Value) ->
    Node = binary_to_atom(Value, latin1),
    case lists:keyfind(Node, 1, get_nodes()) of
        {'nonode@nohost', _} ->
            <<"Node not in distribution">>;
        {_, Cookie} ->
        io:format("Change Node: ~p~n", [Node]),
        erlang:set_cookie(node(), Cookie),
        case net_adm:ping(Node) of
            pang ->
                <<"false">>;
            pong ->
                insert_Data(acc_node, Node),
                observerweb_pro:change_node(Node),
                <<"true">>
        end;
        false ->
            <<"Node invalid">>
    end;
do_process(connect_node, {Value1, Value2}) ->
    try
        Node = binary_to_atom(Value1, latin1),
        Cookie = binary_to_atom(Value2, latin1),
        io:format("Node: ~p~nCookie: ~p~n", [Node, Cookie]),
        erlang:set_cookie(node(), Cookie),
        case net_adm:ping(Node) of
            pang -> pang;
            pong ->
                add_node({Node, Cookie}),
                insert_Data(acc_node, Node),
            pong
        end
    catch _:_ ->
        pang
    end;

do_process(get_app_vsn, _) ->
    json_encode({[{<<"app_vsn">>, list_to_binary(observerweb:vsn())}]}).

add_node({Node, Cookie}) ->
    Nodes = get_nodes(),
    NewNodes = case lists:keyfind(Node, 1, Nodes) of
               false -> [{Node, Cookie} | Nodes];
               _ -> [{Node, Cookie} | lists:keydelete(Node, 1, Nodes)]
             end,
    io:format("Nodes: ~p~n~p~n", [Nodes, NewNodes]),
    insert_Data(nodes, NewNodes).

del_node(Node) ->
    NewNodes = lists:keydelete(Node, 1, get_nodes()),
    insert_Data(nodes, NewNodes).

wrap_info(Type, Info) ->
    wrap_info2(Type, Info, []).

wrap_info2(alloc, [], Data) -> lists:reverse(Data);
wrap_info2(alloc, [{Name, BS, CS}|Alloc], Data) ->
    wrap_info2(alloc, Alloc, [{[{<<"name">>, Name}, {<<"bs">>, (BS div 1024)}, {<<"cs">>, (CS div 1024)}]} | Data]);

wrap_info2(scheduler, [], Data) -> lists:reverse(Data);
wrap_info2(scheduler, [{SchedulerId, ActiveTime, TotalTime}|Scheduler], Data) ->
    wrap_info2(scheduler, Scheduler, [{[{<<"schedulerid">>, SchedulerId},{<<"activetime">>, ActiveTime},{<<"totaltime">>, TotalTime}]} | Data]);

wrap_info2(info, [], Data) -> lists:reverse(Data);
wrap_info2(info, [{Name, Value}|Stat], Data) ->
    wrap_info2(info, Stat, [{[{<<"name">>, list_to_binary(Name)}, {<<"value">>, list_to_binary(observerweb_lib:to_str(Value))}]} | Data]).

get_acc_node() ->
    case get_data(acc_node) of
        [] -> node();
        [{_, Node}] -> Node
    end.

get_nodes() ->
    case get_data(nodes) of
        [] -> [{node(), erlang:get_cookie()}];
        [{_, Nodes1}] -> Nodes1
    end.

get_bare_nodes() ->
    get_bare_nodess(get_nodes(), []).

get_bare_nodess([], Data) -> lists:usort(Data);
get_bare_nodess([{Node, _Cookie} | Nodes], Data) ->
    get_bare_nodess(Nodes, [atom_to_binary(Node, latin1) | Data]).

get_data(Key) ->
    {ok, Dets} = dets:open_file("observer_table"),
    Data = dets:lookup(Dets, Key),
    dets:close(Dets),
    Data.

insert_Data(Key, Data) ->
    {ok, Dets} = dets:open_file("observer_table"),
    dets:insert(Dets, {Key, Data}),
    dets:close(Dets).
