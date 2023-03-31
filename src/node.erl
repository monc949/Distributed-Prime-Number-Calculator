-module(node).
-export([
    start/1,
    launchNode/1,
    connectNode/4,
    printTable/1,
    retrieveNthPrime/3
]).

-import(prime, [nthPrime/1]).
-import(routing, [createNodeMessage/3, updateNeighbor/3, update_rt/5]).

% Launch a node with a nickname
launchNode(Nickname) ->
    Pid = spawn(fun() -> node:start(Nickname) end),
    io:fwrite("~p : Launch node as  ~p ~n", [Pid, Nickname]),
    Pid.

% Connect two nodes together
connectNode(NicknameOne, PidOne, NicknameTwo, PidTwo) ->
    NicknameOne ! {connect, NicknameTwo, PidTwo},
    NicknameTwo ! {connect, NicknameOne, PidOne}.

% Print the routing table of a node
printTable(PID) ->
    PID ! {routing_table}.

% Retrieve the Nth prime number
retrieveNthPrime(N, SendNick, DestNick) ->
    SendNick ! {computeNthPrime, N, DestNick, SendNick, 1}.
%
% Entry point of the node
%

% Start a node with a nickname
start(Nickname) ->
    timer:send_interval(5000, {send_updates}),
    register(Nickname, self()),
    run(Nickname, [], []).

% Run the node
% This node will handle the following messages:
% - {routing_table} : Print the routing table
% - {connect, Nickname, Pid} : Connect to a node
% - {computeNthPrime, N, DestNick, SenderNickname, Hops} : Compute the Nth prime number
% - {receiveAnswer, N, M, DestNick, SenderNickname, Hops} : Receive the answer of the Nth prime number
% - {send_updates} : Send the routing table to the neighbors
% - {route_updates, NeighborNickname, NeighborRT} : Update the routing table

run(NodeName, NeighborList, RT) ->
    receive
        {routing_table} ->
            io:fwrite("~p's Routing Table : ~p ~n", [NodeName, RT]),
            run(NodeName, NeighborList, RT);
        {connect, Nickname, Pid} ->
            io:fwrite("~p is connecting to ~p ~n", [NodeName, Nickname]),
            NewNeighborList = NeighborList ++ [{Nickname, Pid}],
            NewRoutingTable = createNodeMessage(NodeName, NewNeighborList, RT),
            updateNeighbor(NodeName, NewNeighborList, NewRoutingTable),
            run(NodeName, NewNeighborList, NewRoutingTable);
        {receiveAnswer, _N, _M, DestNick, _SenderNickname, Hops} when Hops > 15 ->
            io:fwrite("~p - Message to ~p is over 15 hops ~n", [
                NodeName, DestNick
            ]),
            run(NodeName, NeighborList, RT);
        {computeNthPrime, _N, DestNick, _SenderNickname, Hops} when Hops > 15 ->
            io:fwrite("~p : cannot send message to ~p  because is over 15 hops ~n", [
                NodeName, DestNick
            ]),
            run(NodeName, NeighborList, RT);
        {computeNthPrime, N, DestNick, SenderNickname, Hops} ->
            case DestNick of
                NodeName ->
                    io:fwrite("(~p) Computing prime ~p for ~p by ~p at ~p ~n", [
                        NodeName, N, SenderNickname, DestNick, Hops
                    ]),
                    NthPrime = nthPrime(N),
                    MessageReply =
                        {receiveAnswer, N, NthPrime, SenderNickname, DestNick, Hops + 1},
                    NewLocation = SenderNickname;
                _ ->
                    MessageReply =
                        {computeNthPrime, N, DestNick, SenderNickname, Hops + 1},
                    NewLocation = DestNick
            end,
            case sendMessage(NewLocation, MessageReply, NeighborList, RT) of
                {error, unreachable} ->
                    io:fwrite("~p is unreachable to ~p for ~p ~n", [
                        NodeName, NewLocation, MessageReply
                    ]);
                {ok, _} ->
                    io:fwrite("~p is sending ~p a message ~p ~n", [
                        NodeName, NewLocation, MessageReply
                    ])
            end,
            run(NodeName, NeighborList, RT);
        {receiveAnswer, N, M, DestNick, SenderNickname, Hops} ->
            case DestNick of
                NodeName ->
                    io:fwrite("From : ~p - (~p) Prime number is ~p ~n", [NodeName, N, M]);
                _ ->
                    Message = {receiveAnswer, N, M, DestNick, SenderNickname, Hops + 1},
                    sendMessage(DestNick, Message, NeighborList, RT)
            end,
            run(NodeName, NeighborList, RT);
        {send_updates} ->
            ModifiedRoutes = createNodeMessage(NodeName, NeighborList, RT),
            updateNeighbor(NodeName, NeighborList, ModifiedRoutes),
            run(NodeName, NeighborList, RT);
        {route_updates, NeighborNickname, NeighborRT} ->
            UpdatedRT = update_rt(NodeName, NeighborList, RT, NeighborNickname, NeighborRT),
            run(NodeName, NeighborList, UpdatedRT)
    end.

% Send a message to a node
% If the node is not a neighbor, send the message to the closest node
sendMessage(DestNick, Message, INL, RT) ->
    case lists:filter(fun({Name, _Pid}) -> Name == DestNick end, INL) of
        [] ->
            FilteredRoutes = lists:filter(
                fun({Destination, _Connection, _Distance}) ->
                    DestNick == Destination
                end,
                RT
            ),
            if
                length(FilteredRoutes) >= 1 ->
                    {DestNick, Connection, _Distance} = lists:nth(1, FilteredRoutes),
                    Connection ! Message,
                    {ok, forward};
                true ->
                    {error, unreachable}
            end;
        _ ->
            DestNick ! Message,
            {ok, neighbor}
    end.
