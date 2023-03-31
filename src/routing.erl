%% ---------------------------------------------------------------------------
%% @author      : Ciaran Maye
%% @date        : 2023-03-31
%% @module      : routing
%% @description : Implements routing functionalities for the distributed system
%%
%% This module is a part of the Distributed Prime Number Calculator project.
%% ---------------------------------------------------------------------------

-module(routing).

-export([createNodeMessage/3, updateNeighbor/3, update_rt/5]).

% Create a message to send to each neighbor
% with the current node's routing table
% and the new route to the neighbor

% The new route to the neighbor is the current node
% with a distance of 1

% The new route to the neighbor is added to the
% current node's routing table
createNodeMessage(NodeName, NeighborList, RT) ->
    RoutesToNeighbors = lists:map(
        fun({NeighborNickname, _Pid}) -> {NeighborNickname, NodeName, 1} end, NeighborList
    ),
    RoutesToNeighbors ++ RT.

% Send the message to each neighbor
% with the current node's routing table

updateNeighbor(NodeName, [{_NeighborNickname, NeighborPid} | NeighborList], Routes) ->
    NeighborPid ! {route_updates, NodeName, Routes},
    updateNeighbor(NodeName, NeighborList, Routes);
updateNeighbor(_NodeName, [], _RT) ->
    ok.

% Update the routing table with the new routes
% from the neighbor

% If the new route is not in the routing table
% add it to the routing table

% If the new route is in the routing table
% and the distance is less than the current distance
% update the distance
update_rt(_NodeName, _NeighborList, RT, _NeighborNickname, []) ->
    RT;
update_rt(NodeName, NeighborList, RT, NeighborNickname, [
    {NodeName, _ConnectionNode, _Distance} | RemainingRoutes
]) ->
    update_rt(NodeName, NeighborList, RT, NeighborNickname, RemainingRoutes);
update_rt(NodeName, NeighborList, RT, NeighborNickname, [
    {DestinationNickname, _ConnectionNode, Distance} | RemainingRoutes
]) ->
    case lists:filter(fun(I) -> I == DestinationNickname end, NeighborList) of
        [] ->
            FilteredRoutes = lists:filter(
                fun({Destination, _Connection, _Distance}) ->
                    DestinationNickname == Destination
                end,
                RT
            ),
            if
                length(FilteredRoutes) >= 1 ->
                    ExistingRoute = lists:nth(1, FilteredRoutes),
                    {DestinationNickname, _Connection, OutDatedDistance} = ExistingRoute,
                    case OutDatedDistance > Distance + 1 of
                        false ->
                            update_rt(
                                NodeName, NeighborList, RT, NeighborNickname, RemainingRoutes
                            );
                        true ->
                            DeletedRoutingTable = lists:delete(ExistingRoute, RT),
                            UpdatedRoutingTable =
                                DeletedRoutingTable ++
                                    [{DestinationNickname, NeighborNickname, Distance + 1}],
                            update_rt(
                                NodeName,
                                NeighborList,
                                UpdatedRoutingTable,
                                NeighborNickname,
                                RemainingRoutes
                            )
                    end;
                true ->
                    UpdatedRoutingTable =
                        RT ++ [{DestinationNickname, NeighborNickname, Distance + 1}],
                    update_rt(
                        NodeName,
                        NeighborList,
                        UpdatedRoutingTable,
                        NeighborNickname,
                        RemainingRoutes
                    )
            end;
        _ ->
            update_rt(NodeName, NeighborList, RT, NeighborNickname, RemainingRoutes)
    end.
