%% ---------------------------------------------------------------------------
%% @author      : Ciaran Maye
%% @date        : 2023-03-31
%% @module      : demo
%% @description : Demonstrates the functionality of the Distributed Prime Number Calculator project
%%
%% This module is a part of the Distributed Prime Number Calculator project.
%% ---------------------------------------------------------------------------

-module(demo).
-export([run/0]).

run() ->
    % Create nodes
    io:fwrite("Launching nodes~n", []),
    PID1 = node:launchNode(ciaran),
    timer:sleep(350),
    PID2 = node:launchNode(michal),
    timer:sleep(350),
    PID3 = node:launchNode(mohsin),
    timer:sleep(350),
    PID4 = node:launchNode(emma),
    timer:sleep(350),
    PID5 = node:launchNode(kerry),
    timer:sleep(350),
    PID6 = node:launchNode(sam),
    timer:sleep(350),
    PID7 = node:launchNode(daniel),
    timer:sleep(350),
    PID8 = node:launchNode(shane),
    timer:sleep(350),
    PID9 = node:launchNode(yinglong),
    timer:sleep(350),
    PID10 = node:launchNode(joe),
    timer:sleep(350),

    % Connect nodes
    io:fwrite("Connecting nodes~n", []),
    node:connectNode(ciaran, PID1, michal, PID2),
    timer:sleep(350),
    node:connectNode(michal, PID2, mohsin, PID3),
    timer:sleep(350),
    node:connectNode(mohsin, PID3, emma, PID4),
    timer:sleep(350),
    node:connectNode(emma, PID4, kerry, PID5),
    timer:sleep(350),
    node:connectNode(kerry, PID5, sam, PID6),
    timer:sleep(350),
    node:connectNode(sam, PID6, daniel, PID7),
    timer:sleep(350),
    node:connectNode(daniel, PID7, shane, PID8),
    timer:sleep(350),
    node:connectNode(shane, PID8, yinglong, PID9),
    timer:sleep(350),
    node:connectNode(yinglong, PID9, joe, PID10),
    timer:sleep(350),
    node:connectNode(joe, PID10, ciaran, PID1),
    timer:sleep(350),

    % Print routing tables
    io:fwrite("Routing Tables~n", []),
    print_routing_tables([ciaran, michal, mohsin, emma, kerry, sam, daniel, shane, yinglong, joe]),

    % Retrieve the 10th prime from ciaran to daniel
    io:fwrite("Ciaran asks for the 10th prime from Daniel~n", []),
    node:retrieveNthPrime(10, ciaran, daniel),
    timer:sleep(350),

    % Retrieve the 15th prime from michal to shane
    io:fwrite("Michal asks for the 15th prime from Shane~n", []),
    node:retrieveNthPrime(15, michal, shane),
    timer:sleep(350),

    % Retrieve the 20th prime from yinglong to joe
    io:fwrite("Yinglong asks for the 20th prime from Joe~n", []),
    node:retrieveNthPrime(20, yinglong, joe),

    ok.

print_routing_tables([H | T]) ->
    node:printTable(H),
    print_routing_tables(T);
print_routing_tables([]) ->
    ok.
