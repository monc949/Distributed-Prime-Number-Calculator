%% ---------------------------------------------------------------------------
%% @author      : Ciaran Maye
%% @date        : 2023-03-31
%% @module      : prime
%% @description : Implements prime number functionalities for the distributed system
%%
%% This module is a part of the Distributed Prime Number Calculator project.
%% ---------------------------------------------------------------------------

-module(prime).
-export([nthPrime/1, nthPrime/2]).

% This function returns the Nth prime number
% It is a recursive function that checks if the number is prime
% If it is, it will decrement N and check the next number
% If it is not, it will check the next number
% It will stop when N is 0
% It will return the current number if N is 0
% It will return the next number if N is not 0
% It will check if the number is prime by calling the isPrime function
nthPrime(N) ->
    nthPrime(N, 2).

nthPrime(N, CurrentNum) ->
    case isPrime(CurrentNum) of
        true ->
            if
                N - 1 == 0 ->
                    CurrentNum;
                true ->
                    nthPrime(N - 1, CurrentNum + 1)
            end;
        _ ->
            nthPrime(N, CurrentNum + 1)
    end.

% This function checks if a number is prime
% It will return false if the number is 1
% It will return true if the number is 2
% It will call the isPrime function with the number and 2
% The isPrime function will check if the number is divisible by the x
% If it is, it will return false
% If it is not, it will check the next number
% It will stop when x is equal to the number
% It will return true if x is equal to the number
isPrime(1) ->
    false;
isPrime(2) ->
    true;
isPrime(Num) ->
    isPrime(Num, 2).

isPrime(Num, X) when Num =:= X ->
    true;
isPrime(Num, X) when Num rem X =:= 0 ->
    false;
isPrime(Num, X) ->
    isPrime(Num, X + 1).
