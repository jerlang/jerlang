%% Erlang calling Java. The java module converts the arguments to the
%% parameter types of the static method and converts the result back.
-module(service).
-export([greet/1, scale/2]).

greet(Name) ->
    java:call_static("Service", "greet", [Name]).

scale(Value, Factor) ->
    java:call_static("Service", "scale", [Value, Factor]).
