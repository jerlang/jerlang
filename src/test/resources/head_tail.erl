-module(head_tail).
-export([h/1,t/1]).

h(X) -> hd(X).
t(X) -> tl(X).

