-module(listcomprehensions).
-export([squared_int/1, intersect/2, symmetric_difference/2]).

squared_int(List) -> [Elem * Elem || Elem <- List, is_integer(Elem)].

intersect(List1, List2) -> [Elem || Elem <- List1, lists:member(Elem, List2)].

symmetric_difference(List1, List2) -> lists:append([Elem || Elem <- List1, not lists:member(Elem, List2)], [Elem || Elem <- List2, not lists:member(Elem, List1)]).