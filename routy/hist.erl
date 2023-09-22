-module(hist).

-export([new/1, update/3]).

new(Name) ->
    [{Name, -1}].

update(Node, N, History) -> 
    NewNode = lists:keyfind(Node, 1, History),
    case NewNode of
        {_, Old} ->
            % if we have a higher number we should update
            if N > Old -> 
                {new, lists:keyreplace(Node, 1, History, {Node, N})};
	       true ->
		    old
	    end;
    % create a new one if we don't have it
    false  ->
	    {new, [{Node, N}|History]}
    end.