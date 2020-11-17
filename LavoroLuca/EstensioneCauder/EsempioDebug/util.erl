-module(util).
-export([search/2,remove/2,get_random_el/1]).

get_random_el(List)->
	Index=rand:uniform(length(List)),
	lists:nth(Index,List).

search(_,[])->not_exist;
search(Worker,[{Key,Client}|T])->
	case Worker of
		Key->Client;
		_->search(Worker,T)
	end.

remove(Worker,Map)->
	remove(Worker,Map,[]).
remove(_,[],Acc)->Acc;
remove(Worker,[{Key,Client}|T],Acc)->
	case Worker of
		Key->remove(Worker,T,Acc);
		_->remove(Worker,T,[{Key,Client}|Acc])
	end.

