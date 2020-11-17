-module(clientServer).
-export([main/0,server/1,client/1,worker/3]).

main()->
  Pid=spawn(?MODULE,server,[[]]),
  spawn(?MODULE,client,[Pid]),
  spawn(?MODULE,client,[Pid]).

client(Server)->
  %%possibili input per la richiesta al server
  Terms=[49,ciao,78,98,0,"term",1000,4.5,74569,259.326,1],
  Inputs={util:get_random_el(Terms),util:get_random_el(Terms)},
  Server !{calculate,self(),Inputs},
  receive
    {ret,badarg}->client(Server);
    {ret,Val}->
	  Val
  end.

server(Map)->
  receive 
    {calculate,Client,{First,Second}}->
      Worker=spawn_link(?MODULE,worker,[Client,First,Second]),
	  server([{Worker,Client}|Map]);
	{'EXIT',Worker,normal}->
	  server(util:remove(Worker,Map));
	{'EXIT',Worker,_}->
	  Client=util:search(Worker,Map),
	  Client ! {ret,badarg},
	  server(util:remove(Worker,Map))
  end.

worker(Client,First,Second)->
  Val=First/Second,
  Client ! {ret,Val}.