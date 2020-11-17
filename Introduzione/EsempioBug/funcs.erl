worker()->
 receive
 X->
  receive
   Y->
   io:fwrite("~p~n",[X/Y])
   end
 end.
inter(W)->
 receive
 X->
  W ! X
 end.
sender(I1,I2)->
 I1 ! 0,
 I2 ! 2.
