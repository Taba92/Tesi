main()->
 W=spawn(funcs,worker,[]),
 I1=spawn(funcs,inter,[W]),
 I2=spawn(funcs,inter,[W]),
 spawn(funcs,sender,[I1,I2]).
