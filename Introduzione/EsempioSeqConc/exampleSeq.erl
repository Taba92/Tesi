main()->
  A=3,
  case A of
  	3->
      io:fwrite("A"),
      io:fwrite("B");
    _->
      io:fwrite("C")
  end.


