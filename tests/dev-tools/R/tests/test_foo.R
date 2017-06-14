source('foo.R')
igot <- hola()
igot
stopifnot(igot == 42)
