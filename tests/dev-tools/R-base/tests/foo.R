# wrapper function for C -> foo()
dyn.load("foo.so")
hola <- function() {
  result <- .Call("hola")
  return(result)
}