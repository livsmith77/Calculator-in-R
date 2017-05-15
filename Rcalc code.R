# adapting python calc functions for R

add <- function(x, y) {
  return(x + y)
}

print(add(3,4))

subtract <- function(x, y) {
  return(x - y)
}

print(subtract(3,4))

multiply <- function(x, y) {
  return(x * y)
}

print(multiply(3,4))

divide <- function(x, y) {
  if(y == 0){
    return ("Zero Division Error")
  } else {
    return(x / y)
  }
}
