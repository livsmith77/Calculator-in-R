# adapting python calc functions for R

add <- function(x, y) {
  return(x + y)
}

subtract <- function(x, y) {
  return(x - y)
}

multiply <- function(x, y) {
  return(x * y)
}

divide <- function(x, y) {
  if(y == 0){
    return ("Zero Division Error")
  } else {
    return(x / y)
  }
}


squareroot <-function(x) {
  return (sqrt(x))
}

exponent <-function(x,y) {
  return (x**y)
}


trig_cos <- function(x) {
  return(cos(x*pi/180))
}

trig_sin <- function(x) {
  return(sin(x*pi/180))
}

trig_tan <- function(x) {
  if(x %% 180 ==0){
    return (0)
  } else if(x%% 90 ==0){
    return ("Zero Division Error")
  } else {
    return (tan(x*pi/180))
  }
}

get_pi <- pi
