# creating functions

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

# take input from the user
print("Please select a function: ")
print("1. Add")
print("2. Subtract")
print("3. Multiply")
print("4. Divide")
print("5. Sqrt")
print("6. Exponent")
print("7. Cos")
print("8. Sin")
print("9. Tan")
print("10. Pi")

choice = as.integer(readline(prompt="Enter choice[1/2/3/4/5/6/7/8/9/10]: "))

if(choice < 5){
  num1 = as.numeric(readline(prompt="Enter first number: ")) 
  num2 = as.numeric(readline(prompt="Enter second number: "))
  
  operator <- switch(choice,"+","-","*","/","Sqrt","Exponent","Cos","Sin","Tan","Pi")
  result <- switch(choice, add(num1, num2), subtract(num1, num2), multiply(num1, num2), divide(num1, num2), sqrt(num1), exp(num1), cos(num1), sin(num1), tan(num1), pi)
  
  print(paste(num1, operator, num2, "=", result))
  
} else if(4 < choice & choice < 11){
  if(choice == 10){
    print(paste("Pi =", pi))
    
  } else {
    num1 = as.numeric(readline(prompt="Enter your value: "))
    
    operator <- switch(choice,"+","-","*","/","Sqrt","Exponent","Cos","Sin","Tan","Pi")
    result <- switch(choice, add(num1, num2), subtract(num1, num2), multiply(num1, num2), divide(num1, num2), sqrt(num1), exp(num1), cos(num1), sin(num1), tan(num1), pi)
    
    print(paste(operator, num1, "=", result))
    }
      
} else {
  print("Please enter a valid choice or 'quit()' to exit")
}







