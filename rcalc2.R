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

################################################################
# TESTING FUNCTIONS & APP FLOW
# Testing all functions, checking zero division error, negative numbers
# floating point numbers, floating point division
################################################################

# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 1
# Enter first number: 3
# Enter second number: 9
# [1] "3 + 9 = 12"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 2
# Enter first number: 5.7
# Enter second number: 3.7
# [1] "5.7 - 3.7 = 2"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 3
# Enter first number: 60
# Enter second number: 4
# [1] "60 * 4 = 240"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 4
# Enter first number: 5
# Enter second number: 4.5
# [1] "5 / 4.5 = 1.11111111111111"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 5
# Enter your value: 9
# [1] "Sqrt 9 = 3"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 6
# Enter your value: 34
# [1] "Exponent 34 = 583461742527455"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 7
# Enter your value: 180
# [1] "Cos 180 = -0.598460069057858"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 8
# Enter your value: 90
# [1] "Sin 90 = 0.893996663600558"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 9
# Enter your value: 180
# [1] "Tan 180 = 1.33869021035115"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 10
# [1] "Pi = 3.14159265358979"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 4
# Enter first number: -5
# Enter second number: 3.33
# [1] "-5 / 3.33 = -1.5015015015015"
# 
# 
# > source('~/MSE77/Rcalc/rcalc2.R')
# [1] "Please select a function: "
# [1] "1. Add"
# [1] "2. Subtract"
# [1] "3. Multiply"
# [1] "4. Divide"
# [1] "5. Sqrt"
# [1] "6. Exponent"
# [1] "7. Cos"
# [1] "8. Sin"
# [1] "9. Tan"
# [1] "10. Pi"
# Enter choice[1/2/3/4/5/6/7/8/9/10]: 4
# Enter first number: 6
# Enter second number: 0
# [1] "6 / 0 = Zero Division Error"





