testuniCont <- function(x, xName, main)
  x <-mtcars$hp
  test <- unicont(x, xName = "Gross horsepower", main = "UniCont Test")
  
  print("/n/n/n")
  print(test)
  
  actualnas=sum(is.na(x)
                if (actualnas == 0) {
                  cat("There are no missing values.\n")
                } else {
                  cat("There are ", na, " missing values (", 
                      round(100*na/length(x), digits=1), "%).\n", sep="")
                })
  print(actualnas)
  actualquantiles=quantiles(x, probs=c(0.25, 0.75, 0.5), na.rm=TRUE)
  print(actualquantiles)
  
  assertCondition(actualnas == test$na)
  assertCondition(actualquantiles == test$qts)
  
  #check Q-Q Norm plot so that all the points are close to if not on the line
  
  
  if (verbos){
    print("All UniConttests passed!")
  }