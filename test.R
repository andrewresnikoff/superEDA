# test suite for superEDA

source("superEDA.R")
if (!require("tools")){
    library("tools")
}

testUniCat <- function(verbose = FALSE) {
  x = mtcars$cyl
  test = uniCat(x, xName = "Cylinders", main = "Test")
  print("\n\n\n")
  print(test)
  
  actualCounts = table(x, exclude = NULL)
  actualPercent = round(100 * prop.table(actualCounts))
  print(actualCounts)
  print(actualPercent)
  
  assertCondition(actualCounts = test$counts)
  assertCondition(actualPercent = test$percent)
  # Check if the barplot shows that the third bar is taller than other two,
  # and the second bar is the lowest.
  
  if (verbose) {
    print("All tests passed!")
  }
  
}

testBiCatCat <- function(verbose = FALSE) {
    
    x <- mtcars$cyl
    y <- mtcars$am
    test <- biCatCat(x, y, xName = "Cylinders", yName = "AM", main = "Test")
    
    print("\n\n\n")
    print(test)
    
    actualChiSq <- chisq.test(x,y)
    
    print(actualChiSq$statistic)
    print(test$chiSq)
    
    assertCondition(actualChiSq$statistic == test$chiSq)
    assertCondition(actualChiSq$p.value == test$p)
    
    
    if (verbose) {
        
        print("All tests passed!")
    }
    
}

testBiCatCont <- function() {

}

testBiContCat <- function(verbose = FALSE) {
    
    x <- mtcars$hp
    y <- mtcars$am
    test <- biContCat(x, y, xName = "Gross horsepower", yName = "Transmission", main = "Test")
    
    print("\n\n\n")
    print(test)

    # 1. Check if the first overlapping histogram plot exists, 
    # showing the distribution of "automatic" has a wider spead and a smaller mean.
    # 2. Check if the second conditional density plot exists, 
    # showing a bell-shaped shadow of "manual" starting from 0.25 and ending at 0.
    
    if (verbose) {
      print("All tests passed!")
    }
}

testBiContCont <- function() {


}