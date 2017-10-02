# test suite for superEDA

source("superEDA.R")
if (!require("tools")){
    library("tools")
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

testBiContCat <- function() {


}

testBiContCont <- function() {


}


