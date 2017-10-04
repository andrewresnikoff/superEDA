# test suite for superEDA
source(superEDA.R)

testBiCatCont <- function() {

}

testBiContCat <- function() {


}

testBiContCont <- function() {
  x = cars$dist
  y = cars$speed
  test = biContCont(x, xName="Cylinders", y, yName="AM", main="BiContCont Test")
  actual.correlation = cor(x,y)
  print(test)
  print(actual.correlation)
  assertCondition(actual.correlation = test.correlation)
  #check that plots look correct, if it should be pretty linear make sure the line is not flat or curved
  
  if (verbos){
    print("All test passed")
  } 
  
}


