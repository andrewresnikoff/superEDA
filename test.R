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
  

  #assertCondition(actualCounts == test$counts)
  #assertCondition(actualPercent == test$percent)
  
  #Check percents
  #x
  #4  6  8 
  #34 22 44 
  
  #x
  #4  6  8 
  #11  7 14
  
  # Check if the barplot shows that the third bar is taller than other two,
  # and the second bar is the lowest.
  
 
}

testuniCont <- function(x, xName, main){
  x <-mtcars$hp
  test <- unicont(x, xName = "Gross horsepower", main = "UniCont Test")
  
  print("/n/n/n")
  print(test)
  
  actualnas=sum(is.na(x))
  print(actualnas)
  actualquantiles=quantiles(x, probs=c(0.25, 0.75, 0.5), na.rm=TRUE)
  print(actualquantiles)
  
  
  #assertCondition(actualnas == test$na)
  #assertCondition(actualquantiles == test$qts)
  
  #There should be no nas
  #
  
  #Descriptive Stats
  
  #check Q-Q Norm plot so that all the points are close to if not on the line
} 


testBiCatCat <- function(verbose = FALSE) {
    
    x <- mtcars$cyl
    y <- mtcars$am
    test <- biCatCat(x, y, xName = "Cylinders", yName = "AM", main = "BiCatCat Test")
    

    
    actualChiSq <- chisq.test(x,y)
    
    # check chi sq test 
    # X-squared = 8.7407, df = 2, p-value = 0.01265
    #assertCondition(actualChiSq$statistic == test$chiSq)
    #assertCondition(actualChiSq$parameter == test$parameter)    
    #assertCondition(actualChiSq$p.value == test$p)
    
    # check parts of counts table
    
    #   y
    #x    0  1
    # 4   3  8
    # 6   4  3
    # 8  12  2
    #assertCondition(test$counts[1,1] == 3)
    #assertCondition(test$counts[3,2] == 2)
    
    # check parts of percentages table
    #         y
    #x         0       1
    # 4  0.09375 0.25000
    # 6  0.12500 0.09375
    # 8  0.37500 0.06250
    #assertCondition(test$percents[1,2] == .25)
    #assertCondition(test$percents[2,1] == .125)
    
    # mosaic plot should have percentage of AM = 1 decreasing as cylinders increase.
    # Cylinders = 8 should be widest of the boxes, followed by 4, then 6
    
  
    
}

testBiCatCont <- function() {

    x <- mtcars$cyk
    y <- mtcars$hp
    
    test <- biCatCont(x, xName = "Cylinders",y = y, yName = "AM", main = "Test")
    
    
    # check descriptive stats
    assertCondition(all(test$descStats$'4' == summary(y[which(x == 4)])))
    assertCondition(all(test$descStats$'6' == summary(y[which(x == 6)])))
    assertCondition(all(test$descStats$'8' == summary(y[which(x == 8)])))
    
    # check robust stats
    rob4qts <- quantile(y[which(x == 4)], probs=c(0.25, 0.75, 0.5), na.rm = TRUE)
    rob4 = data.frame(Median=rob4qts[3], 
                     Q1=rob4qts[1], Q2=rob4qts[2],
                     IQR=diff(rob4qts[1:2]))
    
    rob6qts <- quantile(y[which(x == 6)], probs=c(0.25, 0.75, 0.5), na.rm = TRUE)
    rob6 = data.frame(Median=rob6qts[3], 
                      Q1=rob6qts[1], Q2=rob6qts[2],
                      IQR=diff(rob6qts[1:2]))
    
    rob8qts <- quantile(y[which(x == 8)], probs=c(0.25, 0.75, 0.5), na.rm = TRUE)
    rob8 = data.frame(Median=rob8qts[3], 
                      Q1=rob8qts[1], Q2=rob8qts[2],
                      IQR=diff(rob8qts[1:2]))
    
    #assertCondition(all(test$robustStats$'4' == rob4))
    #assertCondition(all(test$robustStats$'6' == rob6))
    #assertCondition(all(test$robustStats$'8' == rob8))
    
    # if categorical is numeric (ordered) - check for correlation
    
    if (test$isOrdered) {
        
        ordX <- as.numeric(x)
        assertCondition(test$cor == cor(ordX, y))
        assertCondition(test$cov == cov(ordX, y))
        
    }
    
    # side by side boxplots should show horsepower
    # ranges from 50-125 for 4cyl, 120-140 for 6cyl (with a boxplot outlier at 175)
    # and 155-340 for 8cyl. The boxes (middle 50% of values) should not be overlapping
    
    # scatter plot should show 3 colors representing each possible number of cylinders
    # 4 cylinders should be in black mostly towards bottom right (lower hp, higher index)
    # 6 cylinders should be in green mostly towards bottom left (a little higher than black, straight line at 100hp)
    # 8 cylinders in light blue above the rest, strech across entire plot
    
}

testBiContCat <- function(verbose = FALSE) {
    
    x <- mtcars$hp
    y <- mtcars$am

    test <- biContCat(x, y, xName = "Gross horsepower", yName = "Transmission", main = "BiContCat Test")

    # 1. Check if the first overlapping histogram plot exists, 
    # showing the distribution of "automatic" has a wider spead and a smaller mean.
    # 2. Check if the second conditional density plot exists, 
    # showing a bell-shaped shadow of "manual" starting from 0.25 and ending at 0.
    # Descriptive stats for x
    # Robust stats for x
    # percentage counts for y
    
    
}

testBiContCont <- function() {
    
    x = mtcars$mpg
    y = mtcars$disp
    
    test = biContCont(x, xName="Miles/(US) gallon", y, yName="Displacement", main="BiContCont Test")
    print("\n\n\n")
    print(test)
    
    actualCorrelation = cor(x,y)
    print(actualCorrelation)
    
    # Check correlation
    # check that plots look correct, if it should be pretty linear make sure the line is not flat or curved
    # Descriptive Stats
    # Robust Stats
    
    # assertCondition(actualCorrelation == testCorrelation)
    
  

}