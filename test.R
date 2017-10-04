# test suite for superEDA

source("superEDA.R")
if (!require("tools")){
    library("tools")
}



testBiCatCat <- function(verbose = FALSE) {
    
    
    x <- mtcars$cyl
    y <- mtcars$am
    test <- biCatCat(x, xName = "Cylinders",y = y, yName = "AM", main = "Test")
    

    
    actualChiSq <- chisq.test(x,y)
    
    # check chi sq test
    assertCondition(actualChiSq$statistic == test$chiSq)
    assertCondition(actualChiSq$parameter == test$parameter)    
    assertCondition(actualChiSq$p.value == test$p)
    
    # check parts of counts table
    assertCondition(test$counts[1,1] == 3)
    assertCondition(test$counts[3,2] == 2)
    
    # check parts of percentages table
    assertCondition(test$percents[1,2] == .25)
    assertCondition(test$percents[2,1] == .125)
    
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
    
    assertCondition(all(test$robustStats$'4' == rob4))
    assertCondition(all(test$robustStats$'6' == rob6))
    assertCondition(all(test$robustStats$'8' == rob8))
    
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

testBiContCat <- function() {


}

testBiContCont <- function() {


}


