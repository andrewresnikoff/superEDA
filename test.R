runAllTests <- function(mtcars) {
    
    # run all tests
    testUniCat(mtcars$cyl)
    testUniCont(mtcars$hp)
    testBiCatCat(mtcars$cyl, mtcars$am)
    testBiCatCont(mtcars$cyl, mtcars$hp)
    testBiContCat(mtcars$hp, mtcars$am)
    testBiContCont(mtcars$mpg, mtcars$disp)
    
}

testUniCat <- function(x) {
    
    
    # passes mtcars$cyl (as factor)
    superEDA(x, main="Motor Trend Cars")
    
    # Non-graphical checks
    # 1. Check percents
    #    x
    #    4  6  8  <NA>
    #    34 22 44  1
    
    # 2. Check counts
    #    x
    #    4  6  8  <NA>
    #    11  7 14  3
    
    # Graphical checks
    # 1. Check if the barplot shows that the third bar is taller than other three,
    #    and the fourth bar is the lowest.
    
    
    #assertCondition(actualCounts == test$counts)
    #assertCondition(actualPercent == test$percent)
    
    
}
testUniCont <- function(x) {
    
    # passes mtcars$hp
    superEDA(x, main = "Motor Trend Cars")
    
    # Non-graphical checks
    # 1. There should be 1 missing value
    # 2. Check descriptive stats
    #    min=52.0, Q1=96.5, median=123.0, mean=146.7, Q3=180.0, max=335.0
    #    assertCondition(actualNa == test$na)
    #    assertCondition(actualQuantiles == test$qts)
    
    # Graphical checks  
    # 1. Check boxplot so that middle 50% of values fall between 100 and 180 and there
    #    is a boxplot outlier above 300
    # 2. Check histogram so that the bars show no bell curve shape
    # 3. Check Q-Q Norm plots so that all the points are close to if not on the line
    
}
testBiCatCat <- function(x,y) {
    
    # passes mtcars$cyl (as factor), mtcars$am (as factor)
    superEDA(x, y, main = "Motor Trend Cars")
    
    # Non-graphical checks
    
    # 1. Check parts of counts table
    #          y
    #    x   manual  automatic <NA>
    #    4       3           8    0
    #    6       4           3    0
    #    8      12           2    0
    #    <NA>    0           0    1
    
    # 2. Check parts of percentages table
    #                  y
    #    x  manual   automatic 
    #    4   27.3      72.7
    #    6   57.1      42.9
    #    8   85.7      14.3 
    
    # 3. Check chi sq test 
    #    X-squared = 8.7407, df = 2, p-value = 0.01265
    #    assertCondition(actualChiSq$statistic == test$chiSq)
    #    assertCondition(actualChiSq$parameter == test$parameter)    
    #    assertCondition(actualChiSq$p.value == test$p)
    
    #    assertCondition(test$percents[1,2] == .25)
    #    assertCondition(test$percents[2,1] == .125)
    
    # Graphical checks
    # 1. mosaic plot should have percentage of AM = 1 decreasing as cylinders increase.
    #    Cylinders = 8 should be widest of the boxes, followed by 4, then 6
    
    
}
testBiCatCont <- function(x,y) {
    
    # passes mtcars$cyl (as factor), mtcars$hp
    superEDA(x,y, main = "Motor Trend Cars")
    
    # Non-graphical checks
    # 1. Check descriptive stats
    #    assertCondition(all(test$descStats$'4' == summary(y[which(x == 4)])))
    #    assertCondition(all(test$descStats$'6' == summary(y[which(x == 6)])))
    #    assertCondition(all(test$descStats$'8' == summary(y[which(x == 8)])))
    
    # Graphical checks
    # 1. Side-by-side boxplots should show horsepower
    #    ranges from 50-125 for 4cyl, 120-140 for 6cyl (with a boxplot outlier at 175)
    #    and 155-340 for 8cyl. The boxes (middle 50% of values) should not be overlapping.
    # 2. Scatter plot should show 3 colors representing each possible number of cylinders
    #    4 cylinders should be in black mostly towards bottom right (lower hp, higher index)
    #    6 cylinders should be in green mostly towards bottom left (a little higher than black, straight line at 100hp)
    #    8 cylinders in light blue above the rest, strech across entire plot.
    
}
testBiContCat <- function(x,y) {
    
    # passes mtcars$hp, mtcars$am
    superEDA(x,y, main = "Motor Trend Cars")
    
    # Non-graphical checks
    # 1. Descriptive stats for x
    #    min=52.0, Q1=96.5, median=123.0, Q3=180.0, max=335.0
    # 2. Percentage counts for y
    #            y
    #    0       1       <NA> 
    #    0.59375 0.40625 0.00000 
    
    # Graphical checks
    # 1. Check if the first overlapping histogram plot exists, 
    #    showing the distribution of "automatic" has a wider spead and a smaller mean.
    # 2. Check if the second conditional density plot exists, 
    #    showing a bell-shaped shadow of "manual" starting from 0.25 and ending at 0.
    
}
testBiContCont <- function(x,y) {
    
    # passes mtcars$mpg, mtcars$disp
    superEDA(x,y, main = "Motor Trend Cars")
    
    # Non-graphical checks
    # 1. check correlation
    #    cor(x, y) = -0.8475514
    #    assertCondition(actualCorrelation == testCorrelation)
    
    # Graphical checks
    # 1. check if the scattor plot shows that there seems to be a non-linear negative relationship.
    # 2. check if the Q-Q plot of x and y looks approximately normal.
}