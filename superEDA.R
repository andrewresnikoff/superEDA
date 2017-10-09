# Super EDA function
# by I. M. Buggy, Fall 2017

# Helper function for percent tables
# Taken from EDA1.pdf from 36601 by Professor Seltman
niceTable = function(tbl, digits=1, append=" %") {     
    if (!is.table(tbl)) stop("'tbl' is not a table")     
    if (!is.character(append) || length(append)!=1) {       
        stop("'append' must be a single string")     
    }    
    temp = formatC(tbl, digits=digits, format="f")     
    tbl[] = paste0(formatC(tbl, digits=digits, format="f",                            
    width = max(nchar(temp))), append)     
    return(tbl)   
} 

# Perform EDA on an combination of one or two variables
# EDA type depends on whether x and/or y are "factors"
# Requires: all functions below this on in this file
#
superEDA = function(x, y, main="") {
  if (!require("ggplot2")) {
    install.packages("ggplot2")
    library("ggplot2")
  }
  
  # Handle function arguments
  xName = dropDollar(deparse(substitute(x)))
  if (is.null(x)) stop(xName, " does not exist")
  xType = ifelse(is.factor(x) || is.logical(x) || is.character(x),
                 "categorical", "continuous")
  bivariate = ifelse(missing(y), FALSE, TRUE)
  if (bivariate) {
    yName = dropDollar(deparse(substitute(y)))
    if (is.null(y)) stop(yName, " does not exist")
    yType = ifelse(is.factor(y) || is.logical(y) || is.character(y),
                   "categorical", "continuous")
  }
  
  # Run the appropriate function
  if (!bivariate && xType=="categorical") {
    uniCat(x, xName=xName, main=main)
  } else if (!bivariate) {
    uniCont(x, xName=xName, main=main)
  } else if (xType=="categorical" && yType=="categorical") {
    biCatCat(x, xName=xName, y=y, yName=yName, main=main)
  } else if (xType=="categorical" && yType=="continuous") {
    biCatCont(x, xName=xName, y=y, yName=yName, main=main)
  } else if (xType=="continuous" && yType=="categorical") {
    biContCat(x, xName=xName, y=y, yName=yName, main=main)
  } else {
    biContCont(x, xName=xName, y=y, yName=yName, main=main)
  }
  invisible(NULL)
}

# Helper function to remove front characters up to "$"
dropDollar = function(txt) {
  return(gsub("^(.*[$])?(.*)", "\\2", txt))
}

# Univariate categorical EDA
# Make labelled output of table, proportion table, and barplot
uniCat = function(x, xName, main) {
  cat("Univariate EDA for categorical variable", xName, "\n")
    

  # Missing values
  na = sum(is.na(x))
  if (na == 0) {
    cat("There are no missing values.\n")
  } 
  else {
    cat("There are ", na, " missing values (", 
        round(100*na/length(x), digits=1), "%).\n", sep="")
  }
    
  cat("Counts:\n")
  cnts = table(x, exclude=NULL)
  names(dimnames(cnts)) = NULL
  print(cnts)
  # use helper function for percent table formatting
  cat("\nPercents:\n")
  pcts = niceTable(cnts)
  names(dimnames(pcts)) = NULL
  print(pcts)
  cat("\n")
  
  # Bar Plot
  
  # check if levels of x are numeric
  if (!all(grepl("^[[:digit:]]", levels(x)))) {
      
      # if they're not, sort the data by decreasing frequency
      x = factor(x, levels = names(sort(table(x), decreasing=TRUE)))
  }
  
  bp = ggplot(data=data.frame(x)) + 
      geom_bar(aes(x=x), stat="count", width=0.7, fill="steelblue") +
      labs(title=main, x=xName) + theme_minimal()
  print(bp)
  invisible(NULL)
  
}



# Univariate continuous EDA
# Make labelled output of basic statistics, and barplot
uniCont = function(x, xName, main) {
  cat("Univariate EDA for continuous variable", xName, "\n")
  
  # Missing values
  na = sum(is.na(x))
  if (na == 0) {
    cat("There are no missing values.\n")
  } else {
    cat("There are ", na, " missing values (", 
        round(100*na/length(x), digits=1), "%).\n", sep="")
  }
  
  # Standard stats
  cat("\nStandard stats:\n")
  rng = range(x, na.rm=TRUE)
  std = data.frame(Min=rng[1], 
                   Mean=signif(mean(x, na.rm=TRUE), digits=3),
                   StdDev=signif(sd(x, na.rm=TRUE), digits=3),
                   Max=rng[2])
  print(std, row.names=FALSE)
  
  # Robust stats
  # Order updated, IQR modified
  cat("\nRobust stats:\n")
  qts = quantile(x, probs=c(0.25, 0.75, 0.5), na.rm=TRUE)
  rob = data.frame(Q1=qts[1], Median=qts[3], 
                   Q3=qts[2], IQR=diff(qts[1:2]))
  print(rob, row.names=FALSE)
  
  # Plots
  # Implemented a Q-Q plot
  cat("\n")
  bpt = ggplot(data=data.frame(x)) + geom_boxplot(aes(x="", y=x)) +
    labs(title=main, x=xName) + theme_minimal()
  print(bpt)
  lenX = length(x)
  hst = ggplot(data=data.frame(x)) + 
    geom_histogram(aes(x=x), bins=min(10, max(100, lenX/10)), col="black",
                   fill="cyan") +
    labs(title=main, x=xName) + theme_minimal()
  print(hst)
  qqn = qqnorm(x, xlab=xName)
  print(qqn)
  invisible(NULL)
}


# Bivariate EDA for categorical IV and DV
# Make two-way table and mosaic plot
biCatCat = function(x, xName=xName, y=y, yName=yName, main=main) {
  cat("Bivariate EDA for categorical variables", xName, "and", yName, "\n")
  
  #cross tabulation
  cat("Counts:\n")
  cnts = table(x, y, exclude=NULL)
  names(dimnames(cnts)) = c(xName, yName)
  print(cnts)
  
  if (sum(is.na(x)) + sum(is.na(y)) > 0) {
    pctNA = round(100 * prop.table(table(!is.na(x), !is.na(y)), "%", margin=1),
                  digits=1)
    cat("\nPercent of", yName, "missing by level of", xName, "\n")
    dimnames(pctNA) = list(c("Observed", "Missing"), 
                           c("Observed", "Missing"))
    names(dimnames(pctNA)) = c(xName, yName)
    print(pctNA)
  }
  
  cat("\nPercent of each level of", yName, "for each level of", xName, "\n")
  cntsNoNA = table(x, y, useNA="no")
  pcts = round(100 * prop.table(cntsNoNA, margin=1), "%", digits=1)
  names(dimnames(pcts)) = c(xName, yName)
  print(pcts)
  
  # Manual chi-square test: for debugging exercise do not use chisq.texst()!
  rs = rowSums(cntsNoNA)
  rowMargins = rowSums(cntsNoNA)
  colMargins = colSums(cntsNoNA)
  
  # need to transpose expected values
  tExpected = outer(colMargins,rowMargins)/sum(cntsNoNA)
  expected = t(tExpected)
  
  OminusE = cntsNoNA - expected
  chiSq = sum(OminusE^2 / expected) 
  
  # misplaced -1, should be outside of ncol
  #df = (nrow(cntsNoNA) - 1) * (ncol(cntsNoNA - 1))
  df = (nrow(cntsNoNA) - 1) * (ncol(cntsNoNA)-1)
  
  # p is the probability we are >= chiSq value
  # p = pchisq(chiSq, df=df)
  p = pchisq(chiSq, df=df, lower.tail = FALSE)
  cat("\nChi-square = ", signif(chiSq, 3), ", df = ", df, ", p=", p, sep="")
  
  # Plot
  mosaicplot(cntsNoNA, xlab=xName, ylab=yName, main=main, col=1+1:ncol(cntsNoNA))
  invisible(NULL)
}

biCatCont = function(x, xName=xName, y=y, yName=yName, main=main){
  #Cross tabulation
  cat("Counts:\n")
  cnts = table(x, y, exclude=NULL)
  names(dimnames(cnts)) = c(xName, yName)
  print(cnts)
    
  #robust stats
  cat("\nRobust stats:\n")
  qts <- quantile(x, probs=c(0.25, 0.5, 0.75), na.rm = TRUE)
  rob = data.frame(Median=qts[2], 
                     Q1=qts[1], Q2=qts[3],
                     IQR=diff(qts[1:2]))
  print(rob, row.names=FALSE)
    
  #Plots
  bp <- ggplot(data=data.frame) + geom_boxplot(aes(x=x, y=y)) +
      labels(title=main, x=xName, y=yName) + theme_minimal()
  print(bp)
  scatterplot <- plot(x, y, xlab=xName, ylab=yName, main=main)
  print(scatterplot)
}

biContCat = function(x, xName=xName, y=y, yName=yName, main=main){
    
    # overlapping density plots
    df <- data.frame(x, y)
    
    ggplot(df, aes(x = x, fill = y)) + geom_density(aes(group = y), alpha = .25) + 
        labs(title=main, x=xName) + theme_minimal()
    
    # if binary
    if (nlevels(y) == 2){
        cdplot(x, y, xlab=xName, ylab=yName, main = main) 
    }
    
    
}

biContCont = function(x, xName=xName, y=y, yName=yName, main=main){

  
    
    df <- data.frame(x, y)
    
    # correlation
    cat("\nCorrelation between", xName, "and", yName, "=", cor(df)[2])
    
    # scatter plot with smoother curves
    sca = ggplot(df, aes(x=x, y=y)) + geom_point(aes(x=x, y=y)) +
            geom_smooth(method="loess", se=TRUE) +
            labs(title=main, x=xName, y=yName) + theme_minimal()
    print(sca)
    
    # qqplots
    qqnorm(x, xlab=xName); qqline(x)
    qqnorm(y, xlab=yName); qqline(y)
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

# Testing
if (exists("testingSuperEDA")) {
    
  data(mtcars)
  mtcars = mtcars  # make a copy
  mtcars = rbind(mtcars, data.frame(mpg=20, cyl=NA, disp=NA, hp=NA, drat=NA,
                                    wt=NA, qsec=NA, vs=NA, am=NA, gear=NA, carb=NA))
  mtcars$cyl = factor(mtcars$cyl)
  mtcars$gear = factor(mtcars$gear)
  mtcars$am = factor(mtcars$am, levels=0:1, labels=c("manual", "automatic"))
  
  # run all tests
  testUniCat(mtcars$cyl)
  testUniCont(mtcars$hp)
  testBiCatCat(mtcars$cyl, mtcars$am)
  testBiCatCont(mtcars$cyl, mtcars$hp)
  testBiContCat(mtcars$hp, mtcars$am)
  testBiContCont(mtcars$mpg, mtcars$disp)
  
}