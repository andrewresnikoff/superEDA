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
  # Use helper function for percent table formatting
  cat("\nPercents:\n")
  pcts = niceTable(cnts)
  names(dimnames(pcts)) = NULL
  print(pcts)
  cat("\n")
  
  
  # Check if levels of x are numeric
  if (!all(grepl("^[[:digit:]]", levels(x)))) {
      
      # If they're not, sort the data by decreasing frequency
      x = factor(x, levels = names(sort(table(x), decreasing=TRUE)))
  }
  
  # Bar Plot
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
  
  # Boxplot
  cat("\n")
  bpt = ggplot(data=data.frame(x)) + geom_boxplot(aes(x="", y=x)) +
    labs(title=main, x=xName) + theme_minimal()
  print(bpt)
  lenX = length(x)
  
  # Histogram
  hst = ggplot(data=data.frame(x)) + 
    geom_histogram(aes(x=x), bins=min(10, max(100, lenX/10)), col="black",
                   fill="cyan") +
    labs(title=main, x=xName) + theme_minimal()
  print(hst)
  
  # Q-Q norm
  qqn = qqnorm(x, xlab=xName); qqline(x)
  print(qqn)
  invisible(NULL)
}


# Bivariate EDA for categorical IV and DV
# Make two-way table and mosaic plot
biCatCat = function(x, xName=xName, y=y, yName=yName, main=main) {
  cat("Bivariate EDA for categorical variables", xName, "and", yName, "\n")
  
  # Cross Tabulation
  cat("Counts:\n")
  cnts = table(x, y, exclude=NULL)
  names(dimnames(cnts)) = c(xName, yName)
  print(cnts)
  
  # Missing Values
  if (sum(is.na(x)) + sum(is.na(y)) > 0) {
    pctNA = niceTable(table(!is.na(x), !is.na(y)))
    cat("\nPercent of", yName, "missing by level of", xName, "\n")
    dimnames(pctNA) = list(c("Observed", "Missing"), 
                           c("Observed", "Missing"))
    names(dimnames(pctNA)) = c(xName, yName)
    print(pctNA)
  }
  
  # Counts and Percents
  cat("\nPercent of each level of", yName, "for each level of", xName, "\n")
  cntsNoNA = table(x, y, useNA="no")
  pcts = niceTable(cntsNoNA)
  names(dimnames(pcts)) = c(xName, yName)
  print(pcts)
  
  # Manual chi-square test
  rs = rowSums(cntsNoNA)
  rowMargins = rowSums(cntsNoNA)
  colMargins = colSums(cntsNoNA)
  
  tExpected = outer(colMargins,rowMargins)/sum(cntsNoNA)
  expected = t(tExpected)
  
  OminusE = cntsNoNA - expected
  chiSq = sum(OminusE^2 / expected) 

  df = (nrow(cntsNoNA) - 1) * (ncol(cntsNoNA)-1)

  p = pchisq(chiSq, df=df, lower.tail = FALSE)
  cat("\nChi-square = ", signif(chiSq, 3), ", df = ", df, ", p=", p, sep="")
  
  # Plot
  mosaicplot(cntsNoNA, xlab=xName, ylab=yName, main=main, col=1+1:ncol(cntsNoNA))
  invisible(NULL)
}

biCatCont = function(x, xName=xName, y=y, yName=yName, main=main){
  # Cross tabulation
  cat("Counts:\n")
  cnts = table(x, y, exclude=NULL)
  names(dimnames(cnts)) = c(xName, yName)
  print(cnts)
    
  # Robust stats
  cat("\nRobust stats:\n")
  qts <- quantile(y, probs=c(0.25, 0.75, 0.5), na.rm = TRUE)
  rob = data.frame(Q1=qts[1], Median=qts[3], 
                   Q3=qts[2], IQR=diff(qts[1:2]))
  print(rob, row.names=FALSE)
    
  # Plots
  
  # Box Plot
  plot(x, y, xlab=xName, ylab=yName, main=main)
}

biContCat = function(x, xName=xName, y=y, yName=yName, main=main){
    
    df <- data.frame(x, y)
    df <- na.omit(df)
    
    # Bin the continuous variable by quantile
    bins <- split(df, cut(df$x, quantile(df$x)))
    bins[[1]]$x = "Min - Q1"
    bins[[2]]$x = "Q1 - Median"
    bins[[3]]$x = "Median - Q3"
    bins[[4]]$x = "Q3 - Max"
    
    cat("\nBinned Counts:\n")
    # Counts of response per bin
    cnts <- lapply(bins, table)
    print(cnts)
    
    cat("\nBinned Percents:\n")
    # Percents of response per bin
    pcts <- lapply(cnts, {function (x) round(prop.table(x)*100, 2) } )
    print(pcts)
    
    # Plot overlapping density plots
    ggplot(df, aes(x = x, fill = y)) + geom_density(aes(group = y), alpha = .25) + 
        labs(title=main, x=xName) + theme_minimal()
    
    # If the response is binary
    if (nlevels(df$y) == 2){
        
        # Plot conditional density plot
        cdplot(df$x, df$y, xlab=xName, ylab=yName, main = main) 
    }
    
    
}

biContCont = function(x, xName=xName, y=y, yName=yName, main=main){
    
    df <- data.frame(x, y)
    
    # Calculate correlation
    cat("\nCorrelation between", xName, "and", yName, "=", cor(df)[2])
    
    # Scatter plot with smoother curves
    sca = ggplot(df, aes(x=x, y=y)) + geom_point(aes(x=x, y=y)) +
            geom_smooth(method="loess", se=TRUE) +
            labs(title=main, x=xName, y=yName) + theme_minimal()
    print(sca)
    
    # Q-Q plots
    qqnorm(x, xlab=xName); qqline(x)
    qqnorm(y, xlab=yName); qqline(y)
}



# Testing
if (exists("testingSuperEDA")) {

  source("test.R")
  data(mtcars)
  mtcars = mtcars  # make a copy
  mtcars = rbind(mtcars, data.frame(mpg=20, cyl=NA, disp=NA, hp=NA, drat=NA,
                                    wt=NA, qsec=NA, vs=NA, am=NA, gear=NA, carb=NA))
  mtcars$cyl = factor(mtcars$cyl)
  mtcars$gear = factor(mtcars$gear)
  mtcars$am = factor(mtcars$am, levels=0:1, labels=c("manual", "automatic"))
  
  # Run all tests
  runAllTests(mtcars)
  
}

