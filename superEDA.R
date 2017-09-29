# Super EDA function
# by I. M. Buggy, Fall 2017

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
  cat("Counts:\n")
  cnts = table(x, exclude=NULL)
  names(dimnames(cnts)) = NULL
  print(cnts)
  cat("\nPercents:\n")
  pcts = round(100 * prop.table(cnts))
  names(dimnames(pcts)) = NULL
  print(pcts)
  cat("\n")
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
  cat("\nRobust stats:\n")
  qts = quantile(x, probs=c(0.25, 0.75, 0.5), na.rm=TRUE)
  rob = data.frame(Median=qts[3], 
                   Q1=qts[1], Q2=qts[2],
                   IQR=diff(qts[1:2]))
  print(rob, row.names=FALSE)
  
  # Plots
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
  invisible(NULL)
}



# Bivariate EDA for categorical IV and DV
# Make two-way table and mosaic plot
biCatCat = function(x, xName=xName, y=y, yName=yName, main=main) {
  cat("Bivariate EDA for categorical variables", xName, "and", yName, "\n")
  
  cat("Counts:\n")
  cnts = table(x, y, exclude=NULL)
  names(dimnames(cnts)) = c(xName, yName)
  print(cnts)

  if (sum(is.na(x)) + sum(is.na(y)) > 0) {
    pctNA = round(100 * prop.table(table(!is.na(x), !is.na(y)), margin=1),
                  digits=1)
    cat("\nPercent of", yName, "missing by level of", xName, "\n")
    dimnames(pctNA) = list(c("Observed", "Missing"), 
                           c("Observed", "Missing"))
    names(dimnames(pctNA)) = c(xName, yName)
    print(pctNA)
  }
  
  cat("\nPercent of each level of", yName, "for each level of", xName, "\n")
  cntsNoNA = table(x, y, useNA="no")
  pcts = round(100 * prop.table(cntsNoNA, margin=1), digits=1)
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


# Testing
if (exists("testingSuperEDA")) {
  data(mtcars)
  mtcars = mtcars  # make a copy
  mtcars = rbind(mtcars, data.frame(mpg=20, cyl=NA, disp=NA, hp=NA, drat=NA,
                                    wt=NA, qsec=NA, vs=NA, am=NA, gear=NA, carb=NA))
  mtcars$cyl = factor(mtcars$cyl)
  mtcars$gear = factor(mtcars$gear)
  mtcars$am = factor(mtcars$am, levels=0:1, labels=c("manual", "automatic"))
  
  superEDA(mtcars$cyl, main="Motor Trend Cars")
  superEDA(mtcars$mpg, main="Motor Trend Cars")
  superEDA(mtcars$cyl, mtcars$am, main="Motor Trend Cars")
  
}

