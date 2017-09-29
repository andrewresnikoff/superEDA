# H. Seltman, October 2016
# Make a Gantt chart from a "plan" data frame

# PURPOSE: Produce a Gannet chart
#
# INPUT:
#  plan: a data frame with one row per task, and columns for task name,
#        start date, end date, and optionally priority and/or text to
#        appear within the task bars (see below for details)
#  format: the x-axis date format (according to ?strptime)
#  xlim: a vector of two dates to control the limits on the x-axis
#        (in any reasonable date format)
#  main: an optional plot title
#  barWidth: linewidth of the task bars (default=10)
#  priorityNames: optional character vector of names to show in the legend
#  priorityTitle: priority legend title
#  eventTimes: optional time(s) of special events to be marked with
#        a vertical line (in any reasonable time format)
#  eventNames: character string of labels (at top of plot) for marking
#        events (length must match 'eventTimes')
#  eventColor: color to use for event vertical lines and labels
#        the priority legend (can be a vector to color events differently)
#  labelWidth: width of task label beyond which wrapping occurs (in characters)
#  barTextColumn: the name of an optional column containing text to appear
#         inside the task bars (in that case the task bars are rendered
#         semi-transparent)
#  barAlpha: the fraction (0<barAlpha<1) of task bar transparency when task
#         bar text is used
#  colors: colors to use for the priorities.  If a numeric or character
#        vector, then the length must match the number of priorities.
#        If a function, then the function must take the number of 
#        priorities as an argument and return a vector of colors
#        as a string.  If priorities are not used, "blue" is the
#        default, but another color may be specified.
#  cex.date: character size for the dates
#  cex.tasks: character size for the task labels
#  cex.barText: character size for the bar text
#  taskLine=NULL: color of task full horizontal line (or TRUE for gray)
#  ...: other arguments to plot(), e.g., sub= or col.main=
#
# DETAILS:
#   The Gantt plot is a fairly typical Gantt plot (see reference).
#   If priorities are used, the task bars are shown in different colors.
#
#   The format of 'plan' is quite flexible.  For the column names,
#   the task name must contain the text "task", "name", or "label";
#   the start date must contain "start" or "begin"; the end date
#   must contain "end", "complet", or "finish"; priority must contain
#   "priorit".  The name of the task bar text column is set
#   explicitly with 'barTextColumn'.
#
#   Multiple 'plan' rows with the same task name are allowed and each
#   row produces a separate bar for that task.
#
#   NA's are allowed in 'plan', but the whole row is removed (except
#   if the NA is for task bar text).
#
#   The priorities may be integers or an ordered factor.  The range
#   of integers can be anything, but skipped integers get a color.
#   Character priorities are not allowed because the order of the
#   legend would be unknown.
#
#   Everywhere, dates may be factors, Date objects, or characters
#   in various common formats.  Note that Sys.Date() is a valid
#   Date object which could be used to mark the event "Today".
#
#   A legend is produced for the priority colors if priorities are
#   present.  The labels are numeric if the priorities are numeric
#   unless priorityNames are given.  If the priorities are an
#   ordered factor, then the legend names come from the factor.
#
#  SIDE EFFECT:
#    the plot is produced
#
#  RETURN VALUE:
#    the input 'plan' with date-like columns converted to Date's
#    (invisibly)
#
# REFERENCE: https://en.wikipedia.org/wiki/Gantt_chart
#
gantt = function(plan, format="%Y-%m-%d", xlim, main, 
                 barWidth=10, priorityNames, priorityTitle="Priorities",
                 eventTimes, eventNames, eventColor="#E322B0",
                 labelWidth=15,
                 barTextColumn, barAlpha=0.3, colors=topo.colors, 
                 cex.date=0.8, cex.tasks=0.8, cex.barText=0.6,
                 taskLine=NULL, ...) {
  
  ## Find needed columns ##
  if (!is.data.frame(plan)) stop("'plan' must be a data frame")
  #
  colNames = names(plan)
  idCol = grep("(task|name|label)", colNames, ignore.case=TRUE)
  if (length(idCol) == 0) 
    stop("one column of 'plan' must contain 'task', 'name', or 'label'")
  if (length(idCol) > 1) 
    stop("only one column of 'plan' must contain 'task', 'name', or 'label'")
  #
  startCol = grep("(start|begin)", colNames, ignore.case=TRUE)
  if (length(startCol) == 0) 
    stop("one column of 'plan' must contain 'start', or 'begin'")
  if (length(startCol) > 1) 
    stop("only one column of 'plan' must contain 'start', or 'begin'")
  #
  endCol = grep("(end|complet|finish)", names(plan), ignore.case=TRUE)
  if (length(endCol) == 0) 
    stop("one column of 'plan' must contain 'end', 'complet', or 'finish'")
  if (length(endCol) > 1) 
    stop("only one column of 'plan' must contain 'end', 'complet', or 'finish'")
  #  
  # Optional priority column
  priorityCol = grep("priorit", colNames, ignore.case=TRUE)
  if (length(priorityCol) == 0) {
    priorityCol = NULL
  } else {
    if (length(priorityCol) > 1) 
      stop("only one column of 'plan' must contain 'priorit'")
  }
  #  
  # Optional bar text column (goes inside task bars)
  if (missing(barTextColumn)) {
    barTextCol = NULL
  } else {
    barTextCol = match(barTextColumn, colNames)
    if (is.na(barTextCol)) {
      stop("'plan' does not have a column named ", barTextColumn)
    }
    if (!is.character(plan[[barTextCol]])) 
      plan[[barTextCol]] = as.character(plan[[barTextCol]])
  }
  
  # Disallow NA in key columns (exclude barText on purpose)
  usedCols = c(idCol, startCol, endCol, priorityCol)
  Bad = apply(plan[, usedCols], 1, function(x) any(is.na(x)))
  if (any(Bad)) {
    plan = plan[!Bad, ]
    warning(sum(Bad), " rows dropped due to NAs")
    if (nrow(plan) == 0) stop("all plan rows have NA")
  }
  
  ## Convert date columns to dates ##
  #
  # Function to force a vector to a Date vector.
  # 'id' is a column identifier used for an error message.
  # Allow already a Date, or formats such as 11/22/1964, 22-Nov-1964, 
  # or 1964-11-22.
  # Return a Date vector or cause an error.
  forceToDate = function(dates, id) {
    if (is(dates, "Date")) return(dates)
    if (is.factor(dates)) dates = as.character(dates)
    if (!is.character(dates)) stop("cannot convert ", id, " to dates")
    fmt = "%Y-%m-%d"
    parts = strsplit(dates[1], "(-|/)")[[1]]
    if (length(parts) == 3) {
      if (is.na(suppressWarnings(as.numeric(parts[2])))) {
        fmt = "%d-%b-%Y"
      } else {
        nc = nchar(parts)
        if (sum(nc==4)!=1 || sum(nc>0 & nc<=2) !=2 ) 
          stop("cannot convert ", id, " to dates")
        fmt = rep("%Y", 3)
        fmt[-match(4, nc)] = c("%m", "%d")
        sep = substring(dates[1], nc[1]+1, nc[1]+1)
        fmt = paste(fmt, collapse=sep)
      }
    }
    dates = try(as.Date(dates, format=fmt))
    if (is(dates, "try-error")) stop("cannot convert ", id, " to dates")
    return(dates)
  }
  #  
  plan[[startCol]] = forceToDate(plan[[startCol]], colNames[startCol])
  plan[[endCol]] = forceToDate(plan[[endCol]], colNames[endCol])
  
  ## Check for invalid data ##
  Bad = which(plan[[startCol]] > plan[[endCol]])
  if (length(Bad) > 0) 
    stop("bad date range in 'plan' on line(s) ", paste(Bad, collapse=", "))
  if (!is.finite(barAlpha) || length(barAlpha)!=1 || barAlpha<=0 || barAlpha>=1)
    stop("invalid 'barAlpha' (0<barAlpha<1)")
  
  ## Handle special case of priorities as an ordered factor
  if (!is.null(priorityCol)) {
    if (!is.numeric(plan[[priorityCol]]) && !is.ordered(plan[[priorityCol]]))
      stop(colNames[priorityCol], " should be numeric or an ordered factor")
    if (is.ordered(plan[[priorityCol]])) {
      if (missing(priorityNames)) priorityNames=levels(plan[[priorityCol]])
      plan[[priorityCol]] = as.numeric(plan[[priorityCol]])
    }
  }
  
  ## Set plot ranges and colors ##
  minX = min(plan[[startCol]])
  maxX = max(plan[[endCol]])
  if (!missing(xlim)) {
    if (length(xlim) != 2) stop("'xlim' must be of length 2")
    xlim = forceToDate(xlim)
    if (xlim[1] > minX) {
      xlim[1] = minX
      warning("'xlim[1]' reset to ", format(minX))
    }
    if (xlim[2] < maxX) {
      xlim[2] = maxX
      warning("'xlim[2]' reset to ", format(maxX))
    }
  } else {
    xlim = c(minX, maxX)
  }
  #
  # Get order for plotting tasks
  if (!is.character(plan[[idCol]])) plan[[idCol]] = as.character(plan[[idCol]])
  uIds = plan[[idCol]][!duplicated(plan[[idCol]])]
  nIds = length(uIds)
  #
  # Compute range of priorities
  if (is.null(priorityCol)) {
    priorityRange = 1
    priorityN = 1
  } else {
    priorityRange = min(plan[[priorityCol]]) : max(plan[[priorityCol]])
    priorityN = length(priorityRange)
  }
  #
  # Run colors() function to get colors
  if (is(colors, "function")) {
    colors = try(colors(priorityN))
    if (is(colors, "try-error")) {
      stop("cannot run ", substitute(deparse(colors)), "(", priorityN, ")")
    }
    if (!is.character(colors)) stop("invalid 'colors' function return value")
    if (length(colors) != priorityN) 
      stop("'colors' function return length incorrect")
  }
  #
  if (!any(class(colors) %in% c("integer", "numeric", "character")))
    stop("'colors' is not a valid color vector")
  if (length(colors)!=priorityN) 
    stop("number of colors must be ", priorityN, " to match priority range")
  #
  if (is.logical(taskLine) && taskLine[1]==TRUE) taskLine = "grey"
  
  ## make the plot ##
  oldpar = par(no.readonly=TRUE)
  on.exit(par(oldpar))
  taskLabelSpace = 6/15 * labelWidth
  par(mar = c(5, 6, taskLabelSpace, 2) + 0.1) # extra left space
  plot(NA, xlim=xlim, ylim=c(0.5, nIds+1), xlab="", ylab="", axes=FALSE, ...)
  title(xlab="Dates", line=2.2)
  box()
  pxlim = pretty(xlim)
  axis(1, pxlim, labels=format(pxlim, format=format), cex.axis=cex.date)
  #
  # y-axis task labels
  cxy = par("cxy")
  usr = par("usr")
  wrapped = lapply(uIds, strwrap, width=labelWidth)
  wrapped = sapply(wrapped, paste, collapse="\n")
  text(rep(usr[1] - 0.5*cxy[1], nIds), rev(1:nIds), wrapped,
       cex=cex.tasks, adj=c(1,0.35), xpd=NA)
  #
  # Render colors semi-transparent if bar text is used
  if (!is.null(barTextCol)) {
    colors = rbind(col2rgb(colors), alpha=round(barAlpha*256))
    colors = apply(colors, 2, 
                   function(x) rgb(red=x[1], green=x[2], blue=x[3],
                                   alpha=x[4], maxColorValue=256))
  }
  #
  # Task time intervals
  if (!is.null(taskLine)) abline(h=1:nIds, col=taskLine)
  for (i in 1:nIds) {
    Sel = plan[[idCol]] == uIds[i]
    col = if (is.null(priorityCol)) colors else colors[plan[Sel, priorityCol]]
    segments(plan[Sel, startCol], nIds-i+1, plan[Sel, endCol], nIds-i+1,
             col=col, lwd=barWidth, lend=1)
    #
    # Add bar text
    if (!is.null(barTextCol)) {
      text(apply(as.matrix(plan[Sel, c(startCol, endCol)]), 
                 1, 
                 function(x) mean(as.Date(x))),
           rep(nIds-i+1, sum(Sel)), plan[Sel, barTextCol], adj=c(0.5, 0.3), cex=cex.barText)
    }
  }
  #
  # Add legend
  if (!is.null(priorityCol)) {
    if (missing(priorityNames)) {
      legText = paste(1:priorityN)
    } else {
      if (length(priorityNames) != priorityN) stop("wrong number of priority names")
      legText = priorityNames
    }
    legend("topright", legText, lwd=5, col=colors, bty="n", cex=0.8, title=priorityTitle)
  }
  
  # Add in optional events
  if (!missing(eventTimes)) {
    nEvents = length(eventTimes)
    eventTimes = forceToDate(eventTimes)
    if (missing(eventNames)) stop("need 'eventNames'")
    if (length(eventNames) != nEvents) stop("event info length mismatch")
    abline(v=eventTimes, col=eventColor)
    text(eventTimes, rep(usr[4] + 0.5*cxy[2], nEvents), eventNames, 
         col=eventColor, adj=c(0.5, 0), xpd=NA, cex=cex.tasks)
  }
  
 
  if (!missing(main)) title(main)
  invisible(plan)
}

if (exists("testingGantt")) {
  myPlan = data.frame(
    tasks       = c("Review literature", "Mung data", "Stats analysis", "Mung data", "Write Report"),
    start.date  = as.Date(c("2010-08-24", "2010-10-01", "2010-11-01", "2010-12-30", "2011-02-14")),
    end.date    = as.Date(c("2010-10-31", "2010-12-14", "2011-02-28", "2011-01-14", "2011-04-30")),
    priority = c(1,2,2,1,3)
  )

  gantt(myPlan, colors=c("red","green","blue"), main="My Plan", format="%d-%b-%y",
        eventTimes=c("01-Oct-2010","31-Jan-2011"), 
        eventNames=c("Ext. Review #1", "Ext. Review #2"),
        priorityNames = c("Low", "Medium", "High"), sub="H. Seltman")

  gantt(myPlan[, -match("priority",names(myPlan))], main="My Plan", format="%d-%b-%y",
        eventTimes=c("01-Oct-2010","31-Jan-2011"), 
        eventNames=c("Ext. Review #1", "Ext. Review #2"),
        priorityNames = c("Low", "Medium", "High"), sub="H. Seltman")

  gantt(cbind(myPlan, person=c("Bob","Sue", "Mengru", NA, "Howard")),
        barTextColumn="person",
        colors=c("red","green","blue"), main="My Plan", format="%d-%b-%y",
        eventTimes=c("01-Oct-2010","31-Jan-2011"), 
        eventNames=c("Ext. Review #1", "Ext. Review #2"),
        priorityNames = c("Low", "Medium", "High"), sub="H. Seltman")
  
  gantt(cbind(myPlan[, -match("priority",names(myPlan))], 
              priorities=ordered(c(1,2,2,1,3), labels=c("low","med","high"))),
        taskLine="pink")
        
  
}
