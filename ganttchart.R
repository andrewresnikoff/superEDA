myPlan = data.frame( 
  tasks=c("improve code", "testsuite", "documentation", "Write Report"),   
  start.date=as.Date(c("30-Sept-2017", "02-Oct-2017",
                       "06- Oct-2017", "08-Oct-2017")),   
  end.date=as.Date(c("31-Sept-2017", "03-Oct-2017",
                     "07-Oct-2017", "09-Oct-2017")),   
  priority = c(1, 2,   2,   1))  

gantt.chart(cbind(myPlan, person=c("Andrew", "Letti", "Amanda")),   
  barTextColumn="person",   
  colors=c("red", "green", "blue"), main="My Plan", format="%d-%b-  %y",   
  eventTimes=c("02-Oct-2017", "06-Oct-2017", "09-Oct-2017"),   
  eventNames=c("Test Suite Review", "Combine", "Documentation Review"),   
  priorityNames = c("Low", "Medium", "High"), sub="H. Seltman") 

