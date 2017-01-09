mydata<-RISQ_data_for_past_year
#RISQ <- RISQ_test

##X1 = largest division of data
##X3 = 2nd level
##X4 = 3rd and smallest level
##X6 = Incident Time
##X7 = Incident Severity
##X9 = Entered Date


# Replaces blank incident dates with corresponding entered date
mydata$X6[is.na(mydata$X6)] <- as.character(mydata$X9[is.na(RISQ$X6)])

library(base)

# Formats dates into year - month
mth_yr <- as.Date(mydata$X6)
mydata$date <- format(mth_yr, "%y-%m") ##creates new column of dates given in year - month format
library(zoo)
mydata$date <- as.yearqtr(mydata$date, format = "%Y-%m") ##dates are changed from monthly to year - quarter format
table(mydata$date)

#removes data that was not within 12 months of current month
library(lubridate)
currentdate <- as.POSIXct(max(mydata$X6)) #sets current date to the latest incident date
startdate <- as.POSIXct(currentdate-months(11)) #sets start date 12 months before current date
start <- format(startdate, "%y-%m") #puts start date into year-month format
mydata<-mydata[!(mydata$date < start),] #removes rows with incident dates greater than 1 year old
table(mydata$date)

a <-function(g) {for (g in 1:nrow(level3)){
  level3n <- level3[g,]
  x <- qcc(level3n, data.name = paste(row.names.data.frame (level1n), row.names.data.frame(level2n),row.names.data.frame(level3n)), 
           type = 'c', plot = FALSE)
  if(x$data[NCOL(level3n)] > x$limits[2]) {
    plot(x)
    cat("\n", "\t", "\t")
    cat("Level 3 Point is above UCL in:", x$data.name)
    
  } else if (x$data[NCOL(GenType)] < x$limits[1]) {
    plot(x)
    cat("\n", "\t", "\t")
    cat("Level 3 Point is below LCL in:", x$data.name)
  } else {}
}
}


library(qcc)

level1 <- as.data.frame.matrix(table(mydata$X1,mydata$date)) 
for (n in 1:nrow(level1)){
  level1n <- level1[n,]
  x <- qcc(level1n, data.name = row.names.data.frame(level1n), type = 'c', plot = FALSE)
  if(x$data[NCOL(level1n)] > x$limits[2]) {# checks if final data point is greater than UCL
    plot(x)
    cat("\n")# creates a new line for each site
    cat("Level 1 Point is above UCL in:", x$data.name)# prints point is out of control and at which site
    assignlevel1 <- assign(row.names.data.frame(level1[n,]), subset(mydata, level1n == row.names.data.frame(level1[n,])))
    level2 <- as.data.frame.matrix(table(assignlevel1$X3,assignlevel1$date))  
    
    for (q in 1:nrow(level2)){
      level2n <- level2[q,]
      x <- qcc(level2n, data.name = paste(row.names.data.frame(level1n),row.names.data.frame(level2n)), type = 'c', plot = FALSE)
      if(x$data[NCOL(level2n)] > x$limits[2]) {
        plot(x)
        cat("\n", "\t")
        cat("level 2 Point is above UCL in:", x$data.name)
        
        assignlevel2 <- assign(rownames(level2[p,]),subset(assignlevel1,level2n == row.names.data.frame(level2[p,])))
        level3 <- as.data.frame.matrix(table(assignlevel2$X4, assignlevel2$date))
        a(g)
        
      } else if (x$data[NCOL(level2n)] < x$limits[1]) {
        plot(x)
        cat("\n","\t")
        cat("Level 2 Point is below LCL in:", x$data.name)
        
        assignlevel2 <- assign(rownames(level2[p,]),subset(assignlevel1,level2n == row.names.data.frame(level2[p,])))
        level3 <- as.data.frame.matrix(table(assignlevel2$X4, assignlevel2$date))
        a(g)

      } else {
      }
    } 
  }
  else if (x$data[NCOL(level1n)] < x$limits[1]) {# checks if final data point is less than LCL
    plot(x)
    cat("\n")# creates a new line for each site
    cat("Level 1 Point is below LCL in:", x$data.name)# prints point is out of control and at which site
    
    assignlevel1 <- assign(row.names.data.frame(level1[n,]), subset(mydata, level1n == row.names.data.frame(level1[n,])))
    level2 <- as.data.frame.matrix(table(assignlevel1$X3,assignlevel1$date))  
    for (p in 1:nrow(level2)){
      level2n <- level2[p,]
      x <- qcc(level2n, data.name = paste(row.names.data.frame(level1n), row.names.data.frame(level2n)), type = 'c', plot = FALSE)
      if(x$data[NCOL(level2n)] > x$limits[2]) {
        plot(x)
        cat("\n", "\t")
        cat("Level 2 Point is above UCL in:", x$data.name)
        
        assignlevel2 <- assign(rownames(level2[p,]),subset(assignlevel1,level2n == row.names.data.frame(level2[p,])))
        level3 <- as.data.frame.matrix(table(assignlevel2$X4, assignlevel2$date))
        a(g)
      } else if (x$data[NCOL(level2n)] < x$limits[1]) {
        plot(x)
        cat("\n", "\t")
        cat("Level 2 Point is below LCL in:", x$data.name)
        assignlevel2 <- assign(rownames(level2[p,]),subset(assignsite,level2n == row.names.data.frame(level2[p,])))
        level3 <- as.data.frame.matrix(table(assignlevel2$X4, assignlevel2$date))
        a(g)
      } else {
      }
      
    }
  } else {
    
  }
}


