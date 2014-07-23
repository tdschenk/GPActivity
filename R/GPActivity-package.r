#' GPActivity
#' 
#' This package contains functions that accept a 60sec epoch .csv file,
#' and generates summaries of sleep time/efficiency, non-wear time, and MET/min.
#'
#' @name GPActivity
#' @docType package
#' @import plyr

# Process the epoch file into a data frame and perform calculations
#' @export
gpa.process <- function(epoch.file, frequency) {
  
  ## Read in 60sec epoch data
  data <- read.table(epoch.file, 
                     skip = 100, sep = ',')
  data <- rename(data, c("V1" = "dateTime", "V2" = "x.mean", "V3" = "y.mean",
                         "V4" = "z.mean", "V5" = "lux", "V6" = "button",
                         "V7" = "temp", "V8" = "svm", "V9" = "x.sd",
                         "V10" = "y.sd", "V11" = "z.sd", "V12" = "light"))
  
  ## Activity Column
  i <- 2
  data$activity[1] <- 0
  while (i < length(data[,1])) {
    data$activity[i] <- sqrt((abs(data$x.sd[i-1] - data$x.sd[i]) +
                                abs(data$y.sd[i-1] - data$y.sd[i]) +
                                abs(data$z.sd[i-1] - data$z.sd[i])) * 
                               data$svm[i] * 100/frequency)
    i <- i + 1
  }
  
  ## Movement Column
  i <- 2
  data$movement[1] <- 0
  while (i < length(data[,1])) {
    data$movement[i] <- sqrt(abs(data$x.mean[i] - data$x.mean[i-1] + 
                                   data$y.mean[i] - data$y.mean[i-1] + 
                                   data$z.mean[i] - data$z.mean[i-1]))
    i <- i + 1
  }
  
  ## 'calc' columns
  i <- 1
  while (i < length(data[,1])) {
    data$calc1[i] <- data$x.sd[i] + data$y.sd[i] + data$z.sd[i]
    data$calc2[i] <- data$svm[i] * data$movement[i] * (100 / frequency)
    data$movement.svm[i] <- data$movement[i] * data$svm[i]
    i <- i + 1
  }
  
  ## Bed column 
  i <- 1
  j <- 60
  data$bed <- FALSE
  while (i < 63) {
    data$bed.calc1[i] <- median(data$calc1[1:j])
    if (data$bed.calc1[i] < 0.1) data$bed[i] <- TRUE
    i <- i + 1
    j <- j + 1
  }
  j <- 2
  k <- 121
  while (i < length(data[,1])) {
    data$bed.calc1[i] <- median(data$calc1[i:j])
    if (data$bed.calc1[i] < 0.1) data$bed[i] <- TRUE
    i <- i + 1
    j <- j + 1
    k <- k + 1
  }
  
  ## 1-hour Average Movement*SVM
  i <- 1
  j <- 31
  k <- 1
  while (i < 30) {
    data$avg.movement.svm[i] <- mean(data$movement.svm[1:j])
    i <- i + 1
    j <- j + 1
  }
  while (i < length(data[,1])-30) {
    data$avg.movement.svm[i] <- mean(data$movement.svm[k:j])
    k <- k + 1
    i <- i + 1
    j <- j + 1
  }
  while (i < length(data[,1])) {
    data$avg.movement.svm[i] <- mean(data$movement.svm[i:length(data[,1])])
    i <- i + 1
  }
  
  ## Min of 1-hour average Movement*svm
  i <- 1
  j <- 31
  k <- 1
  while (i < 30) {
    data$min.movement.svm[i] <- min(data$avg.movement.svm[1:j])
    i <- i + 1
    j <- j + 1
  }
  j <- 
    while (i < length(data[,1])-30) {
      data$min.movement.svm[i] <- min(data$avg.movement.svm[k:j])
      k <- k + 1
      i <- i + 1
      j <- j + 1
    }
  while (i < length(data[,1])) {
    data$min.movement.svm[i] <- min(data$avg.movement.svm[i:length(data[,1])])
    i <- i + 1
  }
  
  ## No-Wear Detection
  i <- 1
  data$no.wear <- FALSE
  while (i < length(data[,1])) {
    data$no.wear[i] <- data$min.movement.svm[i] < 1
    i <- i + 1
  }
  
  ## Sleep detection
  i <- 1
  data$sleep.calc1 <- 0
  while (i < length(data[,1])) {
    if (data$calc2[i] < 8) 
      data$sleep.calc1[i] <- 1
    i <- i + 1
  }
  
  i <- 1
  data$sleep.calc2 <- 0
  while (i < 12) {
    data$sleep.calc2[i] <- sum(data$sleep.calc1[1:i])
    i <- i + 1
  }
  j <- 1
  while (i < length(data[,1])) {
    data$sleep.calc2[i] <- sum(data$sleep.calc1[j:i])
    i <- i + 1
    j <- j + 1
  }
  
  i <- 1
  data$sleep <- FALSE
  while (i < length(data[,1])) {
    if (!data$no.wear[i] && data$sleep.calc2[i] > 6)
      data$sleep[i] <- TRUE
    i <- i + 1
  }
  
  ## MET/min
  i <- 1
  while (i < length(data[,1])) {
    if (data$no.wear[i]) { 
      data$met.min[i] <- 0 
    }
    else if (data$sleep[i]) { 
      data$met.min[i] <- 0.9
    }
    else {
      if (0.9 > (data$svm[i] * 80 / (frequency * 125.34)) ^ 0.6983) {
        data$met.min[i] <- 0.9
      }
      else {
        data$met.min[i] <- (data$svm[i] * 80 / (frequency * 125.34)) ^ 0.6983
      }
    }
    i <- i + 1
  }
  data
}

# Generate a table of daily sleep summaries and energy expenditure
#' @export
gpa.summary <- function(data) {
  results <- data.frame()
  data$day <- substring(data$dateTime, 1 , 10)
  days <- unique(data$day)
  d <- 1
  
  while (d < length(days)) {
    sleep.count <- 0
    # Find bedtime
    temp <- subset(data, day == days[d])
    energy <- sum(temp$met.min)
    i <- length(temp[,1])
    # If sleeping at midnight move backwards
    if (temp$bed[i]) {
      while (temp$bed[i]) {
        i <- i - 1
      }
      bedtime <- temp$dateTime[i+1]
      sleep.count <- sum(temp$sleep[(i+1):length(temp[,1])])
    }
    # If not sleeping at midnight move forwards
    else {
      i <- 1
      temp <- subset(data, day == days[d+1])
      while (!temp$bed[i]) {
        i <- i + 1
      }
      bedtime <- temp$dateTime[i]
      saved <- i
    }
    
    # Find risetime
    i <- 1
    temp <- subset(data, day == days[d+1])
    sleeping <- TRUE
    # If sleeping at midnight find first awake
    if (temp$bed[i]) {
      while (sleeping) {
        while (temp$bed[i]) {
          i <- i + 1
        }
        if (!any(temp$bed[(i:i+frequency/5)])) {
          sleep.count <- sleep.count + sum(temp$sleep[1:i])
          risetime <- temp$dateTime[i]
          sleeping <- FALSE
        }
        else
          i <- i + 1
      }
    }
    # If not sleeping at midnight use bedtime to find
    else {
      i <- saved + 1
      while (sleeping) {
        while (temp$bed[i]) {
          i <- i + 1
        }
        if (!any(temp$bed[i:(i+frequency/5)])) {
          risetime <- temp$dateTime[i]
          sleeping <- FALSE
          sleep.count <- sum(temp$sleep[saved:i])
        }
        else
          i <- i + 1
      }
    }
    elapsed.sleep <- as.numeric(difftime(risetime, bedtime))
    
    results <- rbind(results, data.frame(date = days[d], 
                                         bedtime = bedtime,
                                         risetime = risetime,
                                         elapsed.sleep.time = elapsed.sleep,
                                         actual.sleep.time = sleep.count/60,
                                         sleep.efficiency = (sleep.count/60)/elapsed.sleep,
                                         energy.expenditure = energy))
    d <- d + 1
  }
  results
}

#'
#' @export
#gpa.activity