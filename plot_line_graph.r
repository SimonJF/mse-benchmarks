library(ggplot2)

plotFromFile <- function(filename) {
  csvData <- read.csv(filename)
  csvDataFiltered <- csvData[csvData[, "pings"] %% 200 == 0, ]
  plotGraph(csvDataFiltered)
  # log2 scaling of the y axis (with visually-equal spacing)
  
  #library(scales)     # Need the scales package
  #sp + scale_y_continuous(trans=log10_trans())
  
  # log2 coordinate transformation (with visually-diminishing spacing)
  #<- sp + coord_trans(y="log10")
}

plotGraph <- function(csvData) {
  summarisedData <- summarySE(csvData, measurevar="time", groupvars=c("type", "pings"))
  print(paste0("Current working dir: ", summarisedData["time", ]))
  ggplot(summarisedData, aes(x=pings, y=time, colour=type)) + 
    geom_errorbar(aes(ymin=time-se, ymax=time+se), width=.1) +
    geom_line() +
    geom_point() +
    xlab("Number of Pings") +
    ylab("Time taken / ms") +
    scale_colour_hue(name="Condition",    # Legend label, use darker colors
                     breaks=c("session_erlang", "session_erlang_noerr", "session_erlang_nomonitor", "vanilla_erlang"),
                     labels=c("Monitored Session Erlang (MSE)",
                              "MSE: No synchronous error reporting", "MSE: No monitoring", "Erlang gen_server2"),
                     l=40) + # Use darker colors, lightness=40
    ggtitle("Results of PingPong Benchmark for Monitored Session Erlang")
    
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}