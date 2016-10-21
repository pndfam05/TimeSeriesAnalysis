
library(lubridate)
library(ggplot2)
# library(readxl)

# User variable. Set to the length of a shift that can be dedicataed
# to active eradication of the problem. Not required if no specific
# enforcement efffort is anticipated
shift <- 8

# TZ Correct allows you to use times from different time xones and convert
# them into the same time zone. TZ Correct need not be the GMT offset
# unless you want to change all of your times to be in GMT. For instance, you
# have a thousand records of attempted computer intrusions. The records are
# in GMT but you believe your criminal is in Riyadh, Saudia Arabia. A time
# analysis in your suspect's time zone will make it easier to understand his
# activities than it would be if you tried to understand his activities in
# GMT.

# Alternatively, if you have records from different time zones you probably
# want to convert them all to the same time zone for comparison.

# If all of your records are from the same time zoe and tht is the time zone
# of your examination then TZ Correct must be set to 0 [zero].

# Date output must conform to this format with headers. The fields can appear
# in any order and with other fields as long as the three fileds listed
# below are named corectly. The correct format using Excel number formatting is:
# Custom yyyy-mm-dd hh:mm:ss

# TZ Correct,Date From,Date To
# 1,2015-10-04 00:00,2015-10-04 03:00
# 1,2015-10-10 16:00,2015-10-11 01:00
# 1.5,2015-10-04 01:00,2015-12-12 01:00

# creates MT df for output
out.df <- data.frame(data.frame(matrix(nrow = 0, ncol=168)),
                     stringsAsFactors = FALSE)

colnames(out.df) <- c(0:167) # Add column names that start at 0 go to 167

myFile <- file.choose(new=FALSE) # open a file choose window
# myFile <- "C:\\Users\\toronto$123\\Documents\\R\\TimeSeries\\Time.csv"

# read the .csv file specified by myFile into a data.frame
times.df <- read.csv(myFile, header = TRUE, sep = ",", quote = "\"",
                     dec = ".", fill = TRUE, comment.char = "")

# Correct dates for time zone differences
times.df[,"Date.From"] <- ymd_hms(times.df[,"Date.From"]) + (times.df[,"TZ.Correct"] * 3600)
times.df[,"Date.To"] <- ymd_hms(times.df[,"Date.To"]) + (times.df[,"TZ.Correct"] * 3600)

# use this to roll back a "date to" time that is not an even hour. An event
# that occurred between 0100 and 0200 could not have occurred in the 0200 hour.
# By subtracting two minutes ensures the second hour won't be counted
times.df[,"Date.To"] <- ymd_hms(times.df[,"Date.To"]) - minutes(2)

# Break each time sequence into proportional parts per hour that could
# have occurred.
for(i in seq(nrow(times.df))){

begin.hr <- ((wday(times.df[i,"Date.From"]) - 1) * 24) + (hour(times.df[i,"Date.From"])) + 1

end.hr <- ((wday(times.df[i,"Date.To"]) - 1) * 24) + (hour(times.df[i,"Date.To"])) + 1

out.df[i,] <- 0

if((as.numeric(ymd_hms(times.df[i,"Date.To"])) - as.numeric(ymd_hms(times.df[i,"Date.From"]))) / 3600 > 167){
  # do nothing

} else if(end.hr >= begin.hr){
  out.df[i,begin.hr:end.hr] <- 1 / (end.hr - begin.hr + 1)

} else if(end.hr < begin.hr){
 # do something
  out.df[i,] <- 1 / (168 - (begin.hr - end.hr - 1))
  out.df[i,(end.hr + 1):(begin.hr - 1)] <- 0
}
}

# Sum individual hour column
TimeSeries.Unk <- colSums(out.df,na.rm = TRUE)

# Create a vector of probabilities for each hour that the actual hours
# of occurrence could be. Normally, this will be 1 for each of the 168
# hours but could change if you have a specific suspect in mind and know
# for certain that the suspect COULD NOT HAVE committed the crime. Perhaps
# because your suspect was in custody.
source("C:\\Users\\toronto$123\\Documents\\R\\TimeSeries\\TimeSeries.Std.R")
chisq.test(TimeSeries.Unk,p=TimeSeries.Std,rescale.p = TRUE)

# single chisq.test, can also be used if the probability is even for each of the
# 168 hours
# chisq.test(TimeSeries.Unk)

# Capture chi.square value as a variable so it can be used elsewhere in the code
chisq.value <- round(chisq.test(TimeSeries.Unk,p=TimeSeries.Std,rescale.p = TRUE)$p.value,4)

# IF CHI.SQUARE IS LOW ENOUGH (CAN REJECT NULL HYPOTHESIS THAT PATTERN IS RANDOM)
# THEN CONSIDER PROCEEDING. IF CHI/SQURE IS >.05 (standard, accepted value in statistical science)
# THEN PATTERN COULD BE INDISTINGUISHABLE FROM RANDOM AND PROCEEDING WITH ANY ENFORCEMENT
# OR ANALYTICAL EFFORT MIGHT NOT BE RELIABLE.

# Create a data.frame within which we'll store the cuumulative probabilties
# starting each hour of the week and summing up a period equal to the value
# of shift.

Likelihood.df <- data.frame(data.frame(matrix(nrow = 168, ncol=2)),
                      stringsAsFactors = FALSE)

colnames(Likelihood.df) <- c("Hour","Prob")

Likelihood.df[,1] <- c(1:168) # number ros to correspod to hour of the week

# Fill the scond column with the column summs of the out.df
Likelihood.df[,2] <- colSums(out.df,na.rm = TRUE)

source("C:\\Users\\toronto$123\\Documents\\R\\TimeSeries\\TimeSeries.PlotLabels.R")
source("C:\\Users\\toronto$123\\Documents\\R\\TimeSeries\\TimeSeries.PlotConfig.R")

p <- ggplot(data=Likelihood.df[1:168,], aes(x=Hour, y=Prob, group=1))

p + layer(geom="line") +
  defs +
  annotate("text", x = 84,
          y = (max(Likelihood.df[,2]) * 0.9),
         label = paste("Chi-Squared < 0.05 means distribution is probably not random \n Chi-Squared = ",chisq.value)) +

  labs(title="Hourly Likelihood", x="Day/Hour", y="Relative Likelihood")

# *******************************************************
# Plot Time Series

source("C:\\Users\\toronto$123\\Documents\\R\\TimeSeries\\TimeSeries.Best.CreateDF.R")

p <- ggplot(data=TimeSeries_Best.df[1:168,], aes(x=Hour, y=Score, group=1))

# Here we include the data and the aesthetics grammar elements. We also
# told ggplot2 to group the datapoints together so that a line can be drawn
# to join them; that is the group=1 parameter. But we need to add a layer
# to actually have a plot:

p + layer(geom="line") +
  defs +
  geom_vline(xintercept = seq(TimeSeries.BestStart,
                              TimeSeries.BestStart + shift, 1),
             colour = "indianred1", linetype = 6, size = .5) +

  annotate("text", x = TimeSeries.BestStart - 3 ,
					y = TimeSeries_Best.df[TimeSeries.BestStart,3] - 3,
					label = TimeSeries_Best.df[TimeSeries.BestStart,2],angle=90) +

  annotate("text", x = TimeSeries.BestStart + shift + 3,
					y = TimeSeries_Best.df[TimeSeries.BestStart,3] - 3,
					label = TimeSeries_Best.df[TimeSeries.BestStart + shift, 2], angle = -90) +

  labs(title=paste("Optimum Shift Timing Given", shift, "Hour Shift")
                  , x="Day/Hour", y="Relative Likelihood")

#
# *****************************************************************

