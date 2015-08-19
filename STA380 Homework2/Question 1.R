library(ggplot2)
library(plyr)

rawdata = read.csv('ABIA.csv')
#Question I want to answer...
#What is the impact of a plane's arrival time into Austin on its departure time? 
#Aux: What is the average TAT for a carrier?
#This will of course require only flights that leave in the same day as their arrival.

flights = rawdata
#Remove any rows where the tail number is blank or NA
flights$TailNum[flights$TailNum == ''] = NA
flights = flights[complete.cases(flights$TailNum),]

#number sequentially the grouped tailnumbers
flights_seq = ddply(flights, .(TailNum), mutate, id = seq_along(TailNum))

flights_seq = flights_seq[c('TailNum','id','Month','DayofMonth','DepTime','UniqueCarrier','ArrDelay','DepDelay','Origin','Dest')]

flights_seq$MonthDay = paste(flights_seq$TailNum, flights_seq$Month, flights_seq$DayofMonth, sep='_')

flights_distilled = subset(flights_seq,duplicated(flights_seq$MonthDay) | duplicated(flights_seq$MonthDay, fromLast = TRUE))

flights_intoAUS = subset(flights_distilled, Origin!='AUS')
flights_outofAUS = subset(flights_distilled, Origin=='AUS')

#Merge the two together to allow for comparison of plane arrival and then plane departure.
Merged_flights = merge(flights_intoAUS, flights_outofAUS, by='MonthDay', type='full', all=TRUE, suffixes=c('_Arrive','_Depart'))
Merged_flights$ID_diff = Merged_flights$id_Depart - Merged_flights$id_Arrive

#Removing extra rows (flights that did not directly follow the arrival flight)
Merged_flights = subset(Merged_flights, ID_diff == 1)

#Select out only columns I care about (the merge resulted in a lot of extra columns)  -reusing the variable flights here
flights = Merged_flights[c('MonthDay','TailNum_Arrive','Month_Arrive','DayofMonth_Arrive','UniqueCarrier_Arrive','ArrDelay_Arrive','DepDelay_Arrive','Origin_Arrive','DepDelay_Depart','Dest_Depart')]

#Rename the columns to make them more managable
flights = rename(flights,c('MonthDay'='Unique_ID','TailNum_Arrive'='TailNum','Month_Arrive'='Month','DayofMonth_Arrive'='DayofMonth','UniqueCarrier_Arrive'='UniqueCarrier','ArrDelay_Arrive'='ArrDelay_intoAUS','DepDelay_Arrive'='DepDelay_origin','Origin_Arrive'='Origin','DepDelay_Depart'='DepDelay_AUS'))

#If the delay of arrival into austin was >10 min mark as 1, else 0
flights$WasArrDelay = ifelse(flights$ArrDelay_intoAUS > 10, 1, 0)

#Calculate the Difference between Departure Delay from AUS and Arrival Delay into AUS
flights$Dep_Arr = flights$DepDelay_AUS - flights$ArrDelay_intoAUS

#Begin plotting
ggplot(flights, aes(x=ArrDelay_intoAUS, y=DepDelay_AUS)) + geom_point(shape = 1, aes(colour=WasArrDelay)) + facet_wrap( ~ UniqueCarrier) +scale_colour_gradientn(colours=c("#D55E00","#0072B2")) + stat_smooth(method="lm", geom="smooth", se=TRUE)

#The following three plots were mostly unfruitful, only the preceeding plot gave useful information in regards to its facet's impact to delay time.
#ggplot(flights, aes(x=ArrDelay_intoAUS, y=DepDelay_AUS)) + geom_point(shape = 1, aes(colour=WasArrDelay)) + facet_wrap( ~ Dest_Depart) +scale_colour_gradientn(colours=c("#D55E00","#0072B2")) + stat_smooth(method="lm", geom="smooth", se=TRUE)
#ggplot(flights, aes(x=ArrDelay_intoAUS, y=DepDelay_AUS)) + geom_point(shape = 1, aes(colour=WasArrDelay)) + facet_wrap( ~ Month) +scale_colour_gradientn(colours=c("#D55E00","#0072B2")) + stat_smooth(method="lm", geom="smooth", se=TRUE)
#ggplot(flights, aes(x=ArrDelay_intoAUS, y=DepDelay_AUS)) + geom_point(shape = 1, aes(colour=WasArrDelay)) + facet_wrap( ~ DayofMonth) +scale_colour_gradientn(colours=c("#D55E00","#0072B2")) + stat_smooth(method="lm", geom="smooth", se=TRUE)
