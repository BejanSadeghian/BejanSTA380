library(ggplot2)

rawdata = read.csv('georgia2000.csv', header=TRUE)

#To create the box plot for equipment vs undercounted votes
boxplot((ballots-votes)~equip,rawdata)

#To create the box plot for equipment vs undercounted votes (log form)
boxplot(log(ballots-votes)~equip,rawdata)

#To initially investigate the impact of poor and equip on the number of votes
xtabs(votes~poor+equip, data=rawdata)

#Bar chart of the different equipment types used for poor and not poor
qplot(equip, data=rawdata, geom="bar", fill=equip) + facet_wrap(~ poor, ncol = 5)

#histogram of the percent African American to each type of voting equipment
ggplot(data=rawdata, aes(perAA)) + geom_histogram() + facet_wrap(~ equip, ncol = 5)
