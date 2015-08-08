library(ggplot2)
library(reshape)
library(foreach)

rawdata = read.csv('social_marketing.csv')

moddata = rawdata[,-1]

datascaled = scale(moddata, center=TRUE, scale=TRUE)

K = 40
#Try K-means
ErrorArray = rep(0,K)
for(i in 1:K){
  dataCluster = kmeans(datascaled,centers = i, nstart = 10)# method = 'kmeans', dist='euclidean', save.data=TRUE)
  ErrorArray[i] = dataCluster$tot.withinss
}
plot(ErrorArray)

#Perform K-Means with the optimized K value
K=15
dataCluster = kmeans(datascaled,centers = K, nstart = 10)

Centers = dataCluster$centers
transposeCenters = t(Centers)
transposeCenters = as.data.frame(transposeCenters)
transposeCenters = cbind(transposeCenters,rownames(transposeCenters))
colnames(transposeCenters) = c('one','two','three','four','five','six','seven','eight','nine','ten','eleven','twelve','thirteen','fourteen','fifteen','item')
meltCenters = melt(transposeCenters,id=c('item'))

ggplot(meltCenters, aes(x=item, fill=variable, y=value)) + geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


LowInterest = foreach(i = 1:(length(transposeCenters)-1),.combine=cbind, .inorder=TRUE) %do% {
  head(rownames((transposeCenters[with(transposeCenters, order(transposeCenters[,i])), ])))
}
LowInterest = rbind(LowInterest,t(as.matrix(dataCluster$size)))

HighInterest = foreach(i = 1:(length(transposeCenters)-1),.combine=cbind, .inorder=TRUE) %do% {
  tail(rownames((transposeCenters[with(transposeCenters, order(transposeCenters[,i])), ])))
}
HighInterest = rbind(HighInterest,t(as.matrix(dataCluster$size)))


