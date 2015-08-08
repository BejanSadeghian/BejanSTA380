library(mosaic)
library(ggplot2)

rawdata = read.csv('wine.csv', header=TRUE)

wineData = scale(rawdata[,1:11]) #Scale the data
wineData = cbind(wineData,rawdata[,12:13])


#Find a K number that will lead to the 
numK = 5
SSEArray = rep(0,numK)
#BICArray = rep(0,numK)
for(i in 1:numK){
  wineCluster = kmeans(wineData[1:11],centers = i, nstart = 10)
  SSEArray[i] = wineCluster$tot.withinss
}
plot(SSEArray)


#Performing K-means and plotting
numK = 5

for(r in c(5,10)){
numK = r
ClusterAll = kmeans(wineData[1:11], centers=numK, nstart=10)

names(ClusterAll)

finalwine = cbind(wineData, ClusterAll$cluster)
finalwine = finalwine[,c(13,14)]
names(finalwine)[2]<-'cluster'

qplot(cluster, data=finalwine, geom="bar", fill=as.factor(cluster)) + facet_wrap(~ color, ncol = 6)

#Calculating the percent accuracy at guessing the color of wine per cluster

ResultsTable = table(finalwine)
red = rep(0,numK)
count = 1
for(i in seq(1,numK*2,2)){
  red[count] = ResultsTable[i]/sum(ResultsTable[i],ResultsTable[i+1])
  count = count+1
}

white = rep(0,numK)
count = 1
for(i in seq(1,numK*2,2)){
  white[count] = ResultsTable[i+1]/sum(ResultsTable[i],ResultsTable[i+1])
  count = count+1
}

ColorAccuracy = rbind(red,white)
print(ColorAccuracy)

}


##Calculate the accuracy of quality clustering
for(r in c(5,10)){
  numK = r
  ClusterAll = kmeans(wineData[1:11], centers=numK, nstart=10)
  
  names(ClusterAll)
  
  finalwine = cbind(wineData, ClusterAll$cluster)
  finalwine = finalwine[,c(12,14)]
  names(finalwine)[2]<-'cluster'
  
  qplot(cluster, data=finalwine, geom="bar", fill=as.factor(cluster)) + facet_wrap(~ quality, ncol = 6)
  
  #Calculating the percent accuracy at guessing the color of wine per cluster
  
  ResultsTable = as.data.frame(table(finalwine))
  
  QualityAccuracy = c('Quality 1','Quality 2','Quality 3','Quality 4','Quality 5','Quality 6','Quality 7','Quality 8','Quality 9','Quality 10')
  for(x in 1:9){
    cluster = rep(0,numK)
    for(y in 1:numK){
      cluster[y] = with(ResultsTable,sum(ResultsTable[quality==x & cluster==y,'Freq']))/with(ResultsTable,sum(ResultsTable[cluster==y,'Freq']))
    }
    QualityAccuracy = rbind(QualityAccuracy,t(as.data.frame(cluster)))
  }
  rownames(QualityAccuracy) = c('Names',1,2,3,4,5,6,7,8,9)
  print('K')
  print(numK)
  print(QualityAccuracy)
  }
  








###Trying Principal Component Analysis###

#Reimporting the data
rawdata = read.csv('wine.csv', header=TRUE)

#Remove color and quality rating
workingData = rawdata[,1:11]

#Centering the data but not scaling
centeredData = scale(workingData, center=TRUE, scale=FALSE)

#Calculating the principal components and extracting PC1
PC1 = prcomp(centeredData)
v_best = PC1$rotation[,1]

#inner product of x and v (PC1) gives us the alpha, the scalar score, of each projection
alpha_best = centeredData %*% v_best

#Calculating the amount of variance captured
varofdata = apply(centeredData,2,var)
print(var(alpha_best)/sum(varofdata))

##Because there are so many dimensions it gets very difficult to plot
