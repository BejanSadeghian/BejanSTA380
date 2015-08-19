library(tm)
library(stringr)
library(plyr)
library(SnowballC)


## Rolling two directories together into a single corpus
author_dirs = Sys.glob('../STA380 Homework2/ReutersC50/C50train/*')
file_list = NULL
labels = NULL
authors_list = NULL
files_to_add = NULL

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

for(i in author_dirs){
  author = i
  author_name = substring(author, first=41)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

for(j in author_dirs){
  authors_list = append(authors_list, substring(j, first=41))
}

#Pieces of this code was provided by Dr. James Scott's Naive Bayes R file.

all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = labels


# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower))
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers))
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation))
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace))
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords('SMART'))
my_corpus = tm_map(my_corpus, stemDocument)




#Creating a DTM and a list of all terms used
DTM = DocumentTermMatrix(my_corpus, control= list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
DTM
DTM = removeSparseTerms(DTM, 0.99) #Removing anything uses less than 1% of the time 
DTM

ListofTrainTerms = DTM$dimnames
ListofTrainTerms = ListofTrainTerms$Terms

X = as.matrix(DTM) #Full train corpus

smoothCount = 1/50
count = 1

for(i in authors_list){
  #assign(paste0(substring(i, first = 29),'_train'), X[count:(count+49),])
  smoothFactor = colSums(X[count:(count+49),] + smoothCount)
  smoothFactor = smoothFactor/sum(smoothFactor)
  assign(paste0(i,'_SF'),smoothFactor)
  count = count + 50
}


#Compare against test set

author_dirs = Sys.glob('../STA380 Homework2/ReutersC50/C50test/*')
file_list = NULL
labels = NULL
author_list = NULL
files_to_add = NULL

for(i in author_dirs){
  author = i
  author_name = substring(author, first=40)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  for(ix in seq(1,50)){
    author_list = append(author_list, author_name)
  }
  labels = append(labels, rep(author_name, length(files_to_add)))
  names_of_files = str_sub(files_to_add, -16)
}

#Pieces of this code was provided by Dr. James Scott's Naive Bayes R file.

all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = labels

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower))
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers))
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation))
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace))
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords('SMART'))
my_corpus = tm_map(my_corpus, stemDocument)


#Creating the DTM and a list of all of the terms used
DTM = DocumentTermMatrix(my_corpus, control= list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
DTM
DTM = removeSparseTerms(DTM, 0.99) #Removing anything uses less than 1% of the time
DTM

ListofTestTerms = DTM$dimnames
ListofTestTerms = ListofTestTerms$Terms

X_test = as.matrix(DTM)

#Combining the terms to make sure the test DTM has only terms that are also in the train set
Combined = data.frame(X_test[,intersect(colnames(X_test),colnames(X))])
temp.table = read.table(textConnection(''), col.names=colnames(X),colClasses='integer')
X_eval = rbind.fill(Combined, temp.table)
X_eval[is.na(X_eval)] = 0
X_eval = X_eval[,sort(names(X_eval))]
X_eval = as.matrix(X_eval)

#Calculating probabilties and making a prediction

df = data.frame(Document=rep(0,2500), Predict=rep(0,2500), Weight=rep(0,2500), Actual=rep(0,2500), Match=rep(0,2500))
df[,4] = author_list
count = 1
index = 1
for(doc in file_list){
  current_best = -10000000000
  for(person in authors_list){
    temp_w = sum(X_eval[count,]*log(get(paste0(person,'_SF'))))
    if (temp_w > current_best){
      current_person = person
      current_best = temp_w
    }
  }
  df[index,1] = str_sub(doc,-15)
  df[index,2] = current_person #Best Prediction
  df[index,3] = current_best
  df[index,5] = ifelse(df[index,4] == df[index,2], 1, 0)
  count = count + 1
  index = index + 1
}


#Calculate Accuracy
Accuracy.Score = sum(df[[5]])/length(df[[5]])
print(Accuracy.Score)

#Look at the tabulation and print which authors are very often mixed with another author.
crosstabulate = xtabs(~Actual + Predict, data=df)
cross.df = as.data.frame.matrix(crosstabulate)
rownames = rownames(crosstabulate)
colnames = rownames(crosstabulate)
for(i in seq(1,50)){
  if(colnames[[i]] != rownames[which(cross.df[[i]]==max(cross.df[[i]]))]){
    print(paste0(colnames[[i]],' is commonly mistaken for ',rownames[which(cross.df[[i]]==max(cross.df[[i]]))]))
  }
}
