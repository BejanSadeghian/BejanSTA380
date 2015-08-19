library(tm)
library(stringr)

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('../data/ReutersC50/C50train/*')
author_dirs = author_dirs[1:length(author_dirs)]
file_list = NULL
labels = NULL
authors_list = NULL
files_to_add = NULL

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

for(i in author_dirs){
  author = i
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

for(j in author_dirs){
  authors_list = append(authors_list, substring(j, first=29))
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


DTM = DocumentTermMatrix(my_corpus)
DTM
DTM = removeSparseTerms(DTM, 0.95) #Removing anything uses less than 5% o the time considering the size of this DTM
DTM
X = as.matrix(DTM) #Full train corpus






smoothCount = 1/50
count = 1

for(i in authors_list){
  #assign(paste0(substring(i, first = 29),'_train'), X[count:(count+49),])
  smoothFactor = colSums(X[count:(count+49),] + smoothCount)
  smoothFactor = smoothFactor/sum(smoothFactor)
  assign(paste0(i,'_SF'),smoothFactor)
  #if(i == "../data/ReutersC50/C50train/KarlPenhaul"){
  #print(smoothFactor)
  #}
  count = count + 50
}


#Compare against test set

author_dirs = Sys.glob('../data/ReutersC50/C50test/*')
author_dirs = author_dirs[1:length(author_dirs)]
file_list = NULL
labels = NULL

files_to_add = NULL

for(i in author_dirs){
  author = i
  author_name = substring(author, first=28)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
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


DTM = DocumentTermMatrix(my_corpus)
DTM = removeSparseTerms(DTM, 0.95) #Removing anything uses less than 5% o the time considering the size of this DTM

X = as.matrix(DTM)

#Calculating probabilties and making a prediction

df = data.frame(Document=rep(0,2500), Predict=rep(0,2500), Weight=rep(0,2500), Actual=rep(0,2500))
count = 1
index = 1
for(doc in file_list){
  current_best = -10000000000
  for(person in authors_list){
    temp_w = sum(X[count,]*log(get(paste0(person,'_SF'))))
    if (temp_w > current_best){
      current_person = person
      current_best = temp_w
    }
  }
  df[index,1] = str_sub(doc,-15)
  df[index,2] = current_person
  df[index,3] = current_best
  df[index,4] = substring(substring(doc, first=28), first=1, last=(nchar(substring(doc, first=28))-16))
  count = count + 1
  index = index + 1
}
