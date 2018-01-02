#Loading relevant libraries by looping through the list.
libraries = c("tm","plyr","class","caret","wordcloud","Rgraphviz")
lapply(libraries,require,character.only=TRUE)

source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

#Set options -- for keeping the text as it is
options(stringsAsFactors = FALSE)

#Setting parameters : Authors name should be same as directory name.
subjects=c("RobertChambers","WilliamShakespeare","RudyardKipling")
pathToArtefactsOfSubjects=".\\Resources"

#Functions#
#1:Function to clean the corpus/document
cleaningDocument=function(document){
  #using existing text miner functions
  temp = tm_map(document,removePunctuation)
  temp=tm_map(temp,stripWhitespace)
  temp=tm_map(temp,tolower)
  temp=tm_map(temp,removeWords, stopwords("english"))
  return(temp)
}

#2.1:Fucntion to build Term Document Matrix --essential part for document analysis
#Turning text into some quantitative matrix (one for each subject)
getTDM= function(subject,path){
  completePath = sprintf("%s\\%s",path,subject)
  doc = Corpus(DirSource(directory = completePath, encoding="UTF-8"))
  cleaned_doc = cleaningDocument(doc)
  tdm_of_doc = TermDocumentMatrix(cleaned_doc)
  tdm_of_doc = removeSparseTerms(tdm_of_doc,.7)
  result = list(name = subject,tdm = tdm_of_doc)
}

#2.2:Fucntion to build Term Document Matrix --essential part for document analysis
#Turning text into some quantitative matrix (one for each subject)
getTDMForWordCloud= function(subject,path){
  completePath = sprintf("%s\\%s",path,subject)
  doc = Corpus(DirSource(directory = completePath, encoding="UTF-8"))
  cleaned_doc = cleaningDocument(doc)
  tdm_of_doc = TermDocumentMatrix(cleaned_doc)
  tdm_of_doc = removeSparseTerms(tdm_of_doc,.7)
}

#3:Function to bind name of author
mapSubjectToTDM=function(tdm){
  #for making each term as col and speech as row
  temp_mat = t(data.matrix(tdm[["tdm"]]))
  #spreadsheet-- with cell having frquencey count
  temp_dataframe = as.data.frame(temp_mat,stringAsFactors=FALSE)
  #column bind with name column of the candidate
  temp_dataframe = cbind( temp_dataframe, rep(tdm[["name"]], nrow(temp_dataframe)) )
  colnames(temp_dataframe)[ncol(temp_dataframe)]="author"
  return(temp_dataframe)
}


#Looping through each subject to get TDM
tdm = lapply(subjects,getTDM,path = pathToArtefactsOfSubjects)
str(tdm)

tdm_wc_rc = getTDMForWordCloud(subjects[1],pathToArtefactsOfSubjects)
tdm_wc_ws = getTDMForWordCloud(subjects[2],pathToArtefactsOfSubjects)
tdm_wc_rk = getTDMForWordCloud(subjects[3],pathToArtefactsOfSubjects)

#Generating Word Cloud for RC
#said 1957;know  852;one  794
m <- as.matrix(tdm_wc_rc)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
plot(tdm_wc_rc,
     terms=findFreqTerms(tdm_wc_rc, lowfreq=400),
     corThreshold=0.55)


#Generating Word Cloud for WS
#haue -1346;thou-1307;will-1123
m <- as.matrix(tdm_wc_ws)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

plot(tdm_wc_ws,
     terms=findFreqTerms(tdm_wc_ws, lowfreq=450),
     corThreshold=.55)



#Generating Word Cloud for RK
#said - 2864;one- 1166; man - 949
m <- as.matrix(tdm_wc_rk)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
plot(tdm_wc_rk,
     terms=findFreqTerms(tdm_wc_rk, lowfreq=450),
     corThreshold=0.55)





subject_mapped_TDM=lapply(tdm,mapSubjectToTDM)
str(subject_mapped_TDM)


#Consolidating records for each subject
#row binding them -- so consolidating all terms and na to be replaced by 0 since some terms will be mutually exclusive to other subject's artefacts.
consolidated_tdm = do.call(rbind.fill,subject_mapped_TDM)
consolidated_tdm[is.na(consolidated_tdm)]=0
str(consolidated_tdm)






#head(consolidated_tdm)
#nrow(consolidated_tdm) #74=Total no of documents
#ncol(consolidated_tdm) #4133=Total no of unique words

#Randomly picking training(70% here) set indices and test set indices.
train_set_indices = sample(nrow(consolidated_tdm),ceiling(nrow(consolidated_tdm) * 0.7))
test_set_indices = (1:nrow(consolidated_tdm))[-train_set_indices]

#head(test_set_indices) -- randomly sampled.
#length(test_set_indices) = 21 out of total 74 documents

cons_tdm_with_author = consolidated_tdm[,"author"]
cons_tdm_without_author = consolidated_tdm[,!colnames(consolidated_tdm) %in% "author"]
#cons_tdm_without_author["author"] --should give errror

#cons_tdm_with_author[train_set_indices] -- names just for the training set
prediction=knn(cons_tdm_without_author[train_set_indices,],cons_tdm_without_author[test_set_indices,],cons_tdm_with_author[train_set_indices])


#accuracy using confusion matrix
#actuals are the names of subjects in test matrix
confusion_matrix = table("Predicted" = prediction, Actual = cons_tdm_with_author[test_set_indices])
#diagonal elements represent correctly matched items.
prediction_accuracy = sum(diag(confusion_matrix)) / length(test_set_indices) * 100

confusionMatrix(confusion_matrix)



