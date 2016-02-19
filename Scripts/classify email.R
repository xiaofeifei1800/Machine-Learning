# loading package
library(tm)  # for text mining

# set the directory
txt1<-"H:/新建文件夹 (2)/email/ham/"  

#read in the txt files
txtham<-Corpus(DirSource(txt1),readerControl=list(language = "en"))

# clean the text
# change all char into lower case
txtham<-tm_map(txtham,tolower)

# remove all the punctuations
txtham<-tm_map(txtham,removePunctuation)

# remove the stopwords
txtham<-tm_map(txtham,removeWords, stopwords("english"))

txtham = tm_map(txtham, removeNumbers)

# remove the extra space between the words
txtham<-tm_map(txtham,stripWhitespace)


# do something again to the spam, will made a function of it, so
# don't need to wrote it again
txt2<-"H:/新建文件夹 (2)/email/spam/" 
txtspam<-Corpus(DirSource(txt2),readerControl=list(language = "en"))

txtham<-tm_map(txtham,tolower)
txtham<-tm_map(txtham,removePunctuation)
txtspam<-tm_map(txtspam,removeWords, stopwords("english"))
txtspam = tm_map(txtspam, removeNumbers)
txtspam<-tm_map(txtspam,stripWhitespace)

#txtham = unlist(txtham)
#txt = lapply(txtham, function(x) strsplit(x, " "))
#txt = lapply(1:length(txt), function(x)  gsub("([A-z]*).*", "\\1", unlist(txt[[x]])))
#txtham = lapply(1:length(txt), function(x) txtham[[x]][!txtham[[x]] == ""])
#txtham<-Corpus(VectorSource(txtham),readerControl=list(language= "en")) 

#txtspam = unlist(txtspam)
#txt2 = lapply(txtspam, function(x) strsplit(x, " "))
#txt2 = lapply(1:length(txt2), function(x)  gsub("([A-z]*).*", "\\1", unlist(txt2[[x]])))
#txtsham = lapply(1:length(txt2), function(x) txtspam[[x]][!txtspam[[x]] == ""])
#txtspam<-Corpus(VectorSource(txtspam),readerControl=list(language= "en")) 

# Remove the space char
txtham = lapply(1:length(txtham), function(x) txtham[[x]][!txtham[[x]] == ""])
txtham<-Corpus(VectorSource(txtham),readerControl=list(language= "en")) 

txtspam = lapply(1:length(txtspam), function(x) txtspam[[x]][!txtspam[[x]] == ""])
txtspam<-Corpus(VectorSource(txtspam),readerControl=list(language= "en")) 

# convert it into term matrix
dtm1<-DocumentTermMatrix(txtham)

# get the length of the words without repeating
n1<-length(findFreqTerms(dtm1, 1))

dtm2<-DocumentTermMatrix(txtspam)
n2<-length(findFreqTerms(dtm2, 1))


#######################################################
setwd("H:/新建文件夹 (2)/email/spam" )  
txt2<-"H:/新建文件夹 (2)/email/spam/" 
names<-list.files(txt2)  
data1<-paste("spam",1:23)  
lenspam<-0  
for(i in 1:length(names)){  
  assign(data1[i],scan(names[i],"character"))  
  lenspam<-lenspam+length(get(data1[i]))  
}  
   
setwd("H:/新建文件夹 (2)/email/ham") 
txt1<-"H:/新建文件夹 (2)/email/ham/" 
names<-list.files(txt1)  
data<-paste("ham",1:23)  
lenham<-0  
for(i in 1:length(names)){  
  assign(data[i],scan(names[i],"character"))  
  lenham<-lenham+length(get(data[i]))  
}  

######################################################
3.48
多项式模型
prob<-function(char,corp,len,n){
	re<-inspect(DocumentTermMatrix(corp, list(dictionary = char)))
	as.matrix(re)
	dtm<-DocumentTermMatrix(corp)
	prob<-(sum(re[,1])+1)/(n+len)
	return(prob)
}

testingNB<-function(sentences){
	pro1<-0.5
	pro2<-0.5
	for(i in 1:length(sentences)){
		pro1<-pro1*prob(sentences[i],txtham,lenham,n1)
	}
	for(i in 1:length(sentences)){
		pro2<-pro2*prob(sentences[i],txtspam,lenspam,n2)
	}
	return(list(prob.ham = pro1,
              prob.span = pro2,
              prediction = ifelse(pro1>=pro2/10, "ham", "spam")))
}

###############伯努利模型###########################
prob<-function(char,corp,n){
	re<-inspect(DocumentTermMatrix(corp, list(dictionary = char)))
	as.matrix(re)
	dtm<-DocumentTermMatrix(corp)
	n<-length(findFreqTerms(dtm, 1))
	prob<-(sum(re[,1]>0)+1)/(n+2)
	return(prob)
}

testingNB<-function(sentences){
	pro1<-0.5
	pro2<-0.5
	for(i in 1:length(sentences)){
		pro1<-pro1*prob(sentences[i],txtham,n1)
	}
	for(i in 1:length(sentences)){
		pro2<-pro2*prob(sentences[i],txtspam,n2)
	}
	return(list(prob.ham = pro1,
              prob.spam = pro2,
              prediction = ifelse(pro1>=pro2, "ham", "spam")))
}

#############################################################

email<-scan("H:/新建文件夹 (2)/email/test/ham1.txt","character")
sentences<-unlist(strsplit(email,",|\\?|\\;|\\!"))
a<-tolower(sentences)

testingNB(a)

################ fix

dtm1<-as.matrix(dtm1)
freq1 <- sum(colSums(dtm1))

dtm2 <- as.matrix(dtm2)
freq2 <- sum(colSums(dtm2))

prob<-function(char,corp)
{
	re<-DocumentTermMatrix(corp, list(dictionary = char))
	re = as.matrix(re)
  freq =sum(colSums(re))
	len = length(sentences)
	prob<-(freq+1)/(sum(freq1)+sum(freq2)+len)
	return(prob)
}

testingNB<-function(sentences)
{
	pro1<-(freq1+freq2)/freq1
	pro2<-(freq1+freq2)/freq2
	for(i in 1:length(sentences)){
		pro1<-pro1*prob(sentences[i],txtham)
	}
	for(i in 1:length(sentences)){
		pro2<-pro2*prob(sentences[i],txtspam)
	}
	return(list(prob.ham = pro1,
              prob.span = pro2,
              prediction = ifelse(pro1>=pro2/10, "ham", "spam")))
}

##############################################
email<-scan("H:/新建文件夹 (2)/email/test/ham2.txt","character")

sentences<-Corpus(VectorSource(email),readerControl=list(language = "en"))

# clean the text
# change all char into lower case
sentences<-tm_map(sentences,tolower)



# remove the stopwords
sentences<-tm_map(sentences,removeWords, stopwords("english"))

# remove the extra space between the words
sentences<-tm_map(sentences,stripWhitespace)

sentences = unlist(sentences)
sentences = as.character(sentences)
sentences = lapply(1:length(sentences), function(x) sentences[[x]][!sentences[[x]] == ""])
sentences = unlist(sentences)
sentences = as.character(sentences)
testingNB(sentences)
