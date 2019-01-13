#Naivy Bayes classifier
#	Search for words more likely to happen in a class
#	Compute the probability of a word happen conditioned the corpus belong to a class
#	Naive model : assume that words are statistical independent
#	Bayes model : make use of base ration information about classes

library(tm)
library(ggplot2)

spam.path <- "~/GitHub/Naivy_Bayes/spam/"
eham.path <- "~/GitHub/Naivy_Bayes/easy_ham/"
hham.path <- "~/GitHub/Naivy_Bayes/hard_ham/"

get.msg <- function(path){
	con  <- file(path,open="rt",encoding="latin1")
	text <- readLines(con)
	msg  <- text[seq(which(text=="")[1]+1,length(text),1)]

	close(con)
	return(paste(msg,collapse="\n"))
}

get.tdm <- function(doc.vec){
	doc.corpus <- Corpus(VectorSource(doc.vec)) # ?getSources
	control <- list(stopwords=TRUE,removePunctuation=TRUE,removeNumbers=TRUE,minDocFreq=2)
	doc.tdm <- TermDocumentMatrix(doc.corpus,control)

	return(doc.tdm)
}

classify.spam <- function(path,spam_training.df,eham_training.df,prior=0.8,c=1e-6){
	msg      <- get.msg(path)
	msg.tdm  <- get.tdm(msg)
	msg.freq <- rowSums(as.matrix(msg.tdm))

	msg.match <- intersect(names(msg.freq),spam_training.df$term)
	
	if(length(msg.match) < 1){
		sp <- prior*c^(length(msg.freq))
	} else {
		match.probs <- spam_training.df$occurrence[match(msg.match,spam_training.df$term)]
		sp <- prior * prod(match.probs) * c^(length(msg.freq) - length(msg.match))
	}

	msg.match <- intersect(names(msg.freq),eham_training.df$term)
	
	if(length(msg.match) < 1){
		hp <- prior*c^(length(msg.freq))
	} else {
		match.probs <- eham_training.df$occurrence[match(msg.match,eham_training.df$term)]
		hp <- prior * prod(match.probs) * c^(length(msg.freq) - length(msg.match))
	}

	if(sp > hp){
		return(TRUE)
	} else {
		return(FALSE)
	}

}

#Spams ------------------------------------------------------------------------------------------------

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[2:length(spam.docs)]
all.spams <- sapply(spam.docs,function(p) get.msg(paste(spam.path,p,sep="")))
all.spams <- all.spams[which(all.spams!="")]

spam.tdm  <- get.tdm(all.spams)
spam.mat  <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.mat)
spam.df     <- data.frame(cbind(names(spam.counts),as.numeric(spam.counts)),stringsAsFactors=FALSE)
names(spam.df) <- c("term","frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.mat),function(i) {length(which(spam.mat[i,] > 0))/ncol(spam.mat)}) #percentage of documents a given term occurs
spam.density    <- spam.df$frequency/sum(spam.df$frequency) #frequency within the entire corpus
spam.df <- transform(spam.df,density=spam.density,occurrence=spam.occurrence)

#Easy Ham ---------------------------------------------------------------------------------------------
#	Assuming theres an equal chance of an e-mail been classified as as spam or ham

eham.docs <- dir(eham.path)
eham.docs <- eham.docs[2:2001]
all.ehams <- sapply(eham.docs,function(p) get.msg(paste(eham.path,p,sep="")))
all.ehams <- all.ehams[which(all.ehams!="")]

eham.tdm  <- get.tdm(all.ehams)
eham.mat  <- as.matrix(eham.tdm)
eham.counts <- rowSums(eham.mat)
eham.df     <- data.frame(cbind(names(eham.counts),as.numeric(eham.counts)),stringsAsFactors=FALSE)
names(eham.df) <- c("term","frequency")
eham.df$frequency <- as.numeric(eham.df$frequency)

eham.occurrence <- sapply(1:nrow(eham.mat),function(i) {length(which(eham.mat[i,] > 0))/ncol(eham.mat)}) #percentage of documents a given term occurs
eham.density    <- eham.df$frequency/sum(eham.df$frequency) #frequency within the entire corpus
eham.df <- transform(eham.df,density=eham.density,occurrence=eham.occurrence)

#Classification --------------------------------------------------------------------------------------

hham.docs <- dir(hham.path)
hham.docs <- hham.docs[2:length(hham.docs)]

hham.spamtest <- sapply(hham.docs,function(p) classify.spam(paste(hham.path,p,sep=""),spam_training.df=spam.df,eham_training.df=eham.df))
summary(hham.spamtest)
