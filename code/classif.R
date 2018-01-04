install.packages("SnowballC")
install.packages("tm")
install.packages("FSelector")
install.packages("caret")

library(tm)
library(FSelector)
library(caret)
library(RWeka)
library(doMC)
registerDoMC(cores = 3)

####################################
# load the corpora and pre process #
####################################

ciencia.train = VCorpus(DirSource('../news/train/ciencia'), readerControl = list(reader = readPlain, language='pt'))
ciencia.test = VCorpus(DirSource('../news/eval/ciencia'), readerControl = list(reader = readPlain, language='pt'))

cultura.train = VCorpus(DirSource('../news/train/cultura'), readerControl = list(reader = readPlain, language='pt'))
cultura.test = VCorpus(DirSource('../news/eval/cultura'), readerControl = list(reader = readPlain, language='pt'))

desporto.train = VCorpus(DirSource('../news/train/desporto'), readerControl = list(reader = readPlain, language='pt'))
desporto.test = VCorpus(DirSource('../news/eval/desporto'), readerControl = list(reader = readPlain, language='pt'))

economia.train = VCorpus(DirSource('../news/train/economia'), readerControl = list(reader = readPlain, language='pt'))
economia.test = VCorpus(DirSource('../news/eval/economia'), readerControl = list(reader = readPlain, language='pt'))

mundo.train = VCorpus(DirSource('../news/train/mundo'), readerControl = list(reader = readPlain, language='pt'))
mundo.test = VCorpus(DirSource('../news/eval/mundo'), readerControl = list(reader = readPlain, language='pt'))

musica.train = VCorpus(DirSource('../news/train/musica'), readerControl = list(reader = readPlain, language='pt'))
musica.test = VCorpus(DirSource('../news/eval/musica'), readerControl = list(reader = readPlain, language='pt'))

pais.train = VCorpus(DirSource('../news/train/pais'), readerControl = list(reader = readPlain, language='pt'))
pais.test = VCorpus(DirSource('../news/eval/pais'), readerControl = list(reader = readPlain, language='pt'))

politica.train = VCorpus(DirSource('../news/train/politica'), readerControl = list(reader = readPlain, language='pt'))
politica.test = VCorpus(DirSource('../news/eval/politica'), readerControl = list(reader = readPlain, language='pt'))


preprocess.simple <- function(d){
    d <- tm_map(d, content_transformer(tolower))
    d <- tm_map(d, removeWords, stopwords(kind = "pt"))
    d <- tm_map(d, content_transformer(function(x) iconv(x, to="ASCII//TRANSLIT")))
    d <- tm_map(d, removePunctuation, preserve_intra_word_dashes = TRUE)
    d <- tm_map(d, removeNumbers)
    d <- tm_map(d, content_transformer(function(x) stemDocument(x, language="pt")))
    d <- tm_map(d, stripWhitespace)
    return(d)
}

ciencia.train.p <- preprocess.simple(ciencia.train)
ciencia.test.p <- preprocess.simple(ciencia.test)

cultura.train.p <- preprocess.simple(cultura.train)
cultura.test.p <- preprocess.simple(cultura.test)

desporto.train.p <- preprocess.simple(desporto.train)
desporto.test.p <- preprocess.simple(desporto.test)

economia.train.p <- preprocess.simple(economia.train)
economia.test.p <- preprocess.simple(economia.test)

mundo.train.p <- preprocess.simple(mundo.train)
mundo.test.p <- preprocess.simple(mundo.test)

musica.train.p <- preprocess.simple(musica.train)
musica.test.p <- preprocess.simple(musica.test)

pais.train.p <- preprocess.simple(pais.train)
pais.test.p <- preprocess.simple(pais.test)

politica.train.p <- preprocess.simple(politica.train)
politica.test.p <- preprocess.simple(politica.test)



docs.train = c(ciencia.train.p, cultura.train.p, desporto.train.p, economia.train.p, mundo.train.p, musica.train.p, pais.train.p, politica.train.p)
docs.test = c(ciencia.test.p, cultura.test.p, desporto.test.p, economia.test.p, mundo.test.p, musica.test.p, pais.test.p, politica.test.p)


####################
# create data sets #
####################

dtm <- DocumentTermMatrix(docs.train, control = list(wordLengths = c(3, 30), bounds = list(global=c(10,Inf), local=c(2, Inf)), weighting = weightTfIdf))

train.d <- as.data.frame(as.matrix(removeSparseTerms(dtm, 0.99)))
train.c.vector <- c(
  rep("ciencia",length(ciencia.train)),
  rep("cultura",length(cultura.train)),
  rep("desporto",length(desporto.train)),
  rep("economia",length(economia.train)),
  rep("mundo",length(mundo.train)),
  rep("musica",length(musica.train)),
  rep("pais",length(pais.train)),
  rep("politica",length(politica.train))
)
train.dc <- cbind(train.d, class=train.c.vector)


lexicon <- names(train.d)
train.dc <- train.dc[, c(which(information.gain(class~., train.dc)$attr_importance > 0), ncol(train.dc))]

test.d <- as.data.frame(as.matrix(DocumentTermMatrix(docs.test, control = list(dictionary = lexicon))))
test.c <- c(
  rep("ciencia",length(ciencia.test)),
  rep("cultura",length(cultura.test)),
  rep("desporto",length(desporto.test)),
  rep("economia",length(economia.test)),
  rep("mundo",length(mundo.test)),
  rep("musica",length(musica.test)),
  rep("pais",length(pais.test)),
  rep("politica",length(politica.test))
)

##################
# classification #
##################

# decision trees
dt.start <- Sys.time()
cat("\tstarting DT", paste(dt.start), "\n")

set.seed(15973)
dtree <- train(train.d, train.c.vector, method = 'rpart')

dt.end <- Sys.time()
cat("\tfinished ", paste(dt.end-dt.start),"\n")

conf.mx.dt <- table(test.c, predict(dtree, test.d))

tp.d <- conf.mx.dt[1,1] # true positives
fp.d <- conf.mx.dt[2,1] # false positives
tn.d <- conf.mx.dt[2,2] # true negatives
fn.d <- conf.mx.dt[1,2] # false negatives

tp.e <- conf.mx.dt[2,2] # true positives
fp.e <- conf.mx.dt[1,2] # false positives
tn.e <- conf.mx.dt[1,1] # true negatives
fn.e <- conf.mx.dt[2,1] # false negatives

error.rate.dt <- (sum(conf.mx.dt)-sum(diag(conf.mx.dt)))/sum(conf.mx.dt)
#error.rate.dt <- (fp + fn) / (tp + tn + fp + fn)

precision.dt.d <- tp.d / (tp.d + fp.d)
precision.dt.e <- tp.e / (tp.e + fp.e)

recall.dt.d <- tp.d / (tp.d + fn.d)
recall.dt.e <- tp.e / (tp.e + fn.e)

f1.dt.d <- 2 * precision.dt.d * recall.dt.d / (precision.dt.d + recall.dt.d)
f1.dt.e <- 2 * precision.dt.e * recall.dt.e / (precision.dt.e + recall.dt.e)

macro.f1.dt   <- (f1.dt.d+f1.dt.e)/2


# k-nearest neighbor
set.seed(15973)
knn <- train(train.d, train.c.vector, method = 'knn')
conf.mx.knn <- table(test.c, predict(knn, test.d))

tp.d <- conf.mx.knn[1,1] # true positives
fp.d <- conf.mx.knn[2,1] # false positives
tn.d <- conf.mx.knn[2,2] # true negatives
fn.d <- conf.mx.knn[1,2] # false negatives


tp.e <- conf.mx.knn[2,2] # true positives
fp.e <- conf.mx.knn[1,2] # false positives
tn.e <- conf.mx.knn[1,1] # true negatives
fn.e <- conf.mx.knn[2,1] # false negatives

error.rate.knn <- (sum(conf.mx.knn)-sum(diag(conf.mx.knn)))/sum(conf.mx.knn)
#error.rate.knn <- (fp + fn) / (tp + tn + fp + fn)

precision.knn.d <- tp.d / (tp.d + fp.d)
precision.knn.e <- tp.e / (tp.e + fp.e)

recall.knn.d <- tp.d / (tp.d + fn.d)
recall.knn.e <- tp.e / (tp.e + fn.e)

f1.knn.d <- 2 * precision.knn.d * recall.knn.d / (precision.knn.d + recall.knn.d)
f1.knn.e <- 2 * precision.knn.e * recall.knn.e / (precision.knn.e + recall.knn.e)

macro.f1.knn  <- (f1.knn.d+f1.knn.e)/2


# naive bayes
set.seed(15973)
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
nbayes <- NB(class ~., train.dc)

conf.mx.nb <- table(test.c, predict(nbayes, test.d))

tp.d <- conf.mx.nb[1,1] # true positives
fp.d <- conf.mx.nb[2,1] # false positives
tn.d <- conf.mx.nb[2,2] # true negatives
fn.d <- conf.mx.nb[1,2] # false negatives


tp.e <- conf.mx.nb[2,2] # true positives
fp.e <- conf.mx.nb[1,2] # false positives
tn.e <- conf.mx.nb[1,1] # true negatives
fn.e <- conf.mx.nb[2,1] # false negatives

error.rate.nb <- (sum(conf.mx.nb)-sum(diag(conf.mx.nb)))/sum(conf.mx.nb)
#error.rate.nb <- (fp + fn) / (tp + tn + fp + fn)

precision.nb.d <- tp.d / (tp.d + fp.d)
precision.nb.e <- tp.e / (tp.e + fp.e)

recall.nb.d <- tp.d / (tp.d + fn.d)
recall.nb.e <- tp.e / (tp.e + fn.e)

f1.dt.d <- 2 * precision.nb.d * recall.nb.d / (precision.nb.d + recall.nb.d)
f1.dt.e <- 2 * precision.nb.e * recall.nb.e / (precision.nb.e + recall.nb.e)

macro.f1.nb   <- (f1.nb.d+f1.nb.e)/2


# neural networks
set.seed(15973)
nnets <- train(train.d, train.c.vector, method = 'nnet')

conf.mx.nn <- table(test.c, predict(nnets, test.d))

tp.d <- conf.mx.nn[1,1] # true positives
fp.d <- conf.mx.nn[2,1] # false positives
tn.d <- conf.mx.nn[2,2] # true negatives
fn.d <- conf.mx.nn[1,2] # false negatives


tp.e <- conf.mx.nn[2,2] # true positives
fp.e <- conf.mx.nn[1,2] # false positives
tn.e <- conf.mx.nn[1,1] # true negatives
fn.e <- conf.mx.nn[2,1] # false negatives

error.rate.nn <- (sum(conf.mx.nn)-sum(diag(conf.mx.nn)))/sum(conf.mx.nn)
#error.rate.nn <- (fp + fn) / (tp + tn + fp + fn)

precision.nn.d <- tp.d / (tp.d + fp.d)
precision.nn.e <- tp.e / (tp.e + fp.e)

recall.nn.d <- tp.d / (tp.d + fn.d)
recall.nn.e <- tp.e / (tp.e + fn.e)

f1.nn.d <- 2 * precision.nn.d * recall.nn.d / (precision.nn.d + recall.nn.d)
f1.nn.e <- 2 * precision.nn.e * recall.nn.e / (precision.nn.e + recall.nn.e)

macro.f1.nn   <- (f1.nn.d+f1.nn.e)/2


# svm with radial kernel
set.seed(565)
svmRad <- train(train.d, train.c.vector, method = 'svmRadial')

conf.mx.svm <- table(test.c, predict(svmRad, test.d))

tp.d <- conf.mx.svm[1,1] # true positives
fp.d <- conf.mx.svm[2,1] # false positives
tn.d <- conf.mx.svm[2,2] # true negatives
fn.d <- conf.mx.svm[1,2] # false negatives


tp.e <- conf.mx.svm[2,2] # true positives
fp.e <- conf.mx.svm[1,2] # false positives
tn.e <- conf.mx.svm[1,1] # true negatives
fn.e <- conf.mx.svm[2,1] # false negatives

error.rate.svm <- (sum(conf.mx.svm)-sum(diag(conf.mx.svm)))/sum(conf.mx.svm)
#error.rate.svm <- (fp + fn) / (tp + tn + fp + fn)

precision.svm.d <- tp.d / (tp.d + fp.d)
precision.svm.e <- tp.e / (tp.e + fp.e)

recall.svm.d <- tp.d / (tp.d + fn.d)
recall.svm.e <- tp.e / (tp.e + fn.e)

f1.svm.d <- 2 * precision.svm.d * recall.svm.d / (precision.svm.d + recall.svm.d)
f1.svm.e <- 2 * precision.svm.e * recall.svm.e / (precision.svm.e + recall.svm.e)

macro.f1.svm  <- (f1.svm.d+f1.svm.e)/2


# svm with linear kernel
set.seed(565)
svmLin2 <- train(train.d, train.c.vector, method = 'svmLinear2')

conf.mx.svm2 <- table(test.c, predict(svmRad, test.d))

tp.d <- conf.mx.svm2[1,1] # true positives
fp.d <- conf.mx.svm2[2,1] # false positives
tn.d <- conf.mx.svm2[2,2] # true negatives
fn.d <- conf.mx.svm2[1,2] # false negatives


tp.e <- conf.mx.svm2[2,2] # true positives
fp.e <- conf.mx.svm2[1,2] # false positives
tn.e <- conf.mx.svm2[1,1] # true negatives
fn.e <- conf.mx.svm2[2,1] # false negatives

error.rate.svm2 <- (sum(conf.mx.svm2)-sum(diag(conf.mx.svm2)))/sum(conf.mx.svm2)
#error.rate.svm2 <- (fp + fn) / (tp + tn + fp + fn)

precision.svm2.d <- tp.d / (tp.d + fp.d)
precision.svm2.e <- tp.e / (tp.e + fp.e)

recall.svm2.d <- tp.d / (tp.d + fn.d)
recall.svm2.e <- tp.e / (tp.e + fn.e)

f1.svm2.d <- 2 * precision.svm2.d * recall.svm2.d / (precision.svm2.d + recall.svm2.d)
f1.svm2.e <- 2 * precision.svm2.e * recall.svm2.e / (precision.svm2.e + recall.svm2.e)

macro.f1.svm2 <- (f1.svm2.d+f1.svm2.e)/2

