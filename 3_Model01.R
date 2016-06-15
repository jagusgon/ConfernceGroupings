path <- getwd()
datadir <- paste(path, '/data/', sep = '')

DF <- read.csv(paste(datadir, "DataBinary.csv", sep = ''), 
                     header = T)

#id is col 1
#Event of interest is col 2-4
#I want to meet is col 5-16
#Participant Industry is col 17
str(DF)

library(recommenderlab)

set.seed(1234)
TrainIndex <- sample(DF$id, 150)
Test <- DF[-TrainIndex,]
Train <- DF[TrainIndex,]
Train <- Train[order(Train$id),]
Test <- Test[order(Test$id),]

TrainMatrix <- data.matrix(Train[,2:16])
TestMatrix <- data.matrix(Test[,2:16])
DFMatrix <- data.matrix(DF[,2:16])
dim(TrainMatrix)
dim(TestMatrix)
dim(DFMatrix)
head(DFMatrix)
head(TrainMatrix)
DF[1,]
TrainMatrix[1,]
DFRatMatrix <- as(DFMatrix, 'binaryRatingMatrix')


b <- as(TrainMatrix, 'binaryRatingMatrix')
hist(getRatings(b))
table(getRatings(b))
image(b)
t <- as(TestMatrix, 'binaryRatingMatrix')
image(t)

r <- Recommender(b, method = 'POPULAR') #Doesn't seem to work well
names(getModel(r))
getModel(r)$topN
recom <- predict(r, t[1], n = 5)
as(recom, 'list')

rAR <- Recommender(b, method = 'AR') #Works well
names(getModel(rAR))
getModel(rAR)
recomAR <- predict(rAR, t[1], n=5)
as(recomAR, 'list')
as(bestN(recomAR, n = 3), 'list')

rUBCF <- Recommender(b, method = 'UBCF') #Doesn't seem to work well
names(getModel(rUBCF))
getModel(rUBCF)
recomUBCF <- predict(rUBCF, t[1], n=5)
as(recomUBCF, 'list')
image(t[1])

rIBCF <- Recommender(b, method = 'IBCF') #Effectively the same as UBCF
names(getModel(rIBCF))
getModel(rIBCF)
recomIBCF <- predict(rIBCF, t[1], n=5)
as(recomIBCF, 'list')
image(t[1])

Test[1,]
Test[2,]
TestMatrix[2,]
recommenderRegistry$get_entry_names()
###This is gettinig somewhere:
recommenderRegistry$get_entries(dataType = 'binaryRatingMatrix')
recommenderRegistry$get_entry("IBCF")
colSums(Train[,2:16])



affinity.matrix<- as(TrainMatrix,"realRatingMatrix")
image(affinity.matrix)
testAffinity <- Recommender(affinity.matrix, method = "POPULAR")
names(getModel(testAffinity))
getModel(testAffinity)$ratings
getModel(testAffinity)$topN
t2 <- as(TestMatrix, "realRatingMatrix")
recom2 <- predict(testAffinity, t2[1], n = 5, type = 'topN')
as(recom2, 'list')
recom3 <- predict(testAffinity, t2[1], n = 5, type = 'ratingMatrix')
as(recom3, 'matrix')

#Evaluation
givenVec <- rowSums(DFMatrix)
e <- evaluationScheme(DFRatMatrix, method = 'split', train = 0.9, given = givenVec)
r1 <- Recommender(getData(e, 'train'), "UBCF")
r2 <- Recommender(getData(e, 'train'), "AR")
p1 <- predict(r1, getData(e, 'known'), type = 'topN')
p2 <- predict(r2, getData(e, 'known'), type = 'topN')

error <- UBCF = calcPredictionAccuracy(p1, getData(e, 'unknown'), given = givenVec)

scheme <- evaluationScheme(DFRatMatrix, method = 'cross', k = 4, given = givenVec)
results <- evaluate(scheme, method = "UBCF", type = "topNList", n = 3)
getConfusionMatrix(results)[[1]]


#Follow guide for binary evaluation
algorithms <- list(
        "random items" = list(name = "RANDOM", param = NULL),
        "popular items" = list(name = "POPULAR", param = NULL),
        "user-based CF" = list(name = "UBCF", param=list(nn=50)),
        "item-based CF" = list(name = "IBCF", param=list(k=50)),
        "association rules" = list(name = 'AR', param=NULL)
)

rowCounts(DFRatMatrix)
DFRatMatrix3 <- DFRatMatrix[rowCounts(DFRatMatrix) > 3]
DFRatMatrix3
#Split evaluation scheme
schemeBinarySplit <- evaluationScheme(DFRatMatrix3, method = 'split', 
                                 train=0.9, k=1, given=3)
resultsBinarySplit <- evaluate(schemeBinarySplit, algorithms,
                          type = 'topNList', n=c(1,3,5,10,15,20))
avg(resultsBinarySplit)
plot(resultsBinarySplit, legend = "bottomright", annotate=c(1,3))
title(main = "Split")
plot(resultsBinarySplit, "prec/rec", annotate = c(1,3,5))
title(main = 'Split')

IBCV_prec_split <- avg(resultsBinarySplit)$'item-based CV'[,5]
IBCV_rec_split <- avg(resultsBinarySplit)$'item-based CV'[,6]
f_IBCV_split <- 2*IBCV_prec_split*IBCV_rec_split/
        (IBCV_prec_split + IBCV_rec_split)


#Cross-validation
schemeBinaryCross <- evaluationScheme(DFRatMatrix3, method = 'cross', 
                                 k=4, given=3)
resultsBinaryCross <- evaluate(schemeBinaryCross, algorithms,
                          type = 'topNList', n=c(1,3,5,10,15,20))
avg(resultsBinaryCross)
plot(resultsBinaryCross, legend = 'bottomright', annotate = c(1,3))
title(main = "Cross-validation")
plot(resultsBinaryCross, "prec/rec", annotate = c(1,3,5))
title(main = "Cross-validation")

AR_prec <- avg(resultsBinaryCross)$'association rules'[,5]
AR_rec <- avg(resultsBinaryCross)$'association rules'[,6]
F_AR <- 2*AR_prec*AR_rec/(AR_prec + AR_rec)

IBCV_prec <- avg(resultsBinaryCross)$'item-based CV'[,5]
IBCV_rec <- avg(resultsBinaryCross)$'item-based CV'[,6]
f_IBCV <- 2*IBCV_prec*IBCV_rec/(IBCV_prec + IBCV_rec)

##
e <- evaluationScheme(DFRatMatrix3, method = 'cross', 
                      k=4, given=3)
Rec_IBCF <- Recommender()