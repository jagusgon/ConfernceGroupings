library(fpc)
library(xlsx)
source('AddUnderscores.R')
source('SpreadResponses.R')
path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
Data <- read.csv(paste(datadir, "Smart_Summit_All_Data.csv", sep = ''),
                 header = T, na.strings = '')
Data <- Data[rowSums(is.na(Data)) != ncol(Data),]

# Changed from first version...now only grouping on interest in further learning
DF <- Data[,30:35]
#names(DF)[c(3,5)] <- c("OtherCompany", "OtherValueInMeeting")

removeBlanks <- function(df){
        df[, colSums(is.na(df)) != nrow(df)]
}

# remove columns that contain only NA
DF <- removeBlanks(DF)
# Put underscores between words, but not between answers
DF <- AddUnderscores(DF)
# Spread all answers into a binary dataframe
DFSpread <- SpreadResponses(DF)

# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
DFSpread <- DFSpread[ , !(names(DFSpread) %in% drops)]

KMclustering <- kmeans(DFSpread, 50, nstart = 1)
plotcluster(DFSpread, KMclustering$cluster)
KMclustering
KMclustering$withinss

Delegates <- Data[,c(5,6,9)] #FirstName, Surname, email
Delegates$group <- KMclustering$cluster
#write.xlsx(Delegates, 'groups.xlsx', row.names = F)


table(DFSpread$cluster)

# See where the delegates with missing survey data are clustering
DFSpread$cluster <- KMclustering$cluster
rowSums(DFSpread[, -106])
colSums(DFSpread[rowSums(DFSpread[,-106]) == 1,])



# Diagnostics
#############################################
getTop10Facotrs <- function(data, clust){
        group <- data[data$cluster == clust, -28]
        head(sort((colSums(group)*100/nrow(data)), decreasing = T), n=10)
}

One <- getTop10Facotrs(DFSpread, 1)
Three <- getTop10Facotrs(DFSpread, 3)
seventeen <- getTop10Facotrs(DFSpread, 17)
names(Three) %in% names(One)
table(Three)
names(seventeen)

Groups <- numeric()
for(i in 1:50){
        Groups[i] <- nrow(DFSpread[DFSpread$cluster == i,])
}
boxplot(Groups[Groups != 320])


library(datasets)
head(iris)
Iris <- data("iris")
IrisKM <- kmeans(iris[, 3:4], 3, nstart = 10)
IrisKM
 
library(cluster)
clusplot(DFSpread, KMclustering$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
library(fpc)
plotcluster(DFSpread, KMclustering$cluster)
pamClustering <- pam(DFSpread, 10)
pamClustering
summary(pamClustering)
plot(pamClustering)
plotcluster(DFSpread, pamClustering$clustering)

cluster.stats(DFSpread, KMclustering$cluster, pamClustering$clustering)
length(KMclustering$cluster)
length(pamClustering$clustering)
table(KMclustering$cluster, pamClustering$clustering)

fannyClustering <- fanny(DFSpread, 10)

silhouette(pamClustering)
claraClustering <- clara(DFSpread, 10)
plot(claraClustering)

install.packages('clusteval')
library(clusteval)
clusteval(KMclustering)
jaccard_indep( KMclustering$cluster, pamClustering$clustering)

##############
Prin_Comp <- prcomp(DFSpread, scale. = T)
