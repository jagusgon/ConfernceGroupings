# Use this script to put people into groups based on similarities 
# in what they want to learn about

source('AddUnderscores.R')
source('SpreadResponses.R')
path <- getwd()
datadir <- paste(path, '/data/', sep = '')

#Load and clean data
Data2 <- read.csv(paste(datadir, "Smart_Summit_Exact_Data_Export.csv", sep = ''),
                  header = T)

DF2 <- Data2[,22:39]
names(DF2)[c(3,5)] <- c("OtherCompany", "OtherValueInMeeting")

removeBlanks <- function(df){
        df[, colSums(is.na(df)) != nrow(df)]
}

# remove columns that contain only NA
DF2 <- removeBlanks(DF2)
# Put underscores between words, but not between answers
DF2 <- AddUnderscores(DF2)

DFLearn <- DF2[,c(12, 14, 15)]

#Spread responses
DFLearnSprd <- SpreadResponses(DFLearn)
names(DFLearnSprd)

# Remove columns with no predictive power
drops <- c('Other', 'na', 'N/A', '---_please_select_---')
DFLearnSprd <- DFLearnSprd[, !names(DFLearnSprd) %in% drops]

#Kmeans clustering
KLearn <- kmeans(DFLearnSprd, 10)
KLearn$cluster
DFLearnSprd$cluster <- KLearn$cluster
colSums(DFLearnSprd[DFLearnSprd$cluster == 1,])

# PCA
Prin_Comp <- prcomp(DFLearnSprd, scale. = T)
Prin_Comp$center
Prin_Comp$scale
Prin_Comp$rotation[1:5,1:4]
dim(Prin_Comp$x)
biplot(Prin_Comp, scale = 0)
std_dev <- Prin_Comp$sdev
pr_var <- std_dev^2
pr_var[1:10]
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:10]
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
prop_varex[prop_varex >= 0.02]
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
cumsum(prop_varex)
plot(Prin_Comp$rotation[,1], Prin_Comp$rotation[,2], type = 'p', fill=DFLearnSprd$cluster)

# gets 95% of the variability
PC <- Prin_Comp$rotation[,1:18]


DF_PC <- as.data.frame(as.matrix(DFLearnSprd) %*% PC)
head(DF_PC)
kmPC <- kmeans(DF_PC, 10)
DFLearnSprd$PCAcluster <- kmPC$cluster
DFLearnSprd[,23:24]
table(DFLearnSprd$cluster, DFLearnSprd$PCAcluster)
c <- 9
as.matrix(colSums(DFLearnSprd[DFLearnSprd$cluster == c,]))

plot(DF_PC$PC1, DF_PC$PC2, col = DFLearnSprd$PCAcluster, pch = 20, cex=3)
#how can I assess the goodness of fit for clustering?