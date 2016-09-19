# PCA
str(DFSpread)
Prin_Comp <- prcomp(DFSpread, scale. = T)
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
plot(Prin_Comp$rotation[,1], Prin_Comp$rotation[,2], type = 'p')

PC <- Prin_Comp$rotation[,1:74] # 95% of variance


DF_PC <- as.data.frame(as.matrix(DFSpread) %*% PC)
head(DF_PC)
kmPC <- kmeans(DF_PC, 10)
DF5$PCAcluster <- kmPC$cluster
DF5[,116:117]
table(DF5$cluster, DF5$PCAcluster)
plot(DF5$cluster, DF5$PCAcluster, jitter(DF5$cluster))
plot(DF_PC$PC1, DF_PC$PC2, col = kmPC$cluster, pch = 20)
plot(DF_PC$PC1, DF_PC$PC2, col = km$cluster, pch = 20)

table(kmPC$cluster, pamClustering$clustering)
kmPC
silhouette(kmPC)
plotcluster(DFSpread, kmPC$cluster)
# Kmeans clustering
km <- kmeans(DFSpread, 10)
km$cluster
DF5 <- DFSpread
DF5$cluster <- km$cluster
DF5[,104:106]

getGroupProfiles <- function(df){
        Result <- data.frame()
        for(i in 1:10){
                temp <- df[df$PCAcluster == i,]
                MeanVector <- as.matrix(colMeans(temp))
                Result <- cbind(Result, MeanVector[,1])
        }
        return(Result)
}

test <- getGroupProfiles(DF5)

Group1 <- DF5[DF5$PCAcluster == 1,]
colSums(Group1)
as.matrix(colSums(Group1))
Group2 <- DF5[DF5$PCAcluster == 2,]
as.matrix(colSums(Group2))
cbind(as.matrix(colSums(Group1)), as.matrix(colSums(Group2)))

library(dplyr)
Groups <- group_by(DF5, PCAcluster) 
library(tidyr)
DF6 <- DF5[,-116]
DF7 <- gather(DF6, answer, value, -PCAcluster)%>%
        group_by(PCAcluster)%>%
        summarise(Mean = colMeans())

# Heatmaps
DF5_Matrix <- data.matrix(DF4)
Heatmap <- heatmap(DF5_Matrix)
heatmap(data.matrix(Group1))
