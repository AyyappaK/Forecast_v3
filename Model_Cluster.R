gc()
#par(mar = rep(2, 4))
source("Data_Design_Cluster.R")
par(mfrow =c(1,1))

# verify by plotting variance of columns
barplot(sapply(pmmd_ctranspose[,2:14], var), horiz=T, las=1, cex.names=0.8)

# column scaling
pmmd_ctranspose_scaled <- cbind(pmmd_ctranspose[,1, drop = F], scale(pmmd_ctranspose[,-1])) %>% 
                           data.frame()
# Variace plot after scaling
barplot(sapply(pmmd_ctranspose_scaled[,2:14], var), horiz=T, las=1, cex.names=0.8)


#screen plot to decide number k in kmean clustering
wss <- (nrow(pmmd_ctranspose_scaled[,2:14])-1)*sum(apply(pmmd_ctranspose_scaled[,2:14],2,var))
for (i in 2:18) wss[i] <- sum(kmeans(pmmd_ctranspose_scaled[,2:14],
                                     centers=i, iter.max = 500)$withinss)
plot(1:18, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 13
kmean_c13 <- kmeans(pmmd_ctranspose_scaled[,3:14],15, iter.max = 1000, nstart = 50)
summary(kmean_c13)
100*kmean_c13$betweenss/kmean_c13$totss
kmean_c13$size
kmean_c13$iter
100*kmean_c13$tot.withinss/kmean_c13$totss

pmmd_ctranspose <- data.frame(pmmd_ctranspose,kmean_c13$cluster) %>% rename("ClusterID" = kmean_c13.cluster)

pmmd_c <- pmmd_c %>% left_join(pmmd_ctranspose %>% select(IdDiff,ClusterID)) #1911600

pmmd_cd <- pmmd_c %>% select(FamilyISBN,ExecutiveReportingTag,CustomTraditionalReporting,PrintDigital,
                            Looseleaf,SaleType,ProgramCopyrightyear,sponsorcode,AreaDescription,SubDivisionDesc,
                            Discipline,ExecutiveRepTagNew,ID,diff,ClusterID) %>% 
          distinct() %>% 
          mutate(ClusterID = as.factor(ClusterID), sponsorcode  = as.factor(sponsorcode),
                 AreaDescription = as.factor(AreaDescription), 
                 SubDivisionDesc = as.factor(SubDivisionDesc),
                 Discipline = as.factor(Discipline),
                 ExecutiveRepTagNew = as.factor(ExecutiveRepTagNew)
                 )
  
pmmd_cd %>% saveRDS('C://RProjects/DataSets/derived_data_2/Forecast_v3/pmmd_ClusterAttributes.rds')
           
pmmd_cd %>% write.csv('C://RProjects/DataSets/derived_data_2/Forecast_v3/pmmd_ClusterAttributes.csv')


#hierarchical Clustering
#h_clust <- Rclusterpp.hclust(pmmd_ctranspose_scaled[,3:14], method="average", distance="euclidean")
#model_cluster <- Mclust(pmmd_ctranspose_scaled[,3:14])
#plot(model_cluster,pmmd_ctranspose_scaled[,3:14], what = "BIC")
