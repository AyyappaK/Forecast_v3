# Clusering by taking principal components
pmmd_pr.out <- prcomp(pmmd_ctranspose[,c(-1,-2)], scale = T)
summary(pmmd_pr.out)

pve =100* pmmd_pr.out$sdev ^2/ sum(pmmd_pr.out$sdev ^2)
par(mfrow =c(1,2))
plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",col =" blue")
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="Principal Component ", col =" brown3 ")

