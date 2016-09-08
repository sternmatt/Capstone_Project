setwd("~/Documents/Big_data/project/Data/data")
install.packages("data.table")
install.packages("dplyr")
install.packages("reshape")
install.packages("Matrix")
install.packages("gamlr")

library(dplyr)
library(data.table)
library(reshape)
library(Matrix)
library(gamlr)


#Load Data
carFiles <- dir(path = ".", pattern="*Carrier*", full.names = TRUE, recursive = TRUE)
outFiles <- dir(path = ".", pattern="*Out*", full.names = TRUE, recursive = TRUE)

totals <- data.frame(matrix(ncol=2,nrow=0))

for (i in 1:length(carFiles)) {
	system.time(claims <-  fread(carFiles[i],na.strings=""))
    claims <- as.data.frame(claims)
    system.time(counts <- melt(claims[,c("CLM_ID",names(claims)[grepl("HCPCS_CD",names(claims))])],id.vars="CLM_ID",na.rm=TRUE))
	counts <- group_by(counts,value)
	counts <- summarize(counts, count = n())
	totals <- rbind(totals,counts)
}

carTotals <- totals
totals <- data.frame(matrix(ncol=2,nrow=0))

for (i in 1:length(outFiles)) {
	system.time(claims <-  fread(outFiles[i],na.strings=""))
	claims <- as.data.frame(claims)
    system.time(counts <- melt(claims[,c("CLM_ID",names(claims)[grepl("HCPCS_CD",names(claims))])],id.vars="CLM_ID",na.rm=TRUE))
	counts <- group_by(counts,value)
	counts <- summarize(counts, count = n())
	totals <- rbind(totals,counts)
}

outTotals <- totals

save(carTotals, file = "carTotals.Rda")
save(outTotals, file = "outTotals.Rda")

load("carTotals.Rda")
load("outTotals.Rda")

carTotals <- group_by(carTotals, value)
carTotals <- summarize(carTotals, sums = sum(count))
outTotals <- group_by(outTotals, value)
outTotals <- summarize(outTotals, sums = sum(count))
Totals <- merge(carTotals[carTotals$sums >= quantile(carTotals$sums)[3],],outTotals[outTotals$sums >= quantile(outTotals$sums)[3],], by = "value", all = FALSE)
Totals <- Totals[order(-Totals$sums.y),]

system.time(claims <-  fread("DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv",na.strings=""))
system.time(newclaims <-  fread("DE1_0_2008_to_2010_Carrier_Claims_Sample_1A.csv",na.strings=""))

nms_overlap <- names(claims)[which(names(claims) %in% names(newclaims))]

claims <- as.data.frame(claims)


claims <- claims[,c("DESYNPUF_ID","CLM_ID","CLM_FROM_DT","CLM_THRU_DT",names(claims)[grepl("ICD9_DGNS_CD",names(claims))],names(claims)[grepl("HCPCS_CD",names(claims))])]

nms <- c(names(claims)[grepl("ICD9_DGNS_CD",names(claims))],names(claims)[grepl("HCPCS_CD",names(claims))])
claims[,nms] <- lapply(claims[,nms],as.factor)

nms <- names(claims)[grepl("HCPCS_CD",names(claims))]

claims <- claims[which(claims[,nms[1]]  %in% Totals$value),]

for (i in 2:length(nms)) {
	claims <- claims[which(claims[,nms[i]]  %in% c(NA,Totals$value) ),]
}

nms <- c(names(claims)[grepl("ICD9_DGNS_CD",names(claims))],names(claims)[grepl("HCPCS_CD",names(claims))])
nms <- setdiff(nms,c("HCPCS_CD_45"))

fList <- lapply(nms,reformulate,intercept=FALSE)
mList <- lapply(fList,sparse.model.matrix,data=data.frame(claims[,nms]))
do.call(cBind,mList)
