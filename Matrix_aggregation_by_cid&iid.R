

## 2016-07-29

rm(list = ls())                        # Remove all
setwd("D:/KDI/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/KDI/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/KDI/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)
library(WriteXLS)
library(xlsx)


## Import data from ICIO and assign IDs
icio <- read.csv(paste0(excel,"OECD_ICIO_2011.csv"))
SN   <- dim(icio)[1]-2
ciid <- as.character(icio$X[1:SN])
id   <- sapply(strsplit(as.character(icio$X[1:SN]), "[_]"), unique)
colnames(id) <- ciid
cid  <- unique(id[1,])
iid  <- unique(id[2,])

cid.np  <- unique(substr(cid,1,3))
N       <- length(cid)
N.np    <- length(cid.np)
S       <- length(iid)
SN.np   <- S*N.np
ciid.np <- paste0(rep(cid.np, each=S),"_",iid)


## Set-up
period <- c(1995,2011)  # years to calculate
object <- "M"           # specify the objective matrix to aggregate
value <- "nominal"      # whether to deflate in real terms with cpi, "nominal" or "real"
q <- "levels"           # define if values should use "shares" or use "levels"



## Pre- and Post-multiplications for aggregated countries and industries by year

# Country & Industry Grouping
cty_class <- read.xlsx(paste0(excel,"ICIO_ciid_classification.xlsx"), sheetName="cty_class", colIndex=c(1:5))
ind_class <- read.xlsx(paste0(excel,"ICIO_ciid_classification.xlsx"), sheetName="ind_class", colIndex=c(1:5))

agg_pre_cid  <- cty_class$pre
agg_post_cid <- cty_class$post
agg_pre_iid  <- ind_class$pre
agg_post_iid <- ind_class$post


# Pre_multiplication
pre_cid <- sort(unique(agg_pre_cid))
pre_iid <- sort(unique(agg_pre_iid))
pre_n <- length(pre_cid)
pre_s <- length(pre_iid)
pre <- matrix(0, pre_n*pre_s, S*N)
colnames(pre) <- paste0(rep(cid, each=S), "_", iid)

for (c in 1:N) {
      for (i in 1:S) {
            for (cg in 1:pre_n) {
                  for (ig in 1:pre_s) {
                        if (agg_pre_cid[c]==pre_cid[cg] && agg_pre_iid[i]==pre_iid[ig]) {
                              pre[(cg-1)*pre_s+ig,(c-1)*S+i] <- 1
                        }
                  }
            }
      }
}
pre <- pre[,ciid]


# Post_multiplication
post_cid <- sort(unique(agg_post_cid))
post_iid <- sort(unique(agg_post_iid))
post_n <- length(post_cid)
post_s <- length(post_iid)
post <- matrix(0, S*N, post_n*post_s)
rownames(post) <- paste0(rep(cid, each=S), "_", iid)

for (c in 1:N) {
      for (i in 1:S) {
            for (cg in 1:post_n) {
                  for (ig in 1:post_s) {
                        if (agg_post_cid[c]==post_cid[cg] && agg_post_iid[i]==post_iid[ig]) {
                              post[(c-1)*S+i,(cg-1)*post_s+ig] <- 1
                        }
                  }
            }
      }
}
post <- post[ciid,]


# Generate appropriate ciid and create variable names for the aggregated new matrix
pre_ciid <- paste0(rep(pre_cid,each=length(pre_iid)),"_",pre_iid)
post_ciid <- paste0(rep(post_cid,each=length(post_iid)),"_",post_iid)


# Calculate for each year
for (yr in period) {
      load(paste0(rdata,"ICIO_matrix_",yr,".RData"))
      matrix.old <- eval(parse(text=object))
      if (value=="nominal") result <-  pre %*% matrix.old %*% post
      if (value=="real")    result <- (pre %*% matrix.old %*% post) / (cpi[yr-1994]/100)
      colnames(result) <- post_ciid
      matrix.new <- rbind(result, colSums(result))
      matrix.new <- cbind(year=yr, ciid=c(pre_ciid,"total"), matrix.new)
      
      if (yr==1995) matrix_allyr <- matrix.new
      else matrix_allyr <- rbind(matrix_allyr, matrix.new)
}


# Assign filename and store the results
filename <- paste0("Agg_",object,pre_n*pre_s,"x",post_n*post_s, ".xlsx")
write.xlsx(matrix_allyr, paste0("D:/KDI/수시/160729_ASEM/",filename), sheetName=paste0("Agg_",object), row.names=FALSE)




