

## 2015-07-20

rm(list = ls())                        # Remove all
library(foreign)
library(matlab)
library(xlsx)


## Set-up
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/excel/"     # Excel data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # R Data Directory

obj <- "M"                                                  # specify the objective matrix csv file to aggregate
cty.class <- read.xlsx(paste0(excel,"ICIO_ciid.xlsx"), 
                           sheetName="cty_class")           # Country Classification
ind.class <- read.xlsx(paste0(excel,"ICIO_ciid.xlsx"), 
                            sheetName="ind_class")          # Industry Classification


# Country Grouping
agg.pre.cid  <- cty.class[,4]
names(agg.pre.cid) <- cty.class[,1]
agg.post.cid <- cty.class[,5]
names(agg.post.cid) <- cty.class[,1]

# Industry Grouping
agg.pre.iid  <- ind.class[,4]
names(agg.pre.iid) <- ind.class[,1]
agg.post.iid <- ind.class[,5]
names(agg.post.iid) <- ind.class[,1]


# Pre_multiplication
pre_cid <- sort(unique(agg.pre.cid))
pre_iid <- sort(unique(agg.pre.iid))
pre_n <- length(pre_cid)
pre_s <- length(pre_iid)
pre <- matrix(0, pre_n*pre_s, 2159)
N <- length(agg.pre.cid)
S <- length(agg.pre.iid)

for (c in 1:N) {
      for (i in 1:S) {
            for (cg in 1:pre_n) {
                  for (ig in 1:pre_s) {
                        if (agg.pre.cid[c]==pre_cid[cg] && agg.pre.iid[i]==pre_iid[ig]) {
                              pre[(cg-1)*pre_s+ig,(c-1)*S+i] <- 1
                        }
                  }
            }
      }
}


# Post_multiplication
post_cid <- sort(unique(agg.post.cid))
post_iid <- sort(unique(agg.post.iid))
post_n <- length(post_cid)
post_s <- length(post_iid)
post <- matrix(0, SN, post_n*post_s)

for (c in 1:N) {
      for (i in 1:S) {
            for (cg in 1:post_n) {
                  for (ig in 1:post_s) {
                        if (agg.post.cid[c]==post_cid[cg] && agg.post.iid[i]==post_iid[ig]) {
                              post[(c-1)*S+i,(cg-1)*post_s+ig] <- 1
                        }
                  }
            }
      }
}

