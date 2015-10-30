

## 2015-10-30

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)
library(xlsx)

## Some Countries by Sectors
chn <- c(1293:1326)
kor <- c(613:646)
jpn <- c(579:612)
FD.g.chn <- zeros(2108,1)

#Set-up
nondura <- zeros(34,1)
nondura[1:9] <- 1
nondura[18] <- 1

dura <- zeros(34,1)
dura[10:17] <- 1

service <- zeros(34,1)
service[19:34] <- 1


load(paste0(rdata,"ICIO_matrix_2011.RData"))


Y.alloc <- LeonInv %*% FDD                            # Y.alloc = Output Allocation Matrix
dimnames(Y.alloc) <- list(ciid, ciid.np)

yInv <- zeros(SN,1)
names(yInv) <- ciid
yInv[ciid.nzero] <- 1/y.nzero
OS <- diag(yInv) %*% Y.alloc                          # OS = Output Share Matrix
dimnames(OS) <- list(ciid, ciid.np)

VA.alloc <- diag(r) %*% Y.alloc                       # VA.alloc = Value-added Allocation Matrix
dimnames(VA.alloc) <- list(ciid, ciid.np)

vaInv <- zeros(SN,1)
names(vaInv) <- ciid
vaInv[ciid.nzero] <- 1/va.nzero
VAS <- diag(vaInv) %*% VA.alloc                       # VAS = Value-added Share Matrix
dimnames(VAS) <- list(ciid, ciid.np)

# Export Growth
EX <- rowSums(MX, FX)                                 # Total Export by ciid
MXS <- MX/EX                                          # Intermediate Export Share by ciid
FXS <- FX/EX                                          # Final Export Share by ciid
#ex.growth <- MXS %*% y.growth + FXS %*% fd.growth     # ex.growth = Export growth


for (i in c("nondura","dura","service")) {
      
      if (i=="nondura") {
            FD.g.chn[chn] <- nondura
      }
      else if (i=="dura") {
            FD.g.chn[chn] <- dura            
      }
      else {
            FD.g.chn[chn] <- service
      }

      # Output growth
      y.growth <- OS %*% FD.g.chn                           # y.growth = Output growth
      y.growth.kor <- y.growth[kor]
      names(y.growth.kor) <- iid
      y.growth.kor.agg <- sum((y[kor]/sum(y[kor]))*y.growth.kor)

      # VA growth
      va.growth <- VAS %*% FD.g.chn                         # va.growth = VA growth
      va.growth.kor <- va.growth[kor]
      names(va.growth.kor) <- iid

      if (i=="nondura") {
            kor.all <- c(y.growth.kor,y.growth.kor.agg)
      }
      else {
            kor.all <- cbind(kor.all, c(y.growth.kor,y.growth.kor.agg))
      }
}


write.xlsx2(kor.all, paste0(excel,"China_FD_growth_effect.xlsx", sheetName="FD_growth"))

