

## 2015-07-16

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
rawd  <- "D:/Copy/GVC/ICIO/"           # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)
library(xlsx)

## ICIO includes 62 countries (plus 5 processing ID) with 34 industries over 1995-2011
period <- c(1995,2000,2005,2010,2011)

## Some Countries by Sectors
chn <- c(1309:1377)
kor <- c(613:646)
jpn <- c(579:612)

for (yr in period) {
      load(paste0(rdata,"ICIO_matrix_",yr,".RData"))
      M.chn     <- M[1:length(ciid), chn]
      M.chn.chn <- colSums(M.chn[chn,])
      M.chn.kor <- colSums(M.chn[kor,])
      M.chn.jpn <- colSums(M.chn[jpn,])
      M.chn.row <- colSums(M.chn)-M.chn.chn-M.chn.kor-M.chn.jpn
      M.chn.agg <- rbind(M.chn.row, M.chn.jpn, M.chn.chn, M.chn.kor) / (ones(4,1)%*%colSums(M.chn)) * 100
      M.chn.agg <- cbind(yr, M.chn.agg)
      if (yr==1995) M.china <- M.chn.agg   else M.china <- rbind(M.china, M.chn.agg)
}
