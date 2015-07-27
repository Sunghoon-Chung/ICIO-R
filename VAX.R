

# 2015-07-18
# VAX Estimation

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)
library(xlsx)


## ICIO includes 62 countries (plus 5 processing ID) with 34 industries over 1995-2011
period <- c(1995,2000,2005,2008,2010,2011)


for (yr in period) {
      
      load(paste0(rdata,"ICIO_matrix_",yr,".RData"))

      
      # Generating Domestic & Foreign FD matrix
      cty <- zeros(1,N.np)
      names(cty) <- cid.np
      for (i in cid.np) {                                      # i indicates ith country
            cty[i] <- 1
            if (i=="MEX") {
                  cty.ind <- t(cty) %x% ones(50,1)      
            }
            else if (i=="CHN") {
                  cty.ind <- t(cty) %x% ones(69,1)
            }
            else {
                  cty.ind <- t(cty) %x% ones(34,1)
            }
            if (i=="AUS") d <- cty.ind else d <- rbind(d, cty.ind)
            cty[i] <- 0
      }
      dimnames(d) <- list(ciid,cid.np)
      f <- 1-d
      
      
      ## Calculate Final Demand and Output for Each Country
      dimnames(FD) <- list(ciid, paste0("FD_",cid.np))
      FD.df <- cbind(FD, "FD_d"=rowSums(FD*d), "FD_f"=rowSums(FD*f), "FD"=rowSums(FD))  # domestic & foreign FD
      Y <- LeonInv %*% FD                       # Y = Output Matrix for N countries
      dimnames(Y) <- list(ciid, cid.np)
      
      
      ## Calculate Value-Added Exports
      VAX <- Y * r %*% matrix(1,1,N.np)
      dimnames(VAX) <- list(ciid, paste0("VAX_",cid.np))
      GDP <- rowSums(VAX)
      VAX.df <- cbind(VAX, "VAX"=rowSums(VAX*f))   # Domestic VAX is substracted
      
      
      ## Calculate Intermediate Good Export (MX) & Gross Exports (GX)
      MX <- M %*% d
      dimnames(MX) <- list(ciid, paste0("MX_",cid.np))
      MX.df <- cbind(MX, "MX"=rowSums(MX*f))    # Domestic MX is subtracted
      
      GX <- MX + FD                             # GX = Gross Exports
      dimnames(GX) <- list(ciid, paste0("GX_",cid.np))
      GX.df <- cbind(GX, "GX"=rowSums(GX*f))    # Domestic GX is subtracted
      
      
      ## Calculate VAX ratio (element by element divison)
      # NA = 0/0
      # Inf = positive/0
      VAX.ratio <- VAX.df/GX.df
      dimnames(VAX.ratio) <- list(ciid, c(paste0("VAX_ratio_",cid.np),"VAX_ratio"))
      
      
      # vax file comprises Final Demand, VAX, GX, and VAX ratio.
      VAX.yr <- cbind(year=yr, ciid=ciid, FD.df, VAX.df, GDP, MX.df, GX.df, VAX.ratio)
      save(FD.df, VAX.df, GDP, MX.df, GX.df, VAX.ratio, file=paste0("VAX",yr,".RData"))
      if(yr==1995) VAX.allyr <- VAX.yr else VAX.allyr <- rbind(VAX.allyr, VAX.yr)
      
}

write.csv(VAX.allyr, paste0(excel,"VAX_OECD.csv"), row.names=FALSE)
