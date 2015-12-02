

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(openxlsx)

### Step 1. 

period <- c(2008,2009,2010,2011)   # Sample Period should cover from the base-years to the years of interest

for (yr in period) {
      load(paste0(rdata,"ICIO_iid3d_matrix_",yr,".RData"))    # These RData include all necessary data for analysis
      if (yr==period[1]) FD.allyr <- cbind("year"=yr, "ciid"=ciid, FD) 
      else FD.allyr <- rbind(FD.allyr, cbind("year"=yr, "ciid"=ciid, FD))
}

write.dta(as.data.frame(FD.allyr), "D:/Copy/GVC/ICIO/Stata/FD_by_cid.dta", convert.factors = "string")



### Step 2. run the folloiwng do files

# If you need to re-estimate the growth rate of FD in CHN, KOR or USA, run FD_estimation_XXX.do
# Then, run FDhat_World_PWT.do



### Step 3. 

rm(list = ls())
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(openxlsx)

period <- c(2008,2009,2010)  # Sample Period should be the base-years to use
cty.rsp <- list("KOR","CHN","DEU","JPN","TWN","USA")  # For other countries, choose them together in this list

iclass <- "iid3d"                                     # Industry classification to apply
load(paste0(rdata,"ICIO_",iclass,"_meta.RData"))      # load industry classification meta data
vars   <- c("y","va","mx","fx","ex")



## Import estimate of FD Growth rate

d.year <- paste0(period[2]-period[1], "-year") 
FDhat <- read.dta("D:/Copy/GVC/ICIO/Stata/FDhat_World.dta")
ciid.ord <- FDhat[,1]    # ciid row position
FDhat <- FDhat[,2:(2+length(period)-1)]
rownames(FDhat) <- ciid.ord
FDhat <- FDhat[ciid.np,] # change order of ciid according to ICIO Table

# change some countries' FD growth rate
KOR <- paste0("KOR_",iid)
CHN <- paste0("CHN_",iid)
USA <- paste0("USA_",iid)
FDhat[KOR,] <- read.xlsx(paste0(excel,"FD_estimation_KOR.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat[CHN,] <- read.xlsx(paste0(excel,"FD_estimation_CHN.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat[USA,] <- read.xlsx(paste0(excel,"FD_estimation_USA.xlsx"), sheet=paste0(d.year,"_FD_growth"), 
                         cols=c(1:(length(period)+1)), rowNames=TRUE, startRow=2)
FDhat <- as.list(FDhat)

names(FDhat) <- period
FDhat.name  <- list("FDhat2009_08", "FDhat2010_09", "FDhat2011_10")
names(FDhat.name) <- period



## Prepare an excel file

note <- c(paste0("(1) This file calculates the real growth of output (y), value added (va), intermediate export (mx), final export (fx), and total export (ex) due to the real FD change in the World."),
          "(2) All units are in percentage term.", 
          "(3) 34 original industries in the ICIO table are aggregated to 17 industries as below.",
          "(4) Durable = 22x, Non-durable = 10x & 21x, Utility & Construction = 31x, Service = 32x")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
writeDataTable(wb, "Note", data.frame(iid, iid.eng, iid.kr), startRow=5, withFilter=FALSE)
filename <- paste0("FD_Real_Growth_Effect_",iclass,"_World_",d.year,".xlsx")


# Prepare a List to save calculated elasticities by responding country & year
eps.j.yr <- list()      



### Run for each year

for (yr in period) {
      
      # Load file
      if (yr>2011) load(paste0(rdata,"ICIO_",iclass,"_matrix_2011.RData"))
      else load(paste0(rdata,"ICIO_",iclass,"_matrix_",yr,".RData")) # These RData include all necessary data for analysis
      rm(icio)
      
      
      # Row positions of Source Country & Responding Countries in ICIO matrix
      names(cty.rsp) <- cty.rsp
      rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position
      
      
      ## Insert sector-level FD growth rate
      
      FD.growth <- FDhat[[as.character(yr)]]
      names(FD.growth) <- ciid.np

      
      ## Obtain Output & Value-added Share Matrix
      
      Y.alloc <- LeonInv %*% FDD                    # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid, ciid.np)      # FDD = Final Demand Diagonalized Matrix
      
      yInv <- zeros(SN,1)
      names(yInv) <- ciid
      yInv <- 1/y
      OS <- diag(yInv) %*% Y.alloc                  # OS = Output Share Matrix (S matrix in Bems et al. 2010)
      dimnames(OS) <- list(ciid, ciid.np)
      
      VA.alloc <- diag(r) %*% Y.alloc               # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid, ciid.np)
      
      vaInv <- zeros(SN,1)
      names(vaInv) <- ciid
      vaInv <- 1/va
      VAS <- diag(vaInv) %*% VA.alloc               # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid, ciid.np)
      
      
      
      ## Obtain Export & Import Share
      
      mx <- rowSums(MX)       # mx (SNx1) = Intermediate Export by ciid (do not include home trade values)
      mx[mx==0] <- 0.01       # Set the minimum export equal to 0.01 to avoid zero division
      fx <- rowSums(FX)       # fx = Final Export by ciid
      fx[fx==0] <- 0.01
      ex  <- mx + fx          # ex = Total Export by ciid
      
      MXS <- MX / ex          # MXS = Intermediate Export Share
      FX.diag <- NULL         # FX.diag = Final Export diagonalized matrix
      for (n in c(1:N.np)) {
            FX.n    <- diag(FX[,n]) %*% IDD[,1:S]
            FX.diag <- cbind(FX.diag, FX.n)
      }
      FXS <- FX.diag / ex     # FXS = Final Export Share (sum(MXS[i,])+sum(FXS[i,]) = 1 for all i)
      
      
      # We only calculate country-level import
      #       cols   <- list()
      #       for (c in cid.np) {
      #             cols[[c]] <- ifelse(substr(id[1,],1,3)==c, 1, 0)
      #       }
      #       mm.cid   <- colSums(MX) %*% as.matrix(data.frame(cols)) # Intermediate Import by cid
      #       im.cid   <- mm.cid + colSums(FX)                        # Total Import by cid
      #       freq.cid <- table(substr(id[1,],1,3))
      #       freq.cid <- freq.cid[cid.np]                            # Keep country ordering same as the ICIO table
      #       IM  <- ones(SN,1) %*% t(rep(im.cid, freq.cid))          # IM = Total Import by cid in Matrix form
      #       MMS <- MX / IM                                          # Intermediate Import Share
      #       FMS <- FX / (ones(SN,1) %*% im.cid)                     # Final Import Share
      
      
      
      
      ### Calculating Elasticity by Broad Sectors & Countries ###
      
      # Country-by-Sector level Growth Rate
      
      yhat  <- OS %*% FD.growth      # yhat (SN by # of sectors) = Output growth by ciid
      vahat <- VAS %*% FD.growth     # vahat = VA growth
      mxhat <- MXS %*% yhat          # mxhat = intermediate export growth
      fxhat <- FXS %*% FD.growth     # fxhat = final export growth
      
      yhat  <- as.data.frame(yhat)
      vahat <- as.data.frame(vahat)
      mxhat <- as.data.frame(mxhat)      
      fxhat <- as.data.frame(fxhat)
      exhat <- mx/ex*mxhat + fx/ex*fxhat                    # exhat = Total Export growth
      
      yhat.j  <- lapply(cty.rsp, function(j) yhat[rcty.row[[j]],])  # yhat.j = Sector-level Output growth for country j
      vahat.j <- lapply(cty.rsp, function(j) vahat[rcty.row[[j]],])
      mxhat.j <- lapply(cty.rsp, function(j) mxhat[rcty.row[[j]],]) 
      fxhat.j <- lapply(cty.rsp, function(j) fxhat[rcty.row[[j]],]) 
      exhat.j <- lapply(cty.rsp, function(j) exhat[rcty.row[[j]],]) 
      
      
      # Obtain Aggregate Growth Rates
      
      eps.y  <- list()        # Elasticity of output
      eps.va <- list()        # Elasticity of VA
      eps.mx <- list()        # Elasticity of intermediate export
      eps.fx <- list()        # Elasticity of final export
      eps.ex <- list()        # Elasticity of total export
      eps.j  <- list()        # All Elasticities above by Responding country
      
      for (k in 1:length(cty.rsp)) {
            
            j <- rcty.row[[k]]     # Country's row position
            
            # Aggregate Growth Rate = weighted avg of sector-level growth rate
            yhat.j.agg  <- sum((y[j]/sum(y[j]))*yhat.j[[k]])
            vahat.j.agg <- sum((va[j]/sum(va[j]))*vahat.j[[k]])
            mxhat.j.agg <- sum((mx[j]/sum(mx[j]))*mxhat.j[[k]])
            fxhat.j.agg <- sum((fx[j]/sum(fx[j]))*fxhat.j[[k]])
            exhat.j.agg <- sum((ex[j]/sum(ex[j]))*exhat.j[[k]])
            
            # Stack sector growth rate and aggregate growth rate under in a row
            eps.y[[k]]  <- c(yhat.j[[k]],  "AGG.Economy"=yhat.j.agg)   # Elasticity of Output for country j
            eps.va[[k]] <- c(vahat.j[[k]], "AGG.Economy"=vahat.j.agg)  # Elasticity of VA
            eps.mx[[k]] <- c(mxhat.j[[k]], "AGG.Economy"=mxhat.j.agg)  # Elasticity of intermediate export
            eps.fx[[k]] <- c(fxhat.j[[k]], "AGG.Economy"=fxhat.j.agg)  # Elasticity of final export 
            eps.ex[[k]] <- c(exhat.j[[k]], "AGG.Economy"=exhat.j.agg)  # Elasticity of total export
            
            # Combine all elasticities by responding country
            eps.j[[k]] <- cbind(FDhat.name[[as.character(yr)]], c(ciid[j],paste0(cty.rsp[k],"_TOT")), 
                                eps.y[[k]], eps.va[[k]], eps.mx[[k]], eps.fx[[k]], eps.ex[[k]])
            
            # Stack by year
            if (yr==period[1]) eps.j.yr[[k]] <- eps.j[[k]]   else eps.j.yr[[k]] <- rbind(eps.j.yr[[k]], eps.j[[k]])
      }
      
}


# Convert eps.j.yr to dataframe for each country and save the resutls to the xlsx file

names(eps.j.yr) <- cty.rsp
eps.colname <- c("year", "ciid", paste0("gr_",vars,"_all"))

for (j in cty.rsp) {
      
      result <- as.data.frame(eps.j.yr[[j]])
      colnames(result) <- eps.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Real Growth Rate due to Final Demand Change in the World (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

saveWorkbook(wb, paste0(excel, filename), overwrite=TRUE)

