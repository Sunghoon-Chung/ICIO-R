

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(matlab)
library(openxlsx)



### Set-up ###

# Prepare an excel file
note <- c("This file calculates the real growth of output (y), value added (va) and export (ex) due to the real FD change in China & USA.", "All units are in percentage term.")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)


# Sample Period
period <- c(2011)



### Run for each year

#for (yr in period) {
      
# Load file and Import excel data
load(paste0(rdata,"ICIO_matrix_",2011,".RData"))    # This file includes all necessary data for analysis
rm(icio)

FDhat.iid <- read.xlsx(paste0(excel,"FD_2012-2015_estimation.xlsx"), sheet="R_export_CHN", startRow=3)
FDhat3.CHN <- FDhat.iid[,"fdhat3.CHN"]


# Column positions of Source Country & Responding Countries in ICIO matrix
cty.src <- which(substr(ciid.np, 1, 3)=="CHN")                            # Source country = China
cty.rsp <- list("KOR","DEU","JPN","TWN","USA")                            # Responding countries
names(cty.rsp) <- cty.rsp
cty.pos <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty))   # Country's column position list.


# Sector Classification: nonondurable, durable, utilies & construction, service
sectors <- c("ndura","dura","ucon","svc","all")
FD.scl  <- zeros(S, length(sectors))
FD.scl[c(1:9,18),1] <- FDhat3.CHN[c(1:9,18)]     # Non-durable
FD.scl[c(10:17) ,2] <- FDhat3.CHN[c(10:17)]      # Durable
FD.scl[c(19,20) ,3] <- FDhat3.CHN[c(19,20)]      # Utilities & Construction
FD.scl[c(21:34) ,4] <- FDhat3.CHN[c(21:34)]      # Service
FD.scl[,5]          <- FDhat3.CHN                # All Industries

FD.growth <- as.data.frame(zeros(SN.np, length(sectors)))
dimnames(FD.growth) <- list(ciid.np, sectors)
FD.growth[cty.src,] <- FD.scl


# Obtain Output & Value-added Share Matrix

Y.alloc <- LeonInv %*% FDD                    # Y.alloc = Output Allocation Matrix
dimnames(Y.alloc) <- list(ciid, ciid.np)      # FDD = Final Demand Diagonalized Matrix

yInv <- zeros(SN,1)
names(yInv) <- ciid
yInv[ciid.nzero] <- 1/y.nzero
OS <- diag(yInv) %*% Y.alloc                  # OS = Output Share Matrix (S matrix in Bems et al. 2010)
dimnames(OS) <- list(ciid, ciid.np)

VA.alloc <- diag(r) %*% Y.alloc               # VA.alloc = Value-added Allocation Matrix
dimnames(VA.alloc) <- list(ciid, ciid.np)

vaInv <- zeros(SN,1)
names(vaInv) <- ciid
vaInv[ciid.nzero] <- 1/va.nzero
VAS <- diag(vaInv) %*% VA.alloc               # VAS = Value-added Share Matrix
dimnames(VAS) <- list(ciid, ciid.np)


# Obtain Export & Import Share

cols   <- list()
for (c in cid.np) {
      cols[[c]] <- ifelse(substr(id[1,],1,3)==c, 1, 0)
}

ex  <- rowSums(MX) + rowSums(FX)                        # Total Export by ciid (MX & FX do not include home trade values)
ex[ex==0] <- 0.01                                       # Set the minimum export equal to 0.01 to avoid zero division
mx.cid <- rowSums(MX) %*% as.matrix(data.frame(cols))   # Intermediate Export by cid
fx.cid <- rowSums(FX) %*% as.matrix(data.frame(cols))   # Final Export by cid
ex.cid <- mx.cid + fx.cid                               # Total Export by cid
MXS <- MX / ex                                          # MXS = Intermediate Export Share
FX.diag <- NULL                                         # FX.diag = Final Export diagonalized matrix
for (n in c(1:N.np)) {
      FX.n    <- diag(FX[,n]) %*% IDD[,1:S]
      FX.diag <- cbind(FX.diag, FX.n)
}
FXS <- FX.diag / ex                                     # FXS = Final Export Share (sum(MXS[i,])+sum(FXS[i,]) = 1 for all i)



### Calculating Elasticity by Broad Sectors & Countries ###

# Country-by-Sector level Growth Rate
yhat  <- lapply(FD.growth, function(x) OS %*% x)      # yhat (SN by # of sectors) = Output growth by ciid
vahat <- lapply(FD.growth, function(x) VAS %*% x)     # vahat(SN by # of sectors) = VA growth by ciid
exhat <- sapply(yhat, function(x) MXS%*%x) + sapply(FD.growth, function(y) FXS%*%y)    # exhat = Export growth by ciid

yhat  <- as.data.frame(yhat)
vahat <- as.data.frame(vahat)
exhat <- as.data.frame(exhat)      

yhat.j  <- lapply(cty.rsp, function(j) yhat[cty.pos[[j]],])   # yhat.j  = Sector-level Output growth for each country j
vahat.j <- lapply(cty.rsp, function(j) vahat[cty.pos[[j]],])  # vahat.j = Sector-level VA growth
exhat.j <- lapply(cty.rsp, function(j) exhat[cty.pos[[j]],])  # exhat.j = Sector-level Export growth


# Obtain Aggregate Growth Rates

eps.y  <- list()        # Elasticity of output
eps.va <- list()        # Elasticity of VA
eps.ex <- list()        # Elasticity of export

for (k in 1:length(cty.rsp)) {

      j <- cty.pos[[k]]     # Country j's column position

      # Aggregate Growth Rate = weighted avg of sector-level growth rate
      yhat.j.agg  <- colSums((y[j]/sum(y[j]))*yhat.j[[k]])
      vahat.j.agg <- colSums((va[j]/sum(va[j]))*vahat.j[[k]])  
      exhat.j.agg <- colSums((ex[j]/sum(ex[j]))*exhat.j[[k]])
      
      # Stack by Column
      eps.y[[k]]  <- rbind(yhat.j[[k]],  yhat.j.agg)     # Elasticity of Output for country j
      eps.va[[k]] <- rbind(vahat.j[[k]], vahat.j.agg)    # Elasticity of VA for country j
      eps.ex[[k]] <- rbind(exhat.j[[k]], exhat.j.agg)    # Elasticity of Export for country j 
      
}


# Convert the list to dataframe
eps.y.all  <- as.data.frame(eps.y)   
eps.va.all <- as.data.frame(eps.va)
eps.ex.all <- as.data.frame(eps.ex) 

dimnames(eps.y.all)  <- list(c(iid,"AGG.Economy"), paste("y", rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
dimnames(eps.va.all) <- list(c(iid,"AGG.Economy"), paste("va",rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
dimnames(eps.ex.all) <- list(c(iid,"AGG.Economy"), paste("ex",rep(cty.rsp, each=length(sectors)), sectors, sep="_"))


# Save to the xlsx file 
for (k in c("y", "va", "ex")) {     
      
      addWorksheet(wb, paste("growth",k, sep="_"))     # k = variable of interest, i = source country of shock
      
      writeData(wb, paste("growth",k, sep="_"), 
            paste0("Real Growth of ",k," w.r.t. Actual FD Change in China (Unit: %)"))
      
      writeDataTable(wb, paste("growth",k, sep="_"), eval(as.name(paste0("eps.",k,".all"))), 
            startRow=2, rowNames=TRUE, withFilter=FALSE, tableStyle="TableStyleMedium9")
}


saveWorkbook(wb, paste0(excel,"FD_Real_Growth_Effect.xlsx"), overwrite=TRUE)


