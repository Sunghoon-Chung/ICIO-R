

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(matlab)
library(openxlsx)


### Set-up ###

# Assign Responding Countries and Variables of interest

cty.rsp <- list("KOR","CHN","DEU","JPN","TWN","USA")  # For other countries, choose them together in this list

iclass <- "iid3d"                                     # Industry classification to apply
load(paste0(rdata,"ICIO_",iclass,"_meta.RData"))      # load industry classification meta data
vars    <- c("y","va","mx","fx","ex")           



# Prepare an excel output file

note <- c(paste0("(1) This file calculates the elasticities of output (y), value added (va), intermediate export (mx), final export (fx) and export (ex) to 1% FD change in the World."),
          "(2) All units are in percentage term.", 
          "(3) 34 original industries in the ICIO table are aggregated to 20 industries as below.",
          "(4) Durable = 22x, Non-durable = 10x & 21x, Utility & Construction = 31x, Service = 32x")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
writeDataTable(wb, "Note", data.frame(iid, iid.eng, iid.kr), startRow=5, withFilter=FALSE)
filename <- paste0("FD_elasticity_",iclass,"_World_PT_adjusted.xlsx")




### Run for Elasticity Calculation

# Load file
load(paste0(rdata,"ICIO_iid3d_matrix_2011.RData")) # These RData include all necessary data for analysis
rm(icio)


# Row positions of CHN's industries & Responding Countries in ICIO matrix
CHN.PRO <- which(substr(ciid, 1, 7) == "CHN.PRO")         # CHN.PRO's position
CHN.NPR <- which(substr(ciid, 1, 7) == "CHN.NPR")         # CHN.NPR's position
CHN.DOM <- which(substr(ciid, 1, 7) == "CHN.DOM")         # CHN.DOM's position

names(cty.rsp) <- cty.rsp
rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position

FD.growth <- rep(1, SN.np)
names(FD.growth) <- ciid.np

# Adjust A matrix
A[-rcty.row$CHN, CHN.PRO] <- A[-rcty.row$CHN, CHN.PRO] * 0.8
A[rcty.row$CHN, CHN.PRO]  <- A[rcty.row$CHN, CHN.PRO] * 1.1
A[-rcty.row$CHN, CHN.DOM] <- A[-rcty.row$CHN, CHN.DOM] * 0.9
A[-rcty.row$CHN, CHN.NPR] <- A[-rcty.row$CHN, CHN.NPR] * 0.9

r.PT <- 1 - colSums(A)                                # r.PT = Ratio of Value-added to Total Output
names(r.PT) <- ciid

Leon <- diag(length(ciid)) - A
LeonInv <- solve(Leon)                                # LeonInv = Leontief Inverse


## Obtain Output & Value-added Share Matrix

Y.alloc <- LeonInv %*% FDD                    # Y.alloc = Output Allocation Matrix
dimnames(Y.alloc) <- list(ciid, ciid.np)      # FDD = Final Demand Diagonalized Matrix

yInv <- 1/y
names(yInv) <- ciid
OS <- diag(yInv) %*% Y.alloc                  # OS = Output Share Matrix (S matrix in Bems et al. 2010)
dimnames(OS) <- list(ciid, ciid.np)

VA.alloc <- diag(r.PT) %*% Y.alloc            # VA.alloc = Value-added Allocation Matrix
dimnames(VA.alloc) <- list(ciid, ciid.np)

vaInv <- 1/(y*r.PT)
names(vaInv) <- ciid
VAS <- diag(vaInv) %*% VA.alloc               # VAS = Value-added Share Matrix
dimnames(VAS) <- list(ciid, ciid.np)


# Obtain Export & Import Share

mx <- rowSums(MX)       # mx (SNx1) = Intermediate Export by ciid (do not include home trade values)
mx[mx==0] <- 0.01       # Set the minimum export equal to 0.01 to avoid zero division
fx <- rowSums(FX)       # fx = Final Export by ciid
fx[fx==0] <- 0.01
ex  <- mx + fx          # ex = Total Export by ciid

MXS <- MX / mx          # MXS = Intermediate Export Share
FX.diag <- NULL         # FX.diag = Final Export diagonalized matrix
for (n in c(1:N.np)) {
      FX.n    <- diag(FX[,n]) %*% IDD[,1:S]
      FX.diag <- cbind(FX.diag, FX.n)
}
FXS <- FX.diag / fx     # FXS = Final Export Share (sum(MXS[i,])+sum(FXS[i,]) = 1 for all i)



### Calculating Elasticity by Broad Sectors & Countries ###

# Country-by-Sector level Growth Rate
yhat  <- OS %*% FD.growth      # yhat (SN by # of broad sectors) = Output growth by ciid
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
      eps.j[[k]] <- cbind(c(ciid[j],paste0(cty.rsp[k],"_TOT")), 
                          eps.y[[k]], eps.va[[k]], eps.mx[[k]], eps.fx[[k]], eps.ex[[k]])
      }



# Convert eps.j to dataframe for each country and save the resutls to the xlsx file

names(eps.j) <- cty.rsp
eps.colname <- c("ciid", paste0("gr_",vars,"_all"))


for (j in cty.rsp) {
      
      result <- as.data.frame(eps.j[[j]])
      colnames(result) <- eps.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Elasticities to 1% Final Demand Change in the World (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

r.CHN <- as.data.frame(t(cbind(ciid[rcty.row$CHN],r[rcty.row$CHN],r.PT[rcty.row$CHN], r.PT[rcty.row$CHN]-r[rcty.row$CHN])))
r.cty <- as.data.frame(t(cbind(iid,r[rcty.row$KOR],r[rcty.row$JPN],r[rcty.row$TWN],r[rcty.row$USA])))
addWorksheet(wb, "Vadd_ratio")
writeDataTable(wb, "Vadd_ratio", r.CHN, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
writeDataTable(wb, "Vadd_ratio", r.cty, startRow=8, withFilter=FALSE, tableStyle="TableStyleMedium9")

saveWorkbook(wb, paste0(excel, filename), overwrite=TRUE)

