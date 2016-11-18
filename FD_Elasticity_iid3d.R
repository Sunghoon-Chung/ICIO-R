

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(matlab)
library(openxlsx)


### Set-up ###

# Assign Sample period, Source Country, Responding Countries, Sector classification, and Variables of interest

period <- c(1995,2000,2005,2008,2009,2010,2011)       # Sample Period
cty.src <- "CHN"                                      # Source country should be a single country
cty.rsp <- list("KOR","CHN","DEU","JPN","TWN","USA")  # For other countries, choose them together in this list

iclass <- "iid3d"                                     # Industry classification to apply
load(paste0(rdata,"ICIO_",iclass,"_meta.RData"))      # load industry classification meta data
sectors <- c("ndura","dura","ucon","svc","all")       # Sectors are classified based on durables vs. non-durables
vars    <- c("y","va","mx","fx","ex")           



# Prepare an excel output file

note <- c(paste0("(1) This file calculates the elasticities of output (y), value added (va), intermediate export (mx), final export (fx) and export (ex) to the real FD change in ",cty.src,"."),
          "(2) All units are in percentage term.", 
          "(3) 34 original industries in the ICIO table are aggregated to 20 industries as below.",
          "(4) Durable = 22x, Non-durable = 10x & 21x, Utility & Construction = 31x, Service = 32x")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
writeDataTable(wb, "Note", data.frame(iid, iid.eng, iid.kr), startRow=5, withFilter=FALSE)
filename <- paste0("FD_elasticity_",iclass,"_",cty.src,".xlsx")



# Prepare a List to save calculated elasticities by responding country & year
eps.j.yr <- list()      



### Run for Elasticity Calculation

for (yr in period) {
      
      # Load file
      load(paste0(rdata,"ICIO_iid3d_matrix_",yr,".RData"))    # These RData include all necessary data for analysis
      rm(icio)
      

      # Row positions of Source Country & Responding Countries in ICIO matrix
      scty.row <- which(substr(ciid.np, 1, 3) == cty.src)                     # Source country's row position
      names(cty.rsp) <- cty.rsp
      rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position
      
      
      # Sector Classification: nonondurable, durable, utilies & construction, service
      FD.sector <- zeros(S,length(sectors))
      colnames(FD.sector) <- sectors
      FD.sector[,1] <- ifelse(as.integer(iid/10)<=21, 1, 0)      # Non-durable
      FD.sector[,2] <- ifelse(as.integer(iid/10)==22, 1, 0)      # Durable
      FD.sector[,3] <- ifelse(as.integer(iid/10)==31, 1, 0)      # Utilities & Construction
      FD.sector[,4] <- ifelse(as.integer(iid/10)==32, 1, 0)      # Service
      FD.sector[,5] <- 1                                         # All Industries
      
      FD.growth <- as.data.frame(zeros(SN.np, length(sectors)))
      dimnames(FD.growth) <- list(ciid.np, sectors)
      FD.growth[scty.row,] <- FD.sector
      
      
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
      FXS <- FX.diag / fx     # FXS = Final Export Share

      
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
      yhat  <- lapply(FD.growth, function(x) OS %*% x)      # yhat (SN by # of broad sectors) = Output growth by ciid
      vahat <- lapply(FD.growth, function(x) VAS %*% x)     # vahat = VA growth
      mxhat <- lapply(yhat,      function(x) MXS %*% x)     # mxhat = intermediate export growth
      fxhat <- lapply(FD.growth, function(x) FXS %*% x)     # fxhat = final export growth

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
            yhat.j.agg  <- colSums((y[j]/sum(y[j]))*yhat.j[[k]])
            vahat.j.agg <- colSums((va[j]/sum(va[j]))*vahat.j[[k]])  
            mxhat.j.agg <- colSums((mx[j]/sum(mx[j]))*mxhat.j[[k]])
            fxhat.j.agg <- colSums((fx[j]/sum(fx[j]))*fxhat.j[[k]])
            exhat.j.agg <- colSums((ex[j]/sum(ex[j]))*exhat.j[[k]])
            
            # Stack sector growth rate and aggregate growth rate under in a row
            eps.y[[k]]  <- rbind(yhat.j[[k]],  "AGG.Economy"=yhat.j.agg)   # eps.y = Elasticity of Output for country j
            eps.va[[k]] <- rbind(vahat.j[[k]], "AGG.Economy"=vahat.j.agg)  # Elasticity of VA
            eps.mx[[k]] <- rbind(mxhat.j[[k]], "AGG.Economy"=mxhat.j.agg)  # Elasticity of intermediate export
            eps.fx[[k]] <- rbind(fxhat.j[[k]], "AGG.Economy"=fxhat.j.agg)  # Elasticity of final export 
            eps.ex[[k]] <- rbind(exhat.j[[k]], "AGG.Economy"=exhat.j.agg)  # Elasticity of total export
            
            # Combine all elasticities by responding country and stack by year
            eps.j[[k]] <- cbind(yr, c(ciid[j],paste0(cty.rsp[k],"_TOT")),
                                eps.y[[k]], eps.va[[k]], eps.mx[[k]], eps.fx[[k]], eps.ex[[k]])
            if (yr==period[1]) eps.j.yr[[k]] <- eps.j[[k]]   else eps.j.yr[[k]] <- rbind(eps.j.yr[[k]], eps.j[[k]])
      }
}


# Convert eps.j.yr to dataframe for each country and save the resutls to the xlsx file

names(eps.j.yr) <- cty.rsp
eps.colname <- c("year", "ciid", paste("e",rep(vars, each=length(sectors)),sectors,sep="_"))

for (j in cty.rsp) {
      
      result <- as.data.frame(eps.j.yr[[j]])
      colnames(result) <- eps.colname
      addWorksheet(wb, j)
      writeData(wb, j, paste0(j, "'s Elasticities to Final Demand Change in ", cty.src, " (Unit: %)"))
      writeDataTable(wb, j, result, startRow=2, withFilter=FALSE, tableStyle="TableStyleMedium9")
}

saveWorkbook(wb, paste0(excel, filename), overwrite=TRUE)

