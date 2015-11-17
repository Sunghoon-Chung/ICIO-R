

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(matlab)
library(openxlsx)


### Set-up ###

# Assign Sample period, Source Country, Responding Countries, and Sector classification

period <- c(1995,2000,2005,2008,2009,2010,2011)

cty.src <- "CHN"                                  # Source country should be a single country
cty.rsp <- list("CHN")                            # China or Mexico should be chosen separately due to processing trade
cty.rsp <- list("KOR","DEU","JPN","TWN","USA")    # For other countries, choose them together in this list
sectors <- c("ndura","dura","ucon","svc","all")   # Sectors are classified based on durables vs. non-durables


# Prepare an excel output file

note <- c("This file calculates the elasticities of output (y), value added (va) and export (ex) due to the real FD change in China & USA.", "All units are in percentage term.")

wb <- createWorkbook()
addWorksheet(wb, "Note")
writeData(wb, "Note", note)
filename <- paste0("FD_elasticty_to_",cty.src,".xlsx")



### Run for Elasticity Calculation

for (yr in period) {
      
      # Load file
      load(paste0(rdata,"ICIO_matrix_",yr,".RData"))    # These RData include all necessary data for analysis
      rm(icio)
      

      # Row positions of Source Country & Responding Countries in ICIO matrix
      scty.row <- which(substr(ciid.np, 1, 3) == cty.src)                     # Source country's row position
      names(cty.rsp) <- cty.rsp
      rcty.row <- lapply(cty.rsp, function(cty) which(substr(ciid,1,3)==cty)) # Responding countries' row position
      
      
      # Sector Classification: nonondurable, durable, utilies & construction, service
      FD.sector <- zeros(S, length(sectors))
      FD.sector[c(1:9,18),1] <- 1      # Non-durable
      FD.sector[c(10:17) ,2] <- 1      # Durable
      FD.sector[c(19,20) ,3] <- 1      # Utilities & Construction
      FD.sector[c(21:34) ,4] <- 1      # Service
      FD.sector[,5]          <- 1      # All Industries
      
      FD.growth <- as.data.frame(zeros(SN.np, length(sectors)))
      dimnames(FD.growth) <- list(ciid.np, sectors)
      FD.growth[scty.row,] <- FD.sector
      
      
      ## Obtain Output & Value-added Share Matrix
      
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
      yhat  <- lapply(FD.growth, function(x) OS %*% x)      # yhat (SN by # of sectors) = Output growth by ciid
      vahat <- lapply(FD.growth, function(x) VAS %*% x)     # vahat = VA growth
      mxhat <- lapply(yhat,      function(x) MXS %*% x)     # mxhat = intermediate export growth
      fxhat <- lapply(FD.growth, function(y) FXS %*% y)     # fxhat = final export growth

      yhat  <- as.data.frame(yhat)
      vahat <- as.data.frame(vahat)
      mxhat <- as.data.frame(mxhat)      
      fxhat <- as.data.frame(fxhat)
      exhat <- mxhat + fxhat                                # exhat = Total Export growth
      
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
      
      for (k in 1:length(cty.rsp)) {
            
            j <- rcty.row[[k]]     # Country j's row position
            
            # Aggregate Growth Rate = weighted avg of sector-level growth rate
            yhat.j.agg  <- colSums((y[j]/sum(y[j]))*yhat.j[[k]])
            vahat.j.agg <- colSums((va[j]/sum(va[j]))*vahat.j[[k]])  
            mxhat.j.agg <- colSums((mx[j]/sum(mx[j]))*mxhat.j[[k]])
            fxhat.j.agg <- colSums((fx[j]/sum(fx[j]))*fxhat.j[[k]])
            exhat.j.agg <- colSums((ex[j]/sum(ex[j]))*exhat.j[[k]])
            
            # Stack by Column
            eps.y[[k]]  <- rbind(yhat.j[[k]],  yhat.j.agg)     # eps.y = Elasticity of Output for country j
            eps.va[[k]] <- rbind(vahat.j[[k]], vahat.j.agg)    # Elasticity of VA
            eps.mx[[k]] <- rbind(mxhat.j[[k]], mxhat.j.agg)    # Elasticity of intermediate export
            eps.fx[[k]] <- rbind(fxhat.j[[k]], fxhat.j.agg)    # Elasticity of final export 
            eps.ex[[k]] <- rbind(exhat.j[[k]], exhat.j.agg)    # Elasticity of total export
            
      }
      
      
      # Convert the list to dataframe
      eps.y.all  <- as.data.frame(eps.y)   
      eps.va.all <- as.data.frame(eps.va)
      eps.mx.all <- as.data.frame(eps.mx) 
      eps.fx.all <- as.data.frame(eps.fx) 
      eps.ex.all <- as.data.frame(eps.ex) 
      
      dimnames(eps.y.all)  <- list(c(iid,"AGG.Economy"), paste("y", rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
      dimnames(eps.va.all) <- list(c(iid,"AGG.Economy"), paste("va",rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
      dimnames(eps.mx.all) <- list(c(iid,"AGG.Economy"), paste("mx",rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
      dimnames(eps.fx.all) <- list(c(iid,"AGG.Economy"), paste("fx",rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
      dimnames(eps.ex.all) <- list(c(iid,"AGG.Economy"), paste("ex",rep(cty.rsp, each=length(sectors)), sectors, sep="_"))
      
            
      # Save to the xlsx file
      for (k in c("y","va","mx","fx","ex")) {
            
            sheet <- paste("e", k, yr, sep="_")    # sheetname
            addWorksheet(wb, sheet)                         # k = variables of interest
            
            writeData(wb, sheet, 
                      paste0("Elasticity of ", k, " to Final Demand Change in ", cty.src, " (Unit: %)"))
            
            writeDataTable(wb, sheet, eval(as.name(paste0("eps.",k,".all"))), 
                           startRow=2, rowNames=TRUE, withFilter=FALSE, tableStyle="TableStyleMedium9")
      }
      
}

saveWorkbook(wb, paste0(excel, filename), overwrite=TRUE)

