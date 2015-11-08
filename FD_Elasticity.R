

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)

load(paste0(rdata,"ICIO_matrix_2011.RData"))          # ICIO_matrix_2011.RData includes all necessary data for analysis
rm(icio)


### Set-up ###

# Country's column position in the ICIO matrix
cty <- as.matrix(as.data.frame(strsplit(ciid.np,"_"))[1,])
cty <- as.vector(cty)
names(cty) <- 1:SN.np
CHN <- as.numeric(names(subset(cty, cty=="CHN")))
KOR <- as.numeric(names(subset(cty, cty=="KOR")))
JPN <- as.numeric(names(subset(cty, cty=="JPN")))
TWN <- as.numeric(names(subset(cty, cty=="TWN")))
USA <- as.numeric(names(subset(cty, cty=="USA")))


# Source Country of Shocks and Responding Countries
cty.srce  <- "CHN"                        
cty.resp  <- c("KOR","JPN","TWN","USA")   # Responding countries


# Broad Sector Classification: nonondurable, durable, service
ndura <- zeros(34,1)    # nondurable
ndura[c(1:9,18)] <- 1   # 1 = 1% change x 100. Hence, elasticities should be directly read in percentage term.
dura <- zeros(34,1)     # durable
dura[10:17] <- 1
svc <- zeros(34,1)      # service
svc[19:34] <- 1
sectors <- c("ndura","dura","svc")


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

cols   <- vector(mode = "list")
for (c in cid.np) {
      cols[[c]] <- ifelse(substr(id[1,],1,3)==c, 1, 0)
}

ex     <- rowSums(MX) + rowSums(FX)                     # Total Export by ciid (MX & FX do not include home trade values)
mx.cid <- rowSums(MX) %*% as.matrix(data.frame(cols))   # Intermediate Export by cid
fx.cid <- rowSums(FX) %*% as.matrix(data.frame(cols))   # Final Export by cid
x.cid  <- mx.cid + fx.cid                               # Total Export by cid
MXS <- MX / ex                                          # Intermediate Export Share 
FXS <- FX / ex                                          # Final Export Share -------> sum(MXS[i,])+sum(FXS[i,]) = 1 for all i 

# We only calculate country-level import
mm.cid   <- colSums(MX) %*% as.matrix(data.frame(cols)) # Intermediate Import by cid
im.cid   <- mm.cid + colSums(FX)                        # Total Import by cid
freq.cid <- table(substr(id[1,],1,3))
freq.cid <- freq.cid[cid.np]                            # Keep country ordering same as the ICIO table
IM  <- ones(SN,1) %*% t(rep(im.cid, freq.cid))          # IM = Total Import by cid in Matrix form
MMS <- MX / IM                                          # Intermediate Import Share
FMS <- FX / (ones(SN,1) %*% im.cid)                     # Final Import Share



### Calculating Elasticity by Broad Sectors & Countries ###

# creating an empty lists
eps.y  <- vector(mode="list")                           
eps.va <- vector(mode="list")
eps.ex <- vector(mode="list")
eps.im <- vector(mode="list")

for (s in sectors) {

      FD.growth <- zeros(SN.np,1)                                 # Final Demand Growth vector
      FD.growth[eval(as.name(cty.srce))] <- eval(as.name(s))

      # Country-by-Sector level Growth Rate
      yhat  <- OS  %*% FD.growth                                  # yhat  (SNx1) = Output growth by ciid
      vahat <- VAS %*% FD.growth                                  # vahat (SNx1) = VA growth by ciid
      exhat  <- MXS %*% yhat + FXS[,cty.srce]*(IDD %*% FD.growth) # exhat (SNx1) = Export growth by ciid

      
      # Obtain Aggregate Growth Rates
      for (j in cty.resp) {
            
            jj <- eval(as.name(j))                                # Evaluate text as variable name
            
            # Calculate Aggregate Growth Rate
            yhat.j      <- yhat[jj]                               # yhat.j = Sector-level Output growth in country j
            yhat.j.agg  <- sum((y[jj]/sum(y[jj]))*yhat.j)         # Aggregate Growth = weighted avg of sector-level growth
            
            vahat.j     <- vahat[jj]                              # vahat.j = Sector-level VA growth in country j
            vahat.j.agg <- sum((va[jj]/sum(va[jj]))*vahat.j)                    
            
            exhat.j     <- exhat[jj]                              # exhat.j = Sector-level Output growth in country j
            exhat.j.agg <- sum((ex[jj]/sum(ex[jj]))*exhat.j)

            # Stack by Column
            eps.y[[j]]  <- cbind(eps.y[[j]],  c(yhat.j,yhat.j.agg))   # eps.y  = elasticity of Output for country j
            eps.va[[j]] <- cbind(eps.va[[j]], c(vahat.j,vahat.j.agg)) # eps.va = elasticity of VA for country j
            eps.ex[[j]] <- cbind(eps.ex[[j]], c(exhat.j,exhat.j.agg)) # eps.ex = elasticity of Export for country j 
            
      }
}

eps.y.all  <- as.data.frame(eps.y)   # Convert the list to dataframe
eps.va.all <- as.data.frame(eps.va)
eps.ex.all <- as.data.frame(eps.ex) 

dimnames(eps.y.all)  <- list(c(iid, "AGG.Economy"), paste("y", rep(cty.resp, each=3), sectors, sep="_"))
dimnames(eps.va.all) <- list(c(iid, "AGG.Economy"), paste("va", rep(cty.resp, each=3), sectors, sep="_"))
dimnames(eps.ex.all) <- list(c(iid, "AGG.Economy"), paste("ex", rep(cty.resp, each=3), sectors, sep="_"))



### Save to xlsx file ###

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Note")

for (k in c("Output", "VA", "Export")) {
      addWorksheet(wb, k)
      writeData(wb, k, paste0("Final Demand Elasticity of ", k, " (Unit: %)"))
}

writeDataTable(wb, "Output",  eps.y.all, startRow=2, rowNames=TRUE, withFilter=FALSE, tableStyle="TableStyleMedium9")
writeDataTable(wb, "VA",     eps.va.all, startRow=2, rowNames=TRUE, withFilter=FALSE, tableStyle="TableStyleMedium9")
writeDataTable(wb, "Export", eps.ex.all, startRow=2, rowNames=TRUE, withFilter=FALSE, tableStyle="TableStyleMedium9")

saveWorkbook(wb, paste0(excel,"FD_Elasticity.xlsx"), overwrite = TRUE)

