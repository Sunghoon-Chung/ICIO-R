

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)

load(paste0(rdata,"ICIO_matrix_2011.RData"))          # ICIO_matrix_2011.RData includes all necessary data for analysis


### Set-up ###

# Country's column position in the ICIO matrix
cty <- as.matrix(as.data.frame(strsplit(ciid.np,"_"))[1,])
cty <- as.vector(cty)
SN.np <- S*N.np
names(cty) <- 1:SN.np
chn <- as.numeric(names(subset(cty, cty == "CHN")))
kor <- as.numeric(names(subset(cty, cty == "KOR")))
jpn <- as.numeric(names(subset(cty, cty == "JPN")))
twn <- as.numeric(names(subset(cty, cty == "TWN")))
usa <- as.numeric(names(subset(cty, cty == "USA")))
FD.g.chn <- zeros(SN.np,1)                             # Final Demand Growth in China
countries <- c("kor","jpn","twn","usa")


# Broad Sector Classification: nonondurable, durable, service
ndura <- zeros(34,1)    # nondurable
ndura[c(1:9,18)] <- 1   # 1 = 1% change x 100. Hence, elasticities should be directly read in percentage term.
dura <- zeros(34,1)     # durable
dura[10:17] <- 1
svc <- zeros(34,1)      # service
svc[19:34] <- 1
sectors <- c("ndura","dura","svc")


# Matrix Calculation
Y.alloc <- LeonInv %*% FDD                            # Y.alloc = Output Allocation Matrix
dimnames(Y.alloc) <- list(ciid, ciid.np)              # FDD = Final Demand Diagonalized Matrix

yInv <- zeros(SN,1)
names(yInv) <- ciid
yInv[ciid.nzero] <- 1/y.nzero
OS <- diag(yInv) %*% Y.alloc                          # OS = Output Share Matrix (S matrix in Bems et al. 2010)
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
#ex.growth <- MXS %*% yhat + FXS %*% fd.growth    # ex.growth = Export growth



### Calculating Elasticity by Broad Sectors ###

eps.y <- vector(mode = "list")   # creating an empty list

for (i in sectors) {
      
      if (i == "ndura") {
            FD.g.chn[chn] <- ndura
      }
      else if (i == "dura") {
            FD.g.chn[chn] <- dura            
      }
      else {
            FD.g.chn[chn] <- svc
      }

      # Output & VA Growth rate by country
      yhat <- OS %*% FD.g.chn                       # yhat  = Sector-level Output growth
      vahat <- VAS %*% FD.g.chn                     # vahat = Sector-level VA growth
      
      
      for (j in countries) {
            
            jj <- eval(parse(text = j)) 
            yhat.j <- yhat[jj]                           # yhat.j = Sector-level Output growth in country j
            yhat.j.agg <- sum((y[jj]/sum(y[jj]))*yhat.j) # Aggregate Growth = weighted avg of sector-level growth
            
            # Stack by Column
            if (i == "ndura") {
                  eps.y[[j]] <- c(yhat.j, yhat.j.agg)    # eps.y = Elasticity of Output for each country j      
            }
            else {
                  eps.y[[j]] <- cbind(eps.y[[j]], c(yhat.j, yhat.j.agg))
            }

      }

}


eps.y.all <- as.data.frame(eps.y)   # Convert the list to dataframe
dimnames(eps.y.all) <- list(c(iid,"AGG.Economy"), paste(rep(countries, each = 3), sectors, sep = "_"))


### Save to xlsx file ###
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Note")
addWorksheet(wb, "Output_Elasticity")
writeData(wb, "Output_Elasticity", c("Final Demand Elasticity of Output (Unit: %)"))
writeDataTable(wb, "Output_Elasticity", eps.y.all, startRow = 2, rowNames = TRUE, withFilter = FALSE, 
               tableStyle = "TableStyleMedium9")
saveWorkbook(wb, paste0(excel,"FD_Elasticity.xlsx"), overwrite = TRUE)

