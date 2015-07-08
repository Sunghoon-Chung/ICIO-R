

## 2015-07-08
rm(list = ls())    # Remove all
setwd("D:/Copy/GVC/ICIO/R")
rawd <- "D:/Copy/GVC/ICIO/"    # Raw data Directory

library(foreign)
library(matlab)
library(xlsx)


## ICIO includes 62 countries (plus 5 processing ID) with 34 industries over 1995-2011
period <- c(2011)


## Import data from ICIO and assign IDs
icio <- read.csv(paste0(rawd,"OECD_ICIO_June2015_2011.csv"))
SN   <- dim(icio)[1]-2
ciid <- as.character(icio$X[1:SN])
id   <- sapply(strsplit(as.character(icio$X[1:SN]), "[_]"), unique)
colnames(id) <- ciid
cid  <- unique(id[1,])
iid  <- unique(id[2,])

cid.np  <- unique(substr(cid,1,3))
N <- length(cid)
N.np <- length(cid.np)
S <- length(iid)
ciid.np <- paste0(rep(cid.np, each=S),"_",iid)

save(cid, iid, ciid, SN, file="icio_meta.Rdata")
#writeMat(icio_meta.mat, cid=cid, iid=iid, ciid=ciid, SN=SN)


## Function for generating FDD matrix
fdd_i <- function(fd, mat) {
      for (k in 1:dim(fd)[2]) {
            y <- diag(fd[,k]) %*% mat
            if (k==1) z<-y else z<-cbind(z,y)
      }
      return(z)
}


## Compute the values for defined matrices

for(yr in period) {
      icio <- read.csv(paste0(rawd,"OECD_ICIO_June2015_",yr,".csv"))
      icio <- data.matrix(icio[,2:size(icio)[2]])
      
      y <- icio[nrow(icio), 1:SN]                           # y = World Output Vector
      names(y) <- ciid
      y.nzero <- y[y>0]
      ciid.nzero <- names(y.nzero)
            
      va <- y - colSums(icio[1:SN, 1:SN])                   # va = World VA Vector
      names(va) <- ciid
      va.nzero <- va[va>0]
      
      M <- icio[1:SN, 1:SN]                                 # M = Intermediate IO Matrix
      dimnames(M) <- list(ciid, ciid)
      
      A <- zeros(SN, SN)                                    # A = Input Coefficient Matrix
      dimnames(A) <- list(ciid, ciid)
      A[, ciid.nzero] <- M[, ciid.nzero] / (ones(SN,1) %*% y.nzero)
      
      r  <- 1 - colSums(A)                                  # r = Ratio of Value-added to Total Output
      names(r) <- ciid
      
      Leon <- diag(length(ciid)) - A
      LeonInv <- solve(Leon)                                # LeonInv = Leontief Inverse

      FD <- zeros(SN, N.np)                                 # FD = Final Demand Matrix for N countries
      dimnames(FD) <- list(ciid, cid.np)
      for(i in 1:N.np) {
            FD[, i] <- icio[1:SN, (SN+1+6*(i-1)):(SN+6*i)] %*% ones(6,1)
      }
      FD[, N.np] <- FD[, N.np] + icio[1:SN, ncol(icio)]
      
      
      # FDD = Partial diagonalization of Final Demand with Processing Trade
      MX <- M                                                  # MX = Intermediate good Export Matrix
      dimnames(MX) <- list(ciid,ciid)
      FX <- FD                                                 # FX = Final good Export Matrix
      dimnames(FX) <- list(ciid,cid.np)
      cnum <- 0                                                # cnum = initial row # in each country
      for (i in 1:N.np) {                                      # i indicates ith country
            nind <- table(substr(id[1,],1,3))[cid.np[i]]       # Number of Industries (including PT) in Country i
            for (j in 1:S) {
                  rnum <- table(id[2,(cnum+1):(cnum+nind)])[j] # rnum = Number of repetition of row
                  if (j==1) mat<-t(eye(S)[j,])%x%ones(rnum,1) else mat<-rbind(mat,t(eye(S)[j,])%x%ones(rnum,1))
            }
            FDD.i <- fdd_i(FD[(cnum+1):(cnum+nind),], mat)
            if (i==1) FDD <- FDD.i else FDD <- rbind(FDD, FDD.i)
            MX[(cnum+1):(cnum+nind),(cnum+1):(cnum+nind)] <- 0 # Calculating MX
            FX[(cnum+1):(cnum+nind), i] <- 0                   # Calculating FX
            cnum <- cnum+nind
      }
      dimnames(FDD) <- list(ciid, ciid.np)
      
      
      Y.alloc <- LeonInv %*% FDD                            # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid, ciid.np)
      
      VA.alloc <- diag(r) %*% Y.alloc                       # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid, ciid.np)
      
      yInv <- zeros(SN,1)
      names(yInv) <- ciid
      yInv[ciid.nzero] <- 1/y.nzero
      OS <- diag(yInv) %*% Y.alloc                          # OS = Output Share Matrix
      dimnames(OS) <- list(ciid, ciid.np)
      
      vaInv <- zeros(SN,1)
      names(vaInv) <- ciid
      vaInv[ciid.nzero] <- 1/va.nzero
      VAS <- diag(vaInv) %*% VA.alloc                       # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid, ciid.np)

      MX <- M - eye()
      dimnames(MX) <- list(ciid,ciid)
      
      
      
      # Save objects
      save(icio,S,N,N.np,SN,id,cid,cid.np,iid,ciid,ciid.np,M,A,y,va,r,LeonInv,FD,FDD,Y.alloc,VA.alloc,OS,VAS, 
           file=paste0("ICIO_matrix_",yr,".RData"))
}


