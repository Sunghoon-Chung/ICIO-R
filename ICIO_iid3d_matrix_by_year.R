

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
excel <- "D:/Copy/GVC/ICIO/Excel/"     # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(matlab)
library(stringr)



### Import data from ICIO and assign IDs

icio <- read.csv(paste0(excel,"OECD_ICIO_2011.csv"))
SN.old   <- dim(icio)[1]-2
ciid.old <- as.character(icio$X[1:SN.old])
id.old   <- sapply(strsplit(as.character(icio$X[1:SN.old]), "[_]"), unique)
colnames(id.old) <- ciid.old

cid <- unique(id.old[1,])
iid.old <- unique(str_sub(id.old[2,],-3,-1))



### Import new classification data from ICIO_ciid and apply to ICIO matrix

# Identify original industry positions
irow.old <- str_sub(id.old[2,],-3,-1)            # irow.old = country-matched industry's original row position
irow.old.list <- sapply(iid.old, function(i) which(irow.old==i)) # list of original 34 industries' row positions


# Import new classification file
iid.class <- as.matrix(read.xlsx(paste0(excel,"ICIO_ciid.xlsx"), "ind_class"))
iid.new <- iid.class[,"post"]                    # new classification to apply
iid     <- as.numeric(unique(iid.new))           # the new iid to use
iid.eng <- unique(iid.class[,"ename"])
iid.kr  <- unique(iid.class[,"kname"])


# Apply new classification to old position                  
irow.new <- irow.old                             # irow.new = re-classified industry's row position
for (i in 1:length(iid.old)) irow.new[irow.old.list[[i]]] <- iid.new[i] # replace original iid positions into the new ones 


# Generate new ciid and identify its position in the original data
ciid.new <- paste0(id.old[1,], "_", irow.new)    # ciid.new = new matched country-industry's position
ciid     <- unique(ciid.new)                     # ciid     = the new ciid (order not changed)
SN       <- length(ciid)
ciid.new.list <- sapply(ciid, function(i) which(ciid.new==i)) # list of re-classified ciid's row positions


# construct pre & post matrices for adjustment
post <- zeros(SN.old, SN)                        # post = post adjustment matrix for new ciid
for (j in 1:SN) post[ciid.new.list[[j]],j] <- 1  # assign 1 to the re-classified ciid's row position by column
pre <- t(post)                                   # pre  = pre adjustment matrix for new ciid


# Re-assign identifiers
cid.np  <- unique(substr(cid,1,3))
N       <- length(cid)
N.np    <- length(cid.np)

S       <- length(iid)
SN.np   <- S*N.np
ciid.np <- paste0(rep(cid.np, each=S), "_", iid)

id      <- sapply(strsplit(ciid,"_"), unique)
colnames(id) <- ciid


# Save meta data & remove some noisy data
save(iid, iid.eng, iid.kr, file=paste0(rdata,"ICIO_iid3d_classfication.RData"))  # Save industry classification data
rm(irow.old, irow.old.list, iid.class, irow.new, ciid.new.list)                  # remove noisy data



### Function for generating FDD matrix
fdd_i <- function(M, mat) {             # Take each column of M, diagonalize it, and then adjust (multiply) by mat      
      for (k in 1:dim(M)[2]) {           
            y <- diag(M[,k]) %*% mat    # mat = post (or column) adjust matrix
            if (k==1) z<-y else z<-cbind(z,y)
      }
      return(z)
}



### Compute the values for defined matrices

period <- c(1995,2000,2005,2008,2009,2010,2011)

for(yr in period) {

      icio <- read.csv(paste0(excel,"OECD_ICIO_",yr,".csv"))
      icio <- data.matrix(icio[,2:size(icio)[2]])
      
      y.old <- icio[nrow(icio), 1:SN.old]                   # y = World Output Vector
      y <- as.vector(pre %*% y.old)
      names(y) <- ciid

      va.old <- y.old - colSums(icio[1:SN.old, 1:SN.old])   # va = World VA Vector
      va <- as.vector(pre %*% va.old)
      names(va) <- ciid

      M.old <- icio[1:SN.old, 1:SN.old]                     # M = Intermediate IO Matrix
      M <- pre %*% M.old %*% post
      dimnames(M) <- list(ciid, ciid)
      
      A <- M / (ones(SN,1) %*% y)                           # A = Input Coefficient Matrix
      dimnames(A) <- list(ciid, ciid)
      
      r  <- 1 - colSums(A)                                  # r = Ratio of Value-added to Total Output
      names(r) <- ciid
      
      Leon <- diag(length(ciid)) - A
      LeonInv <- solve(Leon)                                # LeonInv = Leontief Inverse
      
      FD.old <- zeros(SN.old, N.np)                         # FD = Final Demand Matrix for N countries
      FD <- zeros(SN, N.np)
      for(i in 1:N.np) {
            FD.old[, i] <- icio[1:SN.old, (SN.old+1+6*(i-1)):(SN.old+6*i)] %*% ones(6,1)
            FD[, i] <- pre %*% FD.old[, i]
      }
      FD[, N.np] <- FD[, N.np] + pre %*% icio[1:SN.old, ncol(icio)]  # Discrepencies in FD are included to ROW
      dimnames(FD) <- list(ciid, paste0("FD_",cid.np))
      
            
      # Generating FDD (Final Demand Diagonalized) matrix
      MX <- M                                                     # MX = Intermediate good Export Matrix
      dimnames(MX) <- list(ciid,ciid)
      FX <- FD                                                    # FX = Final good Export Matrix
      dimnames(FX) <- list(ciid,cid.np)
      cnum <- 0                                                   # cnum = initial row # in each country
      for (i in 1:N.np) {                                         # i indicates ith country
            nind <- table(substr(id[1,],1,3))[cid.np[i]]          # Number of Industries (including PT) in Country i
            for (j in 1:S) {
                  rnum <- table(id[2,(cnum+1):(cnum+nind)])[j]    # rnum = Number of repetition of row
                  if (j==1) mat<-t(eye(S)[j,])%x%ones(rnum,1) else mat<-rbind(mat,t(eye(S)[j,])%x%ones(rnum,1))
            }                                                     # mat = post-adjust matrix
            FDD.i <- fdd_i(FD[(cnum+1):(cnum+nind),], mat)        # FDD = Partial diagonalization of Final Demand
            if (i==1) FDD <- FDD.i else FDD <- rbind(FDD, FDD.i)
            
            IDD.i <- fdd_i(ones(length((cnum+1):(cnum+nind)), N.np), mat) 
            if (i==1) IDD <- IDD.i else IDD <- rbind(IDD, IDD.i)  # IDD = Partial diagonalized Identity Matrix for later use
            
            MX[(cnum+1):(cnum+nind),(cnum+1):(cnum+nind)] <- 0    # Calculating MX
            FX[(cnum+1):(cnum+nind), i] <- 0                      # Calculating FX
            cnum <- cnum+nind
      }
      dimnames(FDD) <- list(ciid, ciid.np)
      dimnames(IDD) <- list(ciid, ciid.np)
      
      # Save objects
      save(icio,S,N,N.np,SN,SN.np,id,cid,cid.np,iid,iid.kr,iid.eng,ciid,ciid.np,M,A,y,va,r,LeonInv,FD,FDD,IDD,MX,FX, 
           file=paste0(rdata,"ICIO_iid3d_matrix_",yr,".RData"))
      
}

