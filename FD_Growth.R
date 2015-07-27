
# Function for FD change analysis: generating some matrices

FD_Growth <- function(base.year, fd.growth) {

      load(paste0(rdata,"ICIO_matrix_",base.year,".RData"))
      
      Y.alloc <- LeonInv %*% FDD                            # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid, ciid.np)
      
      VA.alloc <- diag(r) %*% Y.alloc                       # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid, ciid.np)
      
      yInv <- zeros(SN,1)
      names(yInv) <- ciid
      yInv[ciid.nzero] <- 1/y.nzero
      OS <- diag(yInv) %*% Y.alloc                          # OS = Output Share Matrix
      dimnames(OS) <- list(ciid, ciid.np)
      y.growth <- OS %*% fd.growth                          # y.growth = Output growth
      
      vaInv <- zeros(SN,1)
      names(vaInv) <- ciid
      vaInv[ciid.nzero] <- 1/va.nzero
      VAS <- diag(vaInv) %*% VA.alloc                       # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid, ciid.np)
      va.growth <- VAS %*% fd.growth                        # va.growth = VA growth
      
      EX <- rowSums(MX, FX)                                 # Total Export by ciid
      MXS <- MX/EX                                          # Intermediate Export Share by ciid
      FXS <- FX/EX                                          # Final Export Share by ciid
      ex.growth <- MXS %*% y.growth + FXS %*% fd.growth     # ex.growth = Export growth
      
      return(y.growth, vs.growth, ex.growth)
}

