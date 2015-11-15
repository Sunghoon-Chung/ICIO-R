

# Function for FD change analysis: generating growth rates of output, VA and export by ciid

FD_Growth <- function(base.year, FD.growth) {
      
      library(matlab)
      load(paste0(rdata,"ICIO_matrix_",base.year,".RData"))
      
      # Output Growth
      Y.alloc <- LeonInv %*% FDD                             # Y.alloc = Output Allocation Matrix
      dimnames(Y.alloc) <- list(ciid, ciid.np)
      
      yInv <- zeros(SN,1)
      names(yInv) <- ciid
      yInv[ciid.nzero] <- 1/y.nzero
      OS <- diag(yInv) %*% Y.alloc                           # OS = Output Share Matrix
      dimnames(OS) <- list(ciid, ciid.np)
      yhat  <- OS  %*% FD.growth                             # yhat (SNx1) = Output growth by ciid
      
      # VA growth
      VA.alloc <- diag(r) %*% Y.alloc                        # VA.alloc = Value-added Allocation Matrix
      dimnames(VA.alloc) <- list(ciid, ciid.np)
      
      vaInv <- zeros(SN,1)
      names(vaInv) <- ciid
      vaInv[ciid.nzero] <- 1/va.nzero
      VAS <- diag(vaInv) %*% VA.alloc                        # VAS = Value-added Share Matrix
      dimnames(VAS) <- list(ciid, ciid.np)
      vahat <- VAS %*% FD.growth                             # vahat (SNx1) = VA growth by ciid
      
      # Export Growth
      cols   <- vector(mode = "list")
      for (c in cid.np) {
            cols[[c]] <- ifelse(substr(id[1,],1,3)==c, 1, 0)
      }
      
      ex  <- rowSums(MX) + rowSums(FX)                       # Total Export by ciid (MX & FX don't include home trade values)
      ex[ex==0] <- 0.01                                      # Set the minimum export equal to 0.01 to avoid zero division
      mx.cid <- rowSums(MX) %*% as.matrix(data.frame(cols))  # Intermediate Export by cid
      fx.cid <- rowSums(FX) %*% as.matrix(data.frame(cols))  # Final Export by cid
      ex.cid <- mx.cid + fx.cid                              # Total Export by cid
      MXS <- MX / ex                                         # MXS = Intermediate Export Share
      FX.diag <- NULL                                        # FX.diag = Final Export diagonalized matrix
      for (n in c(1:N.np)) {
            FX.n    <- diag(FX[,n]) %*% IDD[,1:S]
            FX.diag <- cbind(FX.diag, FX.n)
      }
      FXS <- FX.diag / ex                                    # FXS = Final Export Share (sum(MXS[i,])+sum(FXS[i,]) = 1)
      exhat <- MXS %*% yhat + FXS %*% FD.growth              # exhat (SNx1) = Export growth by ciid
      
      
      return(yhat, vshat, exhat)
}

