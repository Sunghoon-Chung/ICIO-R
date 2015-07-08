


## 2015-07-09

rm(list = ls())                        # Remove all
setwd("D:/Copy/GVC/ICIO/Rcode")        # Working Directory
rawd  <- "D:/Copy/GVC/ICIO/"           # Excel Raw data Directory
rdata <- "D:/Copy/GVC/ICIO/Rdata/"     # RData Saving Directory

library(foreign)
library(matlab)
library(xlsx)

## ICIO includes 62 countries (plus 5 processing ID) with 34 industries over 1995-2011
period <- c(1995,2000,2005,2008,2009,2010,2011)


load()

# Matrices for Analysis

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

EX <- colSums(MX, FX)                                 # Total Export by ciid
MXS <- MX/EX                                          # Intermediate Export Share by ciid
FXS <- FX/EX                                          # Final Export Share by ciid

