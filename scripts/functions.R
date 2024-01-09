## COPIED FROM ORIGINAL SCRIPT

# We present three functions to view the output.
# These functions are explained and used in
#  -Beginner's Guide to Zero-Inflated models with R
#   Zuur and Ieno. www.highstat.com
#  -Beginner's Guide to GLM and GLMM with R
#   Zuur, Hilbe, Ieno. www.highstat.com

# Support files from 'Beginner's Guide to GLM and GLMM with R.
# Zuur, Hilbe, Ieno (2013).
MyBUGSChains <- function(xx, vars, PanelNames = NULL){
  #Small function to make an xyplot of the iterations per chain,
  #for each variable
  x <- xx$sims.array
  idchain.All <- NULL
  x1.All <- NULL
  ChainLength.All <- NULL
  id.All <- NULL

  NumBerChains <- ncol(x[,,vars[1]])

  for (i in vars) {
    x1          <- as.vector(x[,,i])
    id          <- rep(rep(i, length = nrow(x[,,i])),NumBerChains)
    idchain     <- rep(1:NumBerChains, each = nrow(x[,,i]))
    ChainLength <- rep(1:nrow(x[,,i]), NumBerChains)

    x1.All <- c(x1.All, x1)
    ChainLength.All <- c(ChainLength.All, ChainLength)
    id.All <- c(id.All, id)
    idchain.All <- c(idchain.All, idchain)
  }

  if (!is.null(PanelNames)) {
    if (length(unique(id.All)) != length(PanelNames)) {stop("Wrong number of panel names")}
    AllNames <- unique(id.All)
    for (i in 1:length(AllNames)) {
      id.All[id.All == AllNames[i]] <- PanelNames[i]
    }
    id.All <- factor(id.All, levels = PanelNames)
  }


  Z <- xyplot(x1.All ~ ChainLength.All | factor(id.All) ,
              type = "l",
              strip = strip.custom(bg = 'white',
                                   par.strip.text = list(cex = 1.2)),
              scales = list(x = list(relation = "same", draw = TRUE),
                            y = list(relation = "free", draw = TRUE)),
              groups = idchain.All,  col = 1:NumBerChains,
              xlab = list(label = "MCMC iterations", cex = 1.5),
              ylab = list(label = "Sampled values", cex = 1.5))
  print(Z)
}

MyBUGSOutput <- function(Output  = Output, SelectedVar = SelectedVar, VarNames = NULL){
  xx   <- Output
  vars <- SelectedVar

  if (is.null(VarNames)) { VarNames <- SelectedVar }
  if (length(SelectedVar) != length(VarNames)) {stop("Wrong number of variable names")}

  x <- xx$sims.matrix
  OUT <- matrix(nrow = length(vars), ncol = 4)
  j <- 1
  for (i in vars) {
    xi <- x[,i]
    OUT[j,3:4] <- quantile(xi, probs = c(0.025, 0.975))
    OUT[j,1]   <- mean(xi)
    OUT[j,2]   <- sd(xi)
    j          <- j + 1
  }
  colnames(OUT) <- c("mean", "se", "2.5%", "97.5%")
  rownames(OUT) <- VarNames
  OUT
}

uNames <- function(k,Q){
  #Function to make a string of variables names of the form:
  #c("u[1]","u[2]", etc, "u[50]")
  String <- NULL
  for (j in 1:Q) {String <- c(String, paste(k,"[",j,"]",sep = ""))}
  String
}
# End of support code from Zuur et al.(2013).
