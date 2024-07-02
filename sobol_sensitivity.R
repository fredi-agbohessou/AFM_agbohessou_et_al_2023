################################################################################
#                  Variance Based Sensitivity Analysis                         #
################################################################################
# Purpose   : Run a sensitivity analysis for the parameters of the             #
#             hydrological model, by using Variance based sensitivity analysis,#
#             developed by Saltelli et al., 2010                               #
################################################################################
# Reference : Andrea Saltelli, Paola Annoni, Ivano Azzini, Francesca Campolongo#
#             Marco Ratto, Stefano Tarantola, Variance based sensitivity       #
#             analysis of model output. Design and estimator for the total     #
#             sensitivity index, Computer Physics Communications, Volume 181,  #
#             Issue 2, February 2010, Pages 259-270, ISSN 0010-4655,           #
#             DOI: 10.1016/j.cpc.2009.09.018.                                  #
#          (http://www.sciencedirect.com/science/article/pii/S0010465509003087)#
################################################################################
# This function is based on the original Matlab code provided by Stefano       #
# Tarantola (last release: 27-Sep-2010)                                        #
################################################################################
# Author  : Mauricio Zambrano-Bigiarini                                        #
################################################################################
# Started : 07-Mar-2013                                                        #
# Updates : 08-Mar-2013 ; 10-Mar-2013 ; 11-Mar-2013 ; 26-Mar-2013 ; 17-May-2013#
#           18-May-2013 ; 19-May-2013 ; 20-May-2013                            #
################################################################################
#                                   Inputs                                     #
################################################################################
# fn         : character with the name of a valid R function to be analised    #
#              its first argument MUST contain a K-dimensional variable        #
#              representing the input parameters to be analised                #
# ...        : additional arguments for 'fn'                                   #
# lower      : K-dimensional numeric values with the minimum values allowed for#
#              each parameter                                                  #
# upper      : K-dimensional numeric values with the maximum values allowed for#
#              each parameter                                                  #
# N          : number of points used for the low-discrepancy (Sobol') or LHS   #
#              design of the matrices A and B.                                 #
# rtype      : character, indicating the type of random sampling design used   #
#              for matrices A and B. Valid values are:                         #
#              -) 'quasirandom' : low-discrepancy (Sobol's) sequences          #
#              -) 'lhs'         : latin hypercube sampling                     #
# scrambling : Should scrambling be applied to the quasi-random sequences ?    #
#              Default value: 'Owen'. See ?randtoolbox::sobol                  #
# reltol     : numeric, relative tolerance. If a value different from zero is  #
#              the SA will finish when the maximum difference between the first#
#              order sensitivity index in all the parameters ('DSi') satisfy   #
#              the following relation:  Di = Max {|S[i] - S[i-1]} <= reltol    #
# verbose    : logical, should progress messages be printed out to the screen ?#
# REPORT     : numeric, only used when 'verbose=TRUE'.                         #
#              a progress message is printed every 'REPORT' number of model    #
#              evaluations                                                     #
# full.output: logical, should the sampling matrices 'A' and 'B' along with all#
#              the parameter sets and its corresponding goodness-of-fit value  #
#              be included in the output of this function ?.                   #
#              Default value 'full.output=FALSE                                #  
################################################################################
#                                   Outputs                                    #       
# A list with the follwowing elements:                                         # 
################################################################################
#   *) Matrix.A               : (optional). A matrix used in the radial        #
#                               sampling                                       #
#   *) Matrix.B               : (optional). B matrix used in the radial        #
#                               sampling                                       #
#   *) ParameterSets          : (optional) matrix with all the parameter sets  #
#                               used in the analysis                           #
#   *) GoFs                   : (optional) numeric vector with the             #
#                               goodness-of-fit or model's performance of each #
#                               parameter set in 'ParameterSets'               #     
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-#     
#   *) N                      : number of points used for the low-discrepancy  #
#                               (Sobol') or LHS (Latin hypercube sampling)  design of the matrices A and B#   
#   *) counts                 : total number of model evaluations              #
#   *) Si                     : First order sensitivity indices for each one   #
#                               of the K parameters                            #
#   *) Sum.Si                 : sum of all the K first order sensitivity       #
#                               indices ( Si )                                 #
#   *) Total.Order.Indices.Sti: Total order sensitivity indices for each one   #
#                               of the K parameters                            #
#   *) Total.Indices.of.Pairs.of.Factors: matrix with the total indices of     #
#                               pairs of factors, for each possible combination#
#                               of input factors                               #
#   *) Ranking                : matrix with a ranking of the most important    #
#                               parameters, sorted in decreasing order         #
#                               according to the first order sensitivity index.#
################################################################################
 
sobol <- function(
                  fn,  
                  ...,
                  lower=-Inf,
                  upper=Inf,
                  N=1000,                         
                  rtype=c("quasirandom", "lhs"),   
                  scrambling=c("Owen","Faure-Tezuka","Owen+Faure-Tezuka","none"),          # See ?randtoolbox::sobol
                  reltol=0,                       # 
                  verbose= TRUE,                  # logical, indicating if progress messages have to be printed          
                  REPORT=10, 
	          full.output=FALSE          
                  ) {
                     
  ##############################################################################
  #                            INPUT CHECKING                                  #
  ##############################################################################
        
  # Assigning the objective function
  fn <- match.fun(fn)

  # checking length of 'lower' and 'upper'
  if (length(lower) != length(upper) )
    stop( paste( "Invalid argument: 'length(lower) != length(upper) (", length(lower), "!=", length(upper), ")'", sep="" ) )        

  rtype      <- match.arg(rtype, c("quasirandom", "lhs"))  
  scrambling <- match.arg(scrambling, c("Owen","Faure-Tezuka","Owen+Faure-Tezuka","none"))  
  scrambling <- pmatch(scrambling, c("Owen","Faure-Tezuka","Owen+Faure-Tezuka","none"))  
  if (scrambling > 3) scrambling <- 0
          
  ########################################################################
  ##################### Dummy checkings ##################################

  # Checking that 'N' is integer
  if ( trunc(N) != N ) stop( "Invalid argument: 'N' must be integer !" )

  # checking 'X.Boundaries' 
  if ( (lower[1L] == -Inf) || (upper[1L] == Inf) ) {
    #if (any(upper==Inf | lower==-Inf))
    stop( "Invalid argument: 'lower' and 'upper' boundaries must be finite !!'" )
  } else X.Boundaries <- cbind(lower, upper)              
          
  # Computing 'K', the Dimension of the Solution Space
  K <- nrow(X.Boundaries)

  # Meaningful name of each one of the parameters
  if (is.null(rownames(X.Boundaries))) {
    param.IDs <- paste("Param", 1:K, sep="")
  } else param.IDs <- rownames(X.Boundaries)   
  
  if (rtype=="quasirandom") {
      # Backing up the original boundaries
      lower.ini <- lower
      upper.ini <- upper
      X.Boundaries.ini <- X.Boundaries
      LOWER.ini <- matrix( rep(lower.ini, N), nrow=N, byrow=TRUE)
      UPPER.ini <- matrix( rep(upper.ini, N), nrow=N, byrow=TRUE)
      
      # normalising
      lower <- rep(0, K)
      upper <- rep(1, K)
      X.Boundaries <- cbind(lower, upper)
      rownames(X.Boundaries) <- param.IDs
    } # IF end 
        
  # Total Number of parameter sets to be used in the GSA
  nparamsets  <- N*(K+2)

  # Checking report
  if (N < REPORT) {
      REPORT <- N
      warning("[ 'REPORT' is greater than 'N' => 'REPORT = N' ]")
  } # IF end
  
  message("                                                              ")
  message("[ Number of Parameter Sets to be run ( N*(K+2) ): ", nparamsets, " ]")     
 
  ##############################################################################
  #                            MAIN BODY
  ##############################################################################
  
  # sampling design
  if (rtype=="quasirandom") {
    rs <- randtoolbox::sobol(n=N, dim=2*K, scrambling=scrambling)  
    A <- rs[, 1:K]
    B <- rs[, (K+1):(2*K)]
  } else if (rtype=="lhs") {
      A <- rLHS(n=N, ranges=X.Boundaries)
      B <- rLHS(n=N, ranges=X.Boundaries)
    } # ELSE end
  
  # Output Matrix with First Order Indices
  S           <- matrix(NA, nrow=N, ncol=K)
  colnames(S) <- param.IDs
  
  # Output Matrix with Total Order Indices
  St          <- S

  # Matrixes for first and total order variances
  Vi <- matrix(NA, nrow=N, ncol=K)
  Vt <- Vi

  # Vector of model outputs used to compute the output variance
  y4var <- rep(NA, 2*N)
  
  # Preparation of the radial sample matrix X [K+2,K]
  X           <- matrix(NA, nrow=nparamsets, ncol=K)
  colnames(X) <- param.IDs

  # Numerators for total indices of pairs of factors
  Vtij <- array( NA, dim=c(N, K, K) )

  # Si values in the (i-1)-th point
  Si.previous <- rep(0, K)

  # Differences between the first order sensitivity index in the i-th point and 
  # the previous one
  Si.Delta <- rep(NA, K)
  
  # Vector with goodness-of-fit of model outputs
  y <- rep(NA, nparamsets)

  # Vector with model outputs
  ModelOut <- vector("list", nparamsets)
  
  # Counter of the points belonging to the initial LHS
  i <- 1    

  # relative convergence
  reltol.conv <- FALSE
  
  ##############################################################################
  #                4) Loop for each row of the A and B matrixes                #
  ##############################################################################
  while ( (i <= N) & !reltol.conv ) {   

    # Matrix with parameter sets
    ParamSets  <- matrix(NA, nrow=K+2, ncol=K)

    # Creating of the Ab matrix for radial sampling
    Ab <- matrix(NA, nrow=K+2, ncol=K)

    # Goodness-of-fit of each parameter set used in the GSA
    gof <- rep(NA, K+2)

    if (rtype == "quasirandom") {
        An <- A * (UPPER.ini - LOWER.ini) + LOWER.ini
        Bn <- B * (UPPER.ini - LOWER.ini) + LOWER.ini
    } else {
        An <- A
        Bn <- B
      } # ELSE end
      
    # Creating of the Ab matrix for radial sampling
    Ab      <- matrix(NA, nrow=K+2, ncol=K)
    Ab[1, ] <- An[i, ]
    Ab[2, ] <- Bn[i, ]
    for (j in 1:K){
      Ab[j+2, ]  <- An[i, ]
      Ab[j+2, j] <- Bn[i, j]
    } # FOR end    

    Xn <- Ab
    
    # Filling the X matrix
    X[((K+2)*(i-1) + 1):((K+2)*i), ]  <- Xn
    
    ############################################################################
    ############################################################################
    # Loop for each parameter of the i point of the initial LHS
    # 3.a) Evaluate the particles fitness

    GoF <- apply(Xn, fn, MARGIN=1, ...)
     
    gof[1:(K+2)]      <- GoF
    ModelOut[1:(K+2)] <- GoF  ###

    ##########################################################################
    # 9)                  Updating the sensitivity matrix                    #                                 
    ##########################################################################

    # Storing the GoF corresponding to the 'i' parameter set and the related ones
    y[((K+2)*(i-1) + 1):((K+2)*i)]  <- gof
      
    yA  <- gof[1]
    yB  <- gof[2]
    yAb <- gof[3:(K+2)]
      
    # Vector for computation of total variance
    y4var[(2*i-1):(2*i)] <- c(yA, yB)
     
    # Numerator for first and total Indices
    Vi[i, 1:K] <- yB*(yAb[1:K]-yA)
    Vt[i, 1:K] <- (yA - yAb[1:K])^2
    
    # Computation of total variance
    Vtot <- var(y4var, na.rm=TRUE)
    
    # Computation of first and total order sensitivity indices (Si and St, respectively)
    S[i, ]  <- apply(Vi, MARGIN=2, FUN=mean, na.rm=TRUE) / Vtot
    St[i, ] <- apply(Vt, MARGIN=2, FUN=mean, na.rm=TRUE) / (2*Vtot)   
    
    # Numerators for total indices of pairs of factors (no extra computational cost)
    # Vtij[N,k,k] is a set of triangular matrixes
    for (j in 1:K) {
      yAbi <- yAb[j]
      j2 <- j +1
      while(j2 <= K) {
        yAbj <- yAb[j2]
        Vtij[i, j, j2] <-  (yAbi - yAbj) * (yAbi-yAbj)
        j2 <- j2 + 1
      } # FOR 'j2' END
    } # FOR 'j' END

    # Updating values of the first order sensitivity index for the i-th point and
    # its difference with the previous point    
    Si.Delta    <- abs( S[i, ] - Si.previous )
    Si.Delta.mx <- max(Si.Delta)
    Si.previous <- S[i, ]

    if (reltol==0) {
        reltol.conv <- FALSE
    } else {
        if (Si.Delta.mx <= reltol) {
          reltol.conv <- TRUE
        } else reltol.conv <- FALSE
      } # ELSE end
    

    if ( (i/REPORT == floor(i/REPORT)) & (verbose) ) {
      if (K <= 7) {
        message( "[ Model Runs : ", 
                 format( i*(K+2), width=7, justify="left" ), "/", nparamsets, ". ",   
                 paste("dS", 1:K, ": ", format(Si.Delta, scientific=TRUE, digits=3, width=9), ".  ", collapse=" ", sep=""),  
                 "]" )
      } else {
           message( "[ Model Runs : ", 
                   format( i*(K+2), width=7, justify="left" ), "/", nparamsets, ". ",   
                   paste("Max(|dS|): ", 
                         formatC(max(Si.Delta), format="E", digits=3, flag=" "), 
                         " (i=", which.max(Si.Delta), 
                         "). Min(|dS|): ", 
                         formatC(min(Si.Delta), format="E", digits=3, flag=" "),
                         " (i=", which.min(Si.Delta), 
                         ")", sep=""),  "]" )
        } # ELSE end
    } # IF end
    
    i <- i + 1    

  } # WHILE i end

  ##############################################################################
  # 7)                    Sensitivity of each Parameter                       #                                 
  ##############################################################################

  # Computation of totals indices of pairs of factors (no extra computational cost)
  Stij <- matrix(NA, nrow=K, ncol=K)
  for (j in 1:K) {
    j2 <- 1
    while (j2 <= K) {
      Stij[j2, j] <-  mean(Vtij[, j, j2], na.rm=TRUE) / (2*Vtot)
      j2 <- j2 + 1
    } # FOR 'j2' END
  } # FOR 'j' END
  
  # Number of totals indices of pairs of factors
  #nStij <- 1 + sum(1:(K-1)) # including St11
  nStij <- sum(1:(K-1)) # not including St11
  
  # Creating a vector with names for Stij
  Stij.names <- rep(NA, nStij)
  
  # Creating a vector with values for Stij
  Stij.values <- rep(NA, nStij)
  
  #p <- 2 # including St11
  p <- 1 # not including St11
  for (i in 1:K) { 
    j <- i + 1
    while (j <= K) {
      Stij.names[p]  <- paste("St_", param.IDs[i], "_", param.IDs[j], sep="")
      Stij.values[p] <- Stij[j,i]
      j <- j + 1 
      p <- p + 1
    } # WHILE end
  } # FOR end
  colnames(Stij) <- param.IDs 
  rownames(Stij) <- param.IDs 
  
  ##############################################################################
  # 9)          Computing  the  Final  Ranking  of  Sensitivity                #
  #                sorted by First Order Sensitivity Indices                   #                                 
  ##############################################################################
  # First order sensitivity indices: final values
  Si        <- S[N, ]
  names(Si) <- param.IDs

  # Total order sensitivity indices: final values
  St        <- St[N, ]
  names(St) <- param.IDs

  # Sorting the parameters, from the most sensitive to the least one
  Ranking <- sort(Si, decreasing=TRUE, na.last=TRUE)

  # Parameter Order
  index <- pmatch(names(Ranking), param.IDs)
  
  # Adding a column with a sequential number for the ranking
  Ranking <- data.frame(Ranking.Nmbr=format(as.character(1:K), width=11, justify="left"), 
                        Parameter.Name=format(param.IDs[index], width=13, justify="left"), 
                        First.Order.Index=as.numeric(Si[index]),
                        Total.Order.Index=as.numeric(St[index]) 
                        )                        
  Ranking[, "Ranking.Nmbr"]   <- as.character(Ranking[, "Ranking.Nmbr"])
  Ranking[, "Parameter.Name"] <- as.character(Ranking[, "Parameter.Name"]) 
    
  ##############################################################################
  # 10)                    Creating the output                                 #                                 
  ##############################################################################
  if (full.output) {   
    nelements <- 11
    first     <- 5
    colnames(A) <- param.IDs 
    colnames(B) <- param.IDs 
  } else {
      nelements <- 7
      first     <- 1
    } # ELSE end

   # Creating the R output
   ## "pre-allocate" an empty list of length 'nelements'
   out <- vector("list", nelements)

   out[[first]]   <- N           # number of points used in the random design
   out[[first+1]] <- nparamsets  # total number of parameter sets   
   out[[first+2]] <- Si          # First.Order.Indices
   out[[first+3]] <- sum(Si)     # Sum of First.Order.Indices
   out[[first+4]] <- St          # Total.Order.Indices
   out[[first+5]] <- Stij        # Total Indices of Pairs of Factors
   out[[first+6]] <- Ranking     # Ranking
   names(out)[first:(first+6)] <- c("N", "counts", "First.Order.Indices.Si", 
                                    "Sum.Si", "Total.Order.Indices.Sti", 
                                    "Total.Indices.of.Pairs.of.Factors", 
                                    "Ranking")    

  if (full.output) {

    out[[1]] <- An # Matrix A
    out[[2]] <- Bn # Matrix B
    out[[3]] <- X  # All Parameter Sets
    out[[4]] <- y  # all goodness-of-fit values
 
    names(out)[1:4] <- c("Matrix.A", "Matrix.B", "ParameterSets", "GoFs")
    
  } # IF end
  
  return(out)

} # 'sobol' END



################################################################################
#    Example: Ishigami test function                                           #
################################################################################

if ( !require(randtoolbox) ) install.packages(randtoolbox)
if ( !require(lhs) )         install.packages(lhs)



