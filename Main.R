##################################################################################################
##  The source code of the reference vector guided evolutionary algorithm (RVEA) in R
##
##  See the details of RVEA in the following paper:
##
##  R. Cheng, Y. Jin, M. Olhofer and B. Sendhoff,
##  A Reference Vector Guided Evolutionary Algorithm for Many-objective Optimization,
##  IEEE Transactions on Evolutionary Computation, 2016
##
##  The source code of RVEA is implemented by Ran Cheng, this is just its port to R.
##
##  View the original code at
##  https://github.com/ranchengcn/RVEA_Matlab
##################################################################################################

# remember to change this address if the path is somewhere other
source("utils.R")
sourceall("Public")
sourceall("RVEA")

Main <- function(){
  # remove this while using, or change it.. 
  set.seed(21)
  
  # Number of objectives
  M <- 3
  # define objective function DTLZ2 in this case.
  dtlz2 <- function(x){
    # x is a row vector
    # change as necessary
    
    FunctionValue <- zeros(1,M)
    g <- sum((x[M:length(x)]-0.5)^2)
    for (i in 1 : M){
      if ((M-i) == 0) 
        FunctionValue[,i] <- (1+g)
      else
        FunctionValue[,i] <- (1+g)*prod(cos(0.5*pi*x[1:(M-i)]))
      if (i > 1){
        FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*x[M-i+1])
      }
    }
    FunctionValue
    
    # if you have functions f1, f2, f3.. just return them as R(f1, f2, f3)
  }
  
  # reference vector
  p1 <- 13
  p2 <- 0
  
  # num variables?
  K <- 10
  
  # bounds on the variables
  lbound= zeros(1, M+K-1)
  ubound = ones(1,M+K-1)
  
  Generations <- 200 # generations
  N <- 100  # npop
  
  output <- rvea(objective = dtlz2, Generations = Generations, M = M, K = K, N=N, p1=p1, 
                 p2=p2, lbound=lbound, ubound=ubound, optimize_func="min")
  
  # total_population <- output$total_population  # npop + ngen*npop
  # 
  # # entire population and their corresponding values.. stacked upon one another
  # # note that they are not a multiple of npop.. (due to sampling process I guess..)
  # all_population <- output$all_population
  # all_functionvalues <- output$all_functionvalues
  # 
  # population <- output$population  # population after the iteration is complete
  # functionvalue <- output$functionvalue
  # num_solutions <- output$num_solutions
  # 
  # # if you only want 10 solutions.. do this
  # S <- sample(num_solutions, 10)
  # P_s <- population[S,]
  # F_s <- functionvalue[S,]
  # 
}