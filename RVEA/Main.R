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
#source("Rvea.R")

Main <- function(){
  # remove this while using, or change it.. 
  set.seed(14)
  
  assign("last.warning", NULL, envir = baseenv())  
  
#   viennet <- function(x){
#     f1 <- 0.5*(x[1]^2+x[2]^2)+sin(x[1]^2+x[2]^2)
#     f2 <- 0.125*(3*x[1]-2*x[2]+4)^2+(1.0/27.0)*(x[1]-x[2]+1)^2+15
#     f3 <- 1.0/(x[1]^2+x[2]^2+1)-1.1*exp(-(x[1]^2+x[2]^2))
#     return(c(f1,f2,f3))
#   }
  
  p1 <- 18
  p2 <- 3
  
  varcount <- 12
  fncount <- 3
  lbound <- rep(0,12)
  ubound <- rep(1,12)
  optmin <- 0
  popsize <- 200
  maxgen <- 250
  
  ex1 <- rvea(dtlz2_3,varcnt=varcount,fncnt=fncount,
              lowerbound=lbound,upperbound=ubound,opt=optmin, popsize=popsize,maxgen=maxgen,
              p1=p1, p2=p2)
  
#   ex1$paramvalues
#   ex1$objfnvalues  
#   ex1$numsols
#   ex1$stored_params
#   ex1$popFit
  
  #   # if you only want 10 solutions.. do this
  #   S <- sample(num_solutions, 10)
  #   P_s <- population[S,]
  #   F_s <- functionvalue[S,]
  
}