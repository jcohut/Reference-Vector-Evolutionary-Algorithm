# checked!
P_generator <- function(MatingPool,Boundary,Coding,MaxOffspring = 0){
  # This function includes the SBX crossover operator and the polynomial
  # mutation operator.
  
  ND <- size(MatingPool)
  N <- ND[1]
  D <- ND[2]
  if (MaxOffspring < 1 || MaxOffspring > N){
    MaxOffspring <- N
  }
  
  if (Coding == 'Real'){
    ProC <- 1
    ProM <- 1/D
    DisC <- 30
    DisM <- 20
    Offspring <- zeros( N, D)
    
    for (i in seq(1, N, 2) ) {
      beta <- zeros(1, D)
      miu  <- rand(1, D)
      beta[miu<=0.5] <- (2*miu[miu<=0.5])^(1/(DisC+1))
      beta[miu>0.5]  <- (2-2*miu[miu>0.5])^(-1/(DisC+1))
      beta <- beta * (-1)^round(rand(1, D))
      beta[rand(1, D)>ProC] <- 1
      Offspring[i,] <- (MatingPool[i,] +MatingPool[i+1,])/2 + beta * (MatingPool[i,]-MatingPool[i+1,])/2
      Offspring[i+1,] <- (MatingPool[i,]+MatingPool[i+1,])/2 - beta * (MatingPool[i,]-MatingPool[i+1,])/2
    }
    
    Offspring <- Offspring[1:MaxOffspring,]
    
    if (MaxOffspring == 1){
      MaxValue <- Boundary[1,]
      MinValue <- Boundary[2,]
    } else {
      # repmat defined at utils
      MaxValue <- repmat(Boundary[1,], MaxOffspring,1);
      MinValue <- repmat(Boundary[2,], MaxOffspring,1);
    }
    k    <- rand(MaxOffspring, D)
    miu  <- rand(MaxOffspring, D)
    Temp <- (k<=ProM & miu<0.5)
    
    Offspring[Temp] <- Offspring[Temp]+(MaxValue[Temp]-MinValue[Temp]) * ((2*miu[Temp]+(1-2*miu[Temp])*(1-(Offspring[Temp]-MinValue[Temp])/(MaxValue[Temp]-MinValue[Temp]))^(DisM+1))^(1/(DisM+1))-1)
    Temp <- (k<=ProM & miu>=0.5)
    Offspring[Temp] <- Offspring[Temp]+(MaxValue[Temp]-MinValue[Temp]) * (1-(2*(1-miu[Temp])+2*(miu[Temp]-0.5)*(1-(MaxValue[Temp]-Offspring[Temp])/(MaxValue[Temp]-MinValue[Temp]))^(DisM+1))^(1/(DisM+1)))
    
    Offspring[Offspring>MaxValue] <- MaxValue[Offspring>MaxValue]
    Offspring[Offspring<MinValue] <- MinValue[Offspring<MinValue]
    
  }
  return (Offspring)
}
