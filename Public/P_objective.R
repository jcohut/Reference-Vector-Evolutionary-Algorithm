#This file includes the original and scaled DTLZ1 to DTLZ4

# function [Output,Boundary,Coding] <- P_objective(Operation,Problem,M,Input)
P_objective <- function(Operation, Problem, M, Input){
  m <- regexpr("[a-zA-Z]+", Problem)
  value <- regmatches(Problem, m)
  if (value == "DTLZ")
    list[Output,Boundary,Coding] <- P_DTLZ(Operation,Problem,M,Input)
  else if (value == 'SDTLZ')
    list[Output,Boundary,Coding] <- P_SDTLZ(Operation,Problem,M,Input)
  else
    stop(Problem,' Does Not Exist')
  return (list(Output, Boundary, Coding))
}

# This global variable for the function below
K_DTLZ <- 0
# function [Output,Boundary,Coding] <- P_DTLZ(Operation,Problem,M,Input)
P_DTLZ <- function(Operation, Problem, M, Input){
  Boundary <- NaN; 
  Coding <- NaN
  if (Operation == "init"){
    k <- find_last_alphabet_index(Problem)
    K_DTLZ <<- R(5, 10, 10, 10, 10, 10, 20)
    K_DTLZ <<- K_DTLZ[as.numeric(substr(Problem, k+1, nchar(Problem)))]
    
    D <- M+K_DTLZ-1
    MaxValue   <- ones(1, D)
    MinValue   <- zeros(1,D)
    # wrote util for rand
    Population <- rand(Input,D)
    Population <- Population*repmat(MaxValue,Input,1)+(1-Population)*repmat(MinValue,Input,1)
    
    Output   <- Population
    Boundary <- C(MaxValue,MinValue)
    Coding   <- 'Real'
  }
  #Objective Function Evaluation
  else if (Operation == 'value'){
    Population    <- Input
    FunctionValue <- zeros(size(Population,1),M)
    if (Problem == "DTLZ1") {
      g <- 100*(K_DTLZ+Sum((Population[,M:size(Population, 2)]-0.5)^2-cos(20*pi*(Population[,M:size(Population, 2)]-0.5)), 2))
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- 0.5*Prod(Population[,1:(M-i)], 2)*(1+g)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*(1-Population[,M-i+1])
        }
      }
    }
    else if (Problem == 'DTLZ2'){
      g <- Sum((Population[,M:size(Population, 2)]-0.5)^2, 2)
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- (1+g)*Prod(cos(0.5*pi*Population[,1:(M-i)]), 2)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*Population[,M-i+1])
        }
      }
    }
    else if (Problem == 'DTLZ3'){
      g <- 100*(K_DTLZ+Sum((Population[,M:size(Population, 2)]-0.5)^2-cos(20*pi*(Population[,M:size(Population, 2)]-0.5)), 2))
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- (1+g)*Prod(cos(0.5*pi*Population[,1:(M-i)]), 2)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*Population[,M-i+1])
        }
      }
    }
    else if (Problem == "DTLZ4"){
      Population[,1:(M-1)] <- Population[,1:(M-1)]^100
      g <- Sum((Population[,M:size(Population, 2)]-0.5)^2, 2)
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- (1+g)*Prod(cos(0.5*pi*Population[,1:(M-i)]), 2)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*Population[,M-i+1])
        }
      }
    }
    Output <- FunctionValue
  }
  else if (Operation == 'true') {
    #Sample True PFs
    
    if (Problem == "DTLZ1"){
      Population <- T_uniform(Input,M)
      Population <- Population/2
    }
    else if (Problem %in% c('DTLZ2', 'DTLZ3', 'DTLZ4')){
      Population = T_uniform(Input,M)
      for (i in 1 : size(Population,1)){
        Population[i,] <- Population[i,]/norm(Population[i,])
      }
    }
    Output <- Population
  }
  return (list(Output, Boundary, Coding))
}

K_SDTLZ <- 0
F_SDTLZ <- 0
# function [Output,Boundary,Coding] <- P_SDTLZ(Operation,Problem,M,Input)
P_SDTLZ <- function(Output, Boundary, Coding) {
  Boundary <- NaN; Coding <- NaN
  if (Operation == "init"){
    k <- find_last_alphabet_index(Problem)
    K_SDTLZ <<- R(5, 10, 10, 10, 10, 10, 20)
    K_SDTLZ <<- K_SDTLZ(as.numeric(substr(Problem, k+1, nchar(Problem))))
    F_SDTLZ <<- R(10, 10, 10, 10, 10, 5, 4, 3, 2, 2)
    F_SDTLZ <<- F_SDTLZ[M]
    
    D <- M+K_SDTLZ-1
    # These are supposed to be row vectors.. check if being column vectors violate any thing
    MaxValue   <- ones(1,D)
    MinValue   <- ones(1,D)
    Population <- rand(Input,D)
    Population <- Population*repmat(MaxValue,Input,1)+(1-Population)*repmat(MinValue,Input,1)
    
    Output   <- Population
    # This is column matrix.
    Boundary <- C(MaxValue,MinValue)
    Coding   <- 'Real'
  }
  #Objective Function Evaluation
  if (Operation == "value"){
    Population    <- Input
    FunctionValue <- zeros(size(Population,1),M)
    if (Problem == "SDTLZ1"){
      g <- 100*(K_SDTLZ+Sum((Population[, M:size(Population, 2)]-0.5)^2-cos(20*pi*(Population[,M:size(Population,2)]-0.5)),2))
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- 0.5
        else
          FunctionValue[,i] <- 0.5*Prod(Population[,1:(M-i)],2)*(1+g)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*(1-Population[,M-i+1])
        }
      }
    }
    if (Problem == "SDTLZ2"){
      g <- Sum((Population[,M:size(Population, 2)]-0.5)^2,2)
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- (1+g)*Prod(cos(0.5*pi*Population[,1:(M-i)]),2)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*Population[,M-i+1])
        }
      }
    }
    if (Problem == "SDTLZ3"){
      g <- 100*(K_SDTLZ+Sum((Population[,M:size(Population, 2)]-0.5)^2-cos(20*pi*(Population[,M:size(Population,2)]-0.5)),2))
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- (1+g)*Prod(cos(0.5*pi*Population[,1:(M-i)]),2)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*Population[,M-i+1])
        }
      }
    }
    if (Problem == "SDTLZ4"){
      Population[,1:(M-1)] <- Population[,1:(M-1)]^100
      g <- Sum((Population[,M:size(Population, 2)]-0.5)^2,2)
      for (i in 1 : M){
        if ((M-i) == 0) 
          FunctionValue[,i] <- (1+g)
        else
          FunctionValue[,i] <- (1+g)*Prod(cos(0.5*pi*Population[,1:(M-i)]),2)
        if (i > 1){
          FunctionValue[,i] <- FunctionValue[,i]*sin(0.5*pi*Population[,M-i+1])
        }
      }
    }
    Output <- FunctionValue * repmat((F_SDTLZ^(0:(M - 1)) ), R(size(FunctionValue,1), 1))
  }
  #Sample True PFs
  if (Operation == "true"){
    if (Problem == "SDTLZ1"){
      Population <- T_uniform(Input,M)
      Population <- Population/2
    }
    else if (Problem %in% c('SDTLZ2','SDTLZ3','SDTLZ4')){
      Population <- T_uniform(Input,M)
      for (i in 1 : size(Population,1)){
        Population[i,] <- Population[i,]/norm(Population[i,])
      }
    }
    Output <- Population*repmat((F_SDTLZ^(0:(M - 1))), R(size(Population,1), 1))
  }
  return (list(Output, Boundary, Coding))
  
}


# function W in T_uniform(k,M)){){
T_uniform <- function (k, M){
  H <- floor((k*prod(1:(M-1)) )^(1/(M-1)))
  while (nchoosek(H+M-1,M-1) >= k && H > 0){
    H <- H-1
  }
  if (nchoosek(H+M,M-1) <= 2*k || H == 0){
    H <- H+1
  }
  k <- nchoosek(H+M-1,M-1)
  Temp <- nchoosek(1:(H+M-1),M-1)-repmat(0:(M-2),nchoosek(H+M-1,M-1),1)-1
  W <- zeros(k,M)
  W[,1] <- Temp[,1]-0   # why subtract 0? weird..
  if((M-1)>=2)
    for (i in 2 : (M-1)) {
      W[,i] <- Temp[,i]-Temp[,i-1]
    }
  W[,size(W, 2)] <- H-Temp[,size(Temp, 2)]
  W <- W/H
  return (W)
}

## Not referenced from anywhere
## function W <- T_repeat(k,M)
# T_repeat <- function(k, M){
#     if (M > 1){
#         k <- (ceiling(k^(1/M)))^M
#         Temp <- seq(0,1, 1/(k^(1/M)-1))
#         code <- 'list[c1'
#         for (i in 2 : M){
#             code <- R(code,',c',as.character(i))
#         }
#         code <- R(code,'] <- ndgrid(Temp);')
#         # print(paste(code, collapse=""))
#         eval(parse(text=code))
#         code <- 'W <- cbind(c(c1)'
#         for (i in 2 : M){
#             code <- R(code,', c(c',as.character(i),')')
#         }
#         code <- R(code,')')

#         eval(parse(text=code))
#         # print(paste(code, collapse=""))
#     } else {
#         W <- C(seq(0,1, (1/(k-1))))
#     }
#   return (W)
# }

## function FunctionValue <- T_sort(FunctionValue)

## Not referenced from anywhere..

# T_sort <- function (FunctionValue){
#     Choose <- ones(1,size(FunctionValue,1))
#     # what is the equivalent of sortrows ?
#     list[,rank] <- sortrows(FunctionValue)
#     for (i in t(rank)){
#         # This is a bit confusing.. clear this up?
#         if (rfind(rank==i)[1] <= length(rank))
#         for (j in t(rank[(find(rank==i)+1)[1] : length(rank)])){
#             if (Choose[j]){
#                 k <- 1
#                 for (m in 2 : size(FunctionValue,2)){
#                     if (FunctionValue[i,m] > FunctionValue[j,m]){
#                         k <- 0
#                         break
#                     }
#                 }
#                 if (k == 1){
#                     Choose(j) <- F
#                 }
#             }
#         }
#     }
#     FunctionValue <- FunctionValue[Choose,]
#     return (FunctionValue)
# }