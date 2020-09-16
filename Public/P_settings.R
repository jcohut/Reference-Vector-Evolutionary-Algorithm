# Checked!
P_settings <- function(Algorithm,Problem,M){
    list[Generations,N] <- set_problem(Problem,M)
    Parameter <- set_algorithm(Algorithm,Problem,M)
    varargout <- Parameter
	return (list(Generations,N,varargout))
}

set_problem <- function(Problem,M){
    # find last index that's not the digit.. probably can be optimized a lot.
    k <- find_last_alphabet_index(Problem)
    
    D <- as.numeric(substr(Problem, k+1, nchar(Problem)))

    Problem <- substr(Problem,1,k)
    if (Problem == "DTLZ" || Problem == "SDTLZ"){
        if (M < 2 || M > 10){
            error('Objective Number Not Supported !')
        }
        if (D < 1 || D > 7){
            stop(Problem,' Does Not Exist')
        }
        Generations <- R(1000, 500, 1000, 500)
        Generations <- Generations[D]
    }

    else {
        stop(Problem,' Does Not Exist')
      }
    
    N <- R(50, 105, 120, 126, 132, 112, 156, 90, 275)
    N <- N[M-1]
	return(list(Generations,N))
}

set_algorithm <- function(Algorithm,Problem,M){
    Parameter <- c(NaN, NaN)
    
    # find last index that's not the digit.. 
    k <- find_last_alphabet_index(Problem)
    
    D <- as.numeric(substr(Problem, k+1, nchar(Problem)))
    Problem <- substr(Problem, 1, k)
    if (Algorithm == 'RVEA'){
            p1 <- R(49, 13,  7,  5,  4,  3,  3,  2,  3)
            p2 <- R(0,  0,  0,  0,  1,  2,  2,  2,  2)
            p1 <- p1[M-1]
            p2 <- p2[M-1]
                     
            Parameter[1] <- p1; Parameter[2] <- p2
    }
    else{
            stop(Algorithm, ' Does Not Exist')
    }
    return (Parameter)
}

