# checked!
P_sort <- function(FunctionValue, operation = ""){

# Efficient non-dominated sort on sequential search strategy, TEVC, 2014,
# Xingyi Zhang, Ye Tian, Ran Cheng and Yaochu Jin
# Copyright 2014 BCMA Group, Written by Mr Ye Tian and Prof Xingyi Zhang
# Contact: xyzhanghust@gmail.com

    if (operation == 'half'){
        kinds <- 2
    } else if (operation == 'first'){
        kinds <- 3
    } else {
        kinds <- 1
    }

    NM <- size(FunctionValue)
    N <- NM[1]; M <- NM[2]
    MaxFront <- 0
    Sorted <- zeros(1,N)
    
    list[FunctionValue,rank] <- Sortrows(FunctionValue)
    FrontValue <- zeros(1,N) + Inf
    
    while ((kinds == 1 && sum(Sorted)<N) || (kinds == 2 && sum(Sorted)<N/2) || (kinds == 3 && MaxFront<1)){
        MaxFront <- MaxFront + 1
        ThisFront <- as.logical(zeros(1, N))
        for (i in 1:N){
            if (!Sorted[i]){
                x <- 0
                for (j in 1:N){
                    if (ThisFront[j]){
                        x <- 2
                        for (j2 in 2 : M){
                            if (FunctionValue[i,j2] < FunctionValue[j,j2]){
                                x <- 0
                                break
                            }
                        }
                        if (x == 2){
                            break
                        }
                    }
                }
                if (x != 2){
                    ThisFront[i] <- T
                    Sorted[i] <- T
                }
            }
        }
        # Potentially problematic?
        # index <- 1 * (1 %in% ThisFront)
        FrontValue[rank[ThisFront]] <- MaxFront
    }
	return(list(FrontValue,MaxFront))
}
