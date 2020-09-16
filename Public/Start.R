# checked!
Start <- function (Algorithm,Problem,Objectives,Run) {
  # Algorithm := List of algorithms e.g. c('RVEA')
  # Problem := A single problem string.. e.g. DTLZ2
  # Objectives := Number of objectives
  # Run := A single number in this case
  
#     if (nargin < 4)
#       Run <- 0;}){
    
    if (!is.atomic(Algorithm))
        Algorithm <- c(Algorithm)
    
    if (!is.atomic(Problem))
        Problem <- c(Problem)
    
    for (R in 1:Run){
        for (A in Algorithm){
            a <- data.matrix(A)
            if (dir.exists(a)){
                # Warning: Enable this while running!
                # sourceall(a)
                
            } else {
                stop(sprintf("Algorithm %s not found", a))
            }
            for (P in Problem){
                for (M in Objectives){
                    MAIN(as.matrix(P),M,R)
                }
            }
        }
    }
#     sp <- actxserver('SAPI.SpVoice')
#     sp.Speak('job finished')
	return()
}

