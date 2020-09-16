# choose parameters here..
npopulation <- 305
functioncount <- 3



nchoosek = function(n, x) {
  if (length(n)==1)
    return(choose(n, x))
  else if (length(n)>1) {
    # n is a set.. R contains a function that emulates the behaviour of nchoosek
    return (t(combn(n, x)))
  }
}

# a sufficiently large number
Run<-1000

for (p1 in 1:Run) {
  N_ <- nchoosek(p1+functioncount-1, functioncount-1)
  if (N_>npopulation)
    break
  N_p1 = N_
}

p1 <- p1 - 1

# Another sufficiently large number
Run2<-100
N_p2 <- 0

for (p2 in 0:Run2){
  N_ <- nchoosek(p2+functioncount-1, functioncount-1)
  if (N_ + N_p1 > npopulation)
    break
  N_p2 = N_
}
p2 <- max(p2-1, 0)

suggested_pop = N_p1 

if (p2>0) suggested_pop = suggested_pop + N_p2
 
print(list(p1=p1, p2=p2, suggested_pop=suggested_pop))
# print suggested population