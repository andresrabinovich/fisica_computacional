p   <- 0.5
l   <- 128
rep <- 30000

set.seed(123456)

start.time <- Sys.time()
r <- runif(l*l*rep)
a <- r<=p 

for(i in 1:rep){
  m <- matrix(ncol=l, nrow=l, a[(l*l*(i-1)+1):(l*l*i)])
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

