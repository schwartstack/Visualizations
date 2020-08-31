library(tictoc)

primes = read.csv("primes.txt",header=F) #source: http://www.naturalnumbers.org/primes.html
last_digits = primes[,2]%%10


tic()
for(i in 1:1000){
  s = paste("thousands",i,".png",sep="")
  png(filename = s)
  plot(table(last_digits[1:i*1000]),
       main=paste("Final digits of first ",i*1000, " primes",
                  "\n",i*1000,"th prime: ",primes[i*1000,2],sep=""),
       ylab="Count",
       xlab="Final Digit")
  dev.off()
}
toc()

for(i in 1:1000){
  plot(table(last_digits[1:i*1000]),
       main=paste("Final digits of first ",i*1000, " primes",
                  "\n",i*1000,"th prime: ",primes[i*1000,2],sep=""),
       ylab="Count",
       xlab="Final Digit")
  Sys.sleep(.1)
  #dev.off()
}


first_digit <-function(n){
  return(as.numeric(substr(as.character(n),1,1)))
}

firsts = first_digit(primes[,2])

for(i in 1:1000){
  plot(table(firsts[1:i*1000]),
       main=paste("Final digits of first ",i*1000, " primes",
                  "\n",i*1000,"th prime: ",primes[i*1000,2],sep=""),
       ylab="Count",
       xlab="Final Digit")
  Sys.sleep(.1)
  #dev.off()
}