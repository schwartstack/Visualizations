library(beepr)

primes = read.csv("primes.txt",header=F) #source: http://www.naturalnumbers.org/primes.html
last_digits = primes[,2]%%10

#this for loop stores each frame of the animation as a png that I later turn into a gif using Gimp
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
beep()

#this for loop shows the same thing but it displays it right in your R console
for(i in 1:1000){
  plot(table(last_digits[1:i*1000]),
       main=paste("Final digits of first ",i*1000, " primes",
                  "\n",i*1000,"th prime: ",primes[i*1000,2],sep=""),
       ylab="Count",
       xlab="Final Digit")
  Sys.sleep(.1)
}
