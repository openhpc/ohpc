hilbert<-function(n) 1/(outer(seq(n),seq(n),"+")-1)
print("hilbert n=500")
print(system.time(eigen(hilbert(500))))
print(system.time(eigen(hilbert(500))))
print(system.time(eigen(hilbert(500))))
print("hilbert n=1000")
print(system.time(eigen(hilbert(1000))))
print(system.time(eigen(hilbert(1000))))
print(system.time(eigen(hilbert(1000))))
print("sort n=6")
print(system.time(sort(rnorm(10^6))))
print(system.time(sort(rnorm(10^6))))
print(system.time(sort(rnorm(10^6))))
print("sort n=7")
print(system.time(sort(rnorm(10^7))))
print(system.time(sort(rnorm(10^7))))
print(system.time(sort(rnorm(10^7))))
# loess
loess.me<-function(n) {
print(paste("loess n=",as.character(n),sep=""))
for (i in 1:5) {
    x<-rnorm(10^n); y<-rnorm(10^n); z<-rnorm(10^n)
    print(system.time(loess(z~x+y)))
    }
}
loess.me(3)
loess.me(4)
