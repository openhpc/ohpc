# R Benchmark 2.5 (06/2008) [Simon Urbanek]
# version 2.5: scaled to get roughly 1s per test, R 2.7.0 @ 2.6GHz Mac Pro
# R Benchmark 2.4 (06/2008) [Simon Urbanek]
# version 2.4 adapted to more recent Matrix package
# R Benchmark 2.3 (21 April 2004)
# Warning: changes are not carefully checked yet!
# version 2.3 adapted to R 1.9.0
# Many thanks to Douglas Bates (bates@stat.wisc.edu) for improvements!
# version 2.2 adapted to R 1.8.0
# version 2.1 adapted to R 1.7.0
# version 2, scaled to get 1 +/- 0.1 sec with R 1.6.2
# using the standard ATLAS library (Rblas.dll)
# on a Pentium IV 1.6 Ghz with 1 Gb Ram on Win XP pro

# revised and optimized for R v. 1.5.x, 8 June 2002
# Requires additionnal libraries: Matrix, SuppDists
# Author : Philippe Grosjean
# eMail  : phgrosjean@sciviews.org
# Web    : http://www.sciviews.org
# License: GPL 2 or above at your convenience (see: http://www.gnu.org)
#
# Several tests are adapted from the Splus Benchmark Test V. 2
# by Stephan Steinhaus (stst@informatik.uni-frankfurt.de) 
# Reference for Escoufier's equivalents vectors (test III.5):
# Escoufier Y., 1970. Echantillonnage dans une population de variables
# aleatoires réelles. Publ. Inst. Statis. Univ. Paris 19 Fasc 4, 1-47.
#
# type source("c:/<dir>/R2.R") to start the test

runs <- 3			# Number of times the tests are executed
times <- rep(0, 15); dim(times) <- c(5,3)
require(Matrix)		# Optimized matrix operations
###~~~~~~~~~~#### TRon (4/27/15) Not using SuppDist lib for CMT ..........
###require(SuppDists)	# Optimized random number generators
###Runif <- rMWC1019	# The fast uniform number generator
Runif <- runif
# If you don't have SuppDists, you can use: Runif <- runif
#a <- rMWC1019(10, new.start=TRUE, seed=492166)	# Init. the generator
a <- runif(10)  #TRon (4/27/15) Replacing rMWC above ............
###Rnorm <- rziggurat	# The fast normal number generator
# If you don't have SuppDists, you can use: Rnorm <- rnorm
#b <- rziggurat(10, new.start=TRUE)	# Init. the generator
b <- rnorm(10) #~~~~~~~~~~~# TRon (4/27/15) Replacing ziggurat above .....
Rnorm <- rnorm
remove("a", "b")
options(object.size=100000000)

cat("\n\n   R Benchmark 2.5\n")
cat("   ===============\n")
cat(c("Number of times each test is run__________________________: ", runs))
cat("\n\n")


cat("   I. Matrix calculation\n")
cat("   ---------------------\n")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (1)
cumulate <- 0; a <- 0; b <- 0
for (i in 1:runs) {
  invisible(gc())
  timing <- system.time({
    a <- matrix(Rnorm(2500*2500)/10, ncol=2500, nrow=2500);
    b <- t(a);
    dim(b) <- c(1250, 5000);
    a <- t(b)
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[1, 1] <- timing
cat(c("Creation, transp., deformation of a 2500x2500 matrix (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (2)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- abs(matrix(Rnorm(2500*2500)/2, ncol=2500, nrow=2500));
  invisible(gc())
  timing <- system.time({ 
    b <- a^1000 
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[2, 1] <- timing
cat(c("2400x2400 normal distributed random matrix ^1000____ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (3)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- Rnorm(7000000)
  invisible(gc())
  timing <- system.time({
    b <- sort(a, method="quick")	# Sort is modified in v. 1.5.x
    # And there is now a quick method that better competes with other packages!!!
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[3, 1] <- timing
cat(c("Sorting of 7,000,000 random values__________________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (4)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- Rnorm(2800*2800); dim(a) <- c(2800, 2800)
  invisible(gc())
  timing <- system.time({
    b <- crossprod(a)		# equivalent to: b <- t(a) %*% a
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[4, 1] <- timing
cat(c("2800x2800 cross-product matrix (b = a' * a)_________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (5)
cumulate <- 0; c <- 0; qra <-0
for (i in 1:runs) {
  a <- new("dgeMatrix", x = Rnorm(2000*2000), Dim = as.integer(c(2000,2000)))
  b <- as.double(1:2000)
  invisible(gc())
  timing <- system.time({
    c <- solve(crossprod(a), crossprod(a,b))
  })[3]
  cumulate <- cumulate + timing
  
  # This is the old method
  #a <- Rnorm(600*600); dim(a) <- c(600,600)
  #b <- 1:600
  #invisible(gc())
  #timing <- system.time({
  #  qra <- qr(a, tol = 1e-7);
  #  c <- qr.coef(qra, b)
  #  #Rem: a little faster than c <- lsfit(a, b, inter=F)$coefficients
  #})[3]
  #cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[5, 1] <- timing
cat(c("Linear regr. over a 3000x3000 matrix (c = a \\ b')___ (sec): ", timing, "\n"))
remove("a", "b", "c", "qra")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

times[ , 1] <- sort(times[ , 1])
cat("                      --------------------------------------------\n")
cat(c("                 Trimmed geom. mean (2 extremes eliminated): ", exp(mean(log(times[2:4, 1]))), "\n\n"))

cat("   II. Matrix functions\n")
cat("   --------------------\n")
if (R.Version()$os == "Win32") flush.console()

# (1)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- Rnorm(2400000)
  invisible(gc())
  timing <- system.time({
    b <- fft(a)
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[1, 2] <- timing
cat(c("FFT over 2,400,000 random values____________________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (2)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- array(Rnorm(600*600), dim = c(600, 600))
  # Only needed if using eigen.Matrix(): Matrix.class(a)
  invisible(gc())
  timing <- system.time({
  	b <- eigen(a, symmetric=FALSE, only.values=TRUE)$Value
  	# Rem: on my machine, it is faster than:
  #	 b <- La.eigen(a, symmetric=F, only.values=T, method="dsyevr")$Value
  #	 b <- La.eigen(a, symmetric=F, only.values=T, method="dsyev")$Value
  #  b <- eigen.Matrix(a, vectors = F)$Value
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[2, 2] <- timing
cat(c("Eigenvalues of a 640x640 random matrix______________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (3)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- Rnorm(2500*2500); dim(a) <- c(2500, 2500)
  #Matrix.class(a)
  invisible(gc())
  timing <- system.time({
    #b <- determinant(a, logarithm=F)
    # Rem: the following is slower on my computer!
    # b <- det.default(a)
    b <- det(a)
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[3, 2] <- timing
cat(c("Determinant of a 2500x2500 random matrix____________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (4)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- crossprod(new("dgeMatrix", x = Rnorm(3000*3000),
                       Dim = as.integer(c(3000, 3000))))
  invisible(gc())
  #a <- Rnorm(900*900); dim(a) <- c(900, 900)
  #a <- crossprod(a, a)
  timing <- system.time({
    b <- chol(a)
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[4, 2] <- timing
cat(c("Cholesky decomposition of a 3000x3000 matrix________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (5)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  a <- new("dgeMatrix", x = Rnorm(1600*1600), Dim = as.integer(c(1600, 1600)))
  invisible(gc())
  #a <- Rnorm(400*400); dim(a) <- c(400, 400)
  timing <- system.time({
  #  b <- qr.solve(a)
    # Rem: a little faster than
    b <- solve(a)
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[5, 2] <- timing
cat(c("Inverse of a 1600x1600 random matrix________________ (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

times[ , 2] <- sort(times[ , 2])
cat("                      --------------------------------------------\n")
cat(c("                Trimmed geom. mean (2 extremes eliminated): ", exp(mean(log(times[2:4, 2]))), "\n\n"))

cat("   III. Programmation\n")
cat("   ------------------\n")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (1)
cumulate <- 0; a <- 0; b <- 0; phi <- 1.6180339887498949
for (i in 1:runs) {
  a <- floor(Runif(3500000)*1000)
  invisible(gc())
  timing <- system.time({
    b <- (phi^a - (-phi)^(-a))/sqrt(5)
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[1, 3] <- timing
cat(c("3,500,000 Fibonacci numbers calculation (vector calc)(sec): ", timing, "\n"))
remove("a", "b", "phi")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (2)
cumulate <- 0; a <- 3000; b <- 0
for (i in 1:runs) {
  invisible(gc())
  timing <- system.time({
    b <- rep(1:a, a); dim(b) <- c(a, a);
    b <- 1 / (t(b) + 0:(a-1))
    # Rem: this is twice as fast as the following code proposed by R programmers
    # a <- 1:a; b <- 1 / outer(a - 1, a, "+")
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[2, 3] <- timing
cat(c("Creation of a 3000x3000 Hilbert matrix (matrix calc) (sec): ", timing, "\n"))
remove("a", "b")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (3)
cumulate <- 0; c <- 0
gcd2 <- function(x, y) {if (sum(y > 1.0E-4) == 0) x else {y[y == 0] <- x[y == 0]; Recall(y, x %% y)}}
for (i in 1:runs) {
  a <- ceiling(Runif(400000)*1000)
  b <- ceiling(Runif(400000)*1000)
  invisible(gc())
  timing <- system.time({	  
    c <- gcd2(a, b)                            # gcd2 is a recursive function
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[3, 3] <- timing
cat(c("Grand common divisors of 400,000 pairs (recursion)__ (sec): ", timing, "\n"))
remove("a", "b", "c", "gcd2")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (4)
cumulate <- 0; b <- 0
for (i in 1:runs) {
  b <- rep(0, 500*500); dim(b) <- c(500, 500)
  invisible(gc())
  timing <- system.time({
  	# Rem: there are faster ways to do this
  	# but here we want to time loops (220*220 'for' loops)! 
    for (j in 1:500) {
      for (k in 1:500) {
        b[k,j] <- abs(j - k) + 1
      }
    }
  })[3]
  cumulate <- cumulate + timing
}
timing <- cumulate/runs
times[4, 3] <- timing
cat(c("Creation of a 500x500 Toeplitz matrix (loops)_______ (sec): ", timing, "\n"))
remove("b", "j", "k")
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

# (5)
cumulate <- 0; p <- 0; vt <- 0; vr <- 0; vrt <- 0; rvt <- 0; RV <- 0; j <- 0; k <- 0;
x2 <- 0; R <- 0; Rxx <- 0; Ryy <- 0; Rxy <- 0; Ryx <- 0; Rvmax <- 0
# Calculate the trace of a matrix (sum of its diagonal elements)
Trace <- function(y) {sum(c(y)[1 + 0:(min(dim(y)) - 1) * (dim(y)[1] + 1)], na.rm=FALSE)}
for (i in 1:runs) {
  x <- abs(Rnorm(45*45)); dim(x) <- c(45, 45)
  invisible(gc())
  timing <- system.time({
    # Calculation of Escoufier's equivalent vectors
    p <- ncol(x)
    vt <- 1:p                                  # Variables to test
    vr <- NULL                                 # Result: ordered variables
    RV <- 1:p                                  # Result: correlations
    vrt <- NULL
    for (j in 1:p) {                           # loop on the variable number
      Rvmax <- 0
      for (k in 1:(p-j+1)) {                   # loop on the variables
        x2 <- cbind(x, x[,vr], x[,vt[k]])
        R <- cor(x2)                           # Correlations table
        Ryy <- R[1:p, 1:p]
        Rxx <- R[(p+1):(p+j), (p+1):(p+j)]
        Rxy <- R[(p+1):(p+j), 1:p]
        Ryx <- t(Rxy)
        rvt <- Trace(Ryx %*% Rxy) / sqrt(Trace(Ryy %*% Ryy) * Trace(Rxx %*% Rxx)) # RV calculation
        if (rvt > Rvmax) {
          Rvmax <- rvt                         # test of RV
          vrt <- vt[k]                         # temporary held variable
        }
      }
      vr[j] <- vrt                             # Result: variable
      RV[j] <- Rvmax                           # Result: correlation
      vt <- vt[vt!=vr[j]]                      # reidentify variables to test
    }
  })[3]
  cumulate <- cumulate + timing
}
times[5, 3] <- timing
cat(c("Escoufier's method on a 45x45 matrix (mixed)________ (sec): ", timing, "\n"))
remove("x", "p", "vt", "vr", "vrt", "rvt", "RV", "j", "k")
remove("x2", "R", "Rxx", "Ryy", "Rxy", "Ryx", "Rvmax", "Trace") 
if (R.Version()$os == "Win32" || R.Version()$os == "mingw32") flush.console()

times[ , 3] <- sort(times[ , 3])
cat("                      --------------------------------------------\n")
cat(c("                Trimmed geom. mean (2 extremes eliminated): ", exp(mean(log(times[2:4, 3]))), "\n\n\n"))

cat(c("Total time for all 15 tests_________________________ (sec): ", sum(times), "\n"))
cat(c("Overall mean (sum of I, II and III trimmed means/3)_ (sec): ", exp(mean(log(times[2:4, ]))), "\n"))
remove("cumulate", "timing", "times", "runs", "i")
cat("                      --- End of test ---\n\n")   
