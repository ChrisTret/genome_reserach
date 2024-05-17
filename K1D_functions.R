library(compiler)
k1d.s <- function(X,start,end,stepsize=1,nstep=51){
  # shift data to start at 0
  X <- X-start+1
  
  Gn <- length(X[1,])
  n <- array(NA,Gn)
  # Determine length of record
  for(i in 1:Gn){
    n[i] <- length(X[!is.na(X[,i]),i])
  }
  
  alen <- abs(end-start)+1
  
  # print(paste("Record length =",alen))
  # dmax = maximum distance for analysis
  
  dmax <- alen/2
  nh <- floor(dmax/stepsize)+1
  dmax <- nh*stepsize
  
  if(nstep*stepsize>(alen/2)){
    nstep <- nh
    print(paste("nstep reduced to ",nstep))
  }
  dmax<-(nstep-1)*stepsize
  h <- seq(0,dmax,stepsize)
  Khat <- array(NA,nstep)
  Lhat <- array(NA,nstep)
  Ka <- matrix(0,nstep,Gn)
  
  for(a in 1:Gn){
    for(i in 1:n[a]){
      for(k in 1:Gn){
        if(k!=a){
          for(j in 1:n[k]){
            # find distance between two events
            dist <- abs(X[i,a]-X[j,k])
            if(dist<=dmax){
              # find the bin of dist in the h vector
              q <- k1d.find.place(dist,h)
              w <- k1d.edge.correction(X[i,a],dist,alen)
              # put the weights in the right bin
              Ka[q,a] <- Ka[q,a] + w
            } #end if
          } #next j
        } #end if
      } #next k
    } #next i
  } #next a
  
  # make cumulative functions
  for(a in 1:Gn){
    for(i in 2:nstep){
      Ka[i,a] <- Ka[i,a] + Ka[i-1,a]
    }
  }
  
  # Scale the function by alen/ n(a)*sum( n(i))
  for(a in 1:Gn){
    denom <- 0
    for(k in 1:Gn){
      if(a!=k){
        denom <- denom+n[k]
      }
    }
    denom <- denom*(n[a])
    
    for(i in 1:nstep){
      Ka[i,a] <- Ka[i,a]*alen/denom
    }
  }  
  
  # Combine functions 
  denom <- 0
  for(k in 1:Gn){
    for(a in 1:Gn){
      if(a!=k){
        denom <- denom+n[a]
      }
    }
  }
  
  for(i in 1:nstep){     
    numer <- 0
    for(a in 1:Gn){
      nsum <- 0
      for(k in 1:Gn){
        if(k!=a){
          nsum <- nsum+n[k]
        }
      }
      numer <- numer+nsum*Ka[i,a]
    }
    Khat[i] <- numer/denom
    Lhat[i] <- Khat[i]/2-h[i]-0.5   #still need to determine why -0.5 is needed
  }
  
  k1d.out <- list(X,alen,stepsize,nstep,start,end,h,Khat,Lhat,Gn,n,dmax)
  names(k1d.out) <- c("X","record.length","stepsize","nstep","start","end","h","Khat","Lhat","Gn","n","dmax")
  if(exists("k1d.out")){
    return(k1d.out)
  } else {
    print("Error: No output produced")
    return(NULL)
  }
}

k1d <- cmpfun(k1d.s)



#	********************************************************************************
#	FUNCTION K1D2  
#	This function calculates the bivariate Ripley K statistic on one dimension. 
#	Integer data required.  This is function k1d modified to run for the simulations.
#	********************************************************************************

k1d2.s <- function(X,start,end,stepsize,nstep,Gn,n,dmax,h){
  
  alen <- abs(end-start)+1
  Khat <- array(NA,nstep)
  Lhat <- array(NA,nstep)
  Ka <- matrix(0,nstep,Gn)
  
  for(a in 1:Gn){
    for(i in 1:n[a]){
      for(k in 1:Gn){
        if(k!=a){
          for(j in 1:n[k]){
            # find distance between two events
            dist <- abs(X[i,a]-X[j,k])
            if(dist<=dmax){
              # find the bin of dist in the h vector
              q <- k1d.find.place(dist,h)
              w <- k1d.edge.correction(X[i,a],dist,alen)
              # put the weights in the right bin
              Ka[q,a] <- Ka[q,a] + w
            } #end if
          } #next j
        } #end if
      } #next k
    } #next i
  } #next a
  
  # make cumulative functions
  for(a in 1:Gn){
    for(i in 2:nstep){
      Ka[i,a] <- Ka[i,a] + Ka[i-1,a]
    }
  }
  
  # Scale the function by alen/ n(a)*sum( n(i))
  for(a in 1:Gn){
    denom <- 0
    for(k in 1:Gn){
      if(a!=k){
        denom <- denom+n[k]
      }
    }
    denom <- denom*(n[a])
    
    for(i in 1:nstep){
      Ka[i,a] <- Ka[i,a]*alen/denom
    }
  }  
  
  # Combine functions 
  denom <- 0
  for(k in 1:Gn){
    for(a in 1:Gn){k
      if(a!=k){
        denom <- denom+n[a]
      }
    }
  }
  
  for(i in 1:nstep){     
    numer <- 0
    for(a in 1:Gn){
      nsum <- 0
      for(k in 1:Gn){
        if(k!=a){
          nsum <- nsum+n[k]
        }
      }
      numer <- numer+nsum*Ka[i,a]
    }
    Khat[i] <- numer/denom
    Lhat[i] <- Khat[i]/2-h[i]-0.5   #still need to determine why -0.5 is needed
  }
  
  return(Lhat)
}

k1d2 <- cmpfun(k1d2.s)

# ********************************************************************************
# Function k1d.edge.correction
# This function computes the edge correction for Kr estimator of K-function
# for a given event at v and radius u,
# assuming that the study region is linear with length alen.
# Modification of CRESSIE (1991) p.640 .
# ********************************************************************************/
# v=location along a
# d1=distance from v to nearest edge
# u=distance to comparison point

k1d.edge.correction.s <- function(v,u,alen){
  
  if(v > 0.5*alen){
    d1 <- alen-v
  } else {
    d1 <- v
  }
  # If closer to edge than to comparison point
  if(d1 < u){
    w <- 0.5
    # w <- (u+d1)/(2*u)
  } else {
    w <- 1
  }
  
  return(1/w)
  
}
k1d.edge.correction<-cmpfun(k1d.edge.correction.s)

# ********************************************************************************
# Function k1d.find.place
# This function find the FIRST position in the h vector
# such as the distance between events is less or equal
# to the value of h(i)
# ********************************************************************************/

k1d.find.place.s <- function(dist,h){
  
  i<-1
  repeat{
    if(dist <= h[i]){break}
    i <- i+1
  }
  
  return(i)
}

k1d.find.place <- cmpfun(k1d.find.place.s)

# ********************************************************************************
# FUNCTION:  k1d.sim
# Computes the ENVELOPES for Ripley's K-function
# (estimator of Stoyan, 1984), for 2 types of processes.
# Circular scenario:  Shifts series 2 all subsequent 
# series by a random amount in length a.
# Randomization scenario:  Chooses random dates for all events 
# in series 2 and all subsequent series.
#
# The circular-shift envelope tests the Null Hypothesis
# of process 1 being independent of process 2 (and all subsquent processes),
# while preserving the temporal pattern of each process.
# The randomization envelope tests the Null Hypothesis
# of process 1 being independent of process 2 if process 2 dates are 
# randomized.
# rcol=vector of length 2, first value indicates whether to randomize record 1,
# second value indicates whether to randomize records 2 and greater
# ********************************************************************************/

k1d.sim <- function(k1d.obj,rcol=c(TRUE,TRUE),scenario="circular",nsim=200,perc=c(0.05,0.95)){
  
  if(scenario=="circular"){
    sim.out <- circular.sim(k1d.obj,rcol,nsim,perc)
  }
  if(scenario=="random"){
    sim.out <- random.sim(k1d.obj,rcol,nsim,perc)
  }
  return(sim.out)
}

# ********************************************************************************
# FUNCTION:  circular.sim
# Randomizes a vector by shifting values a random amount and wrapping around from end
# to the start (i.e., circular for 1D rather than toroidal for 2D)
# ********************************************************************************

circular.sim.s <- function(k1d.obj,rcol,nsim,perc){
  X<-k1d.obj$X
  start<-k1d.obj$start
  end<-k1d.obj$end
  stepsize<-k1d.obj$stepsize
  nstep<-k1d.obj$nstep
  alen <- k1d.obj$record.length
  h <- k1d.obj$h
  Gn <- k1d.obj$Gn
  dmax<-k1d.obj$dmax
  n<-k1d.obj$n
  
  Xsim <- X
  simk <- matrix(NA,nsim,nstep) #holds all simulation results
  for(s in 1:nsim){
    if(rcol[1]){
      W <- X[!is.na(X[,1]),1]
      sim <- W
      shift <- sample(1:alen,1)
      for(i in 1:length(W)){
        if((shift+W[i])>alen){
          sim[i] <- shift+W[i]-alen
        } else {
          sim[i] <- shift+W[i]
        }
        Xsim[,1] <- append(sim,rep(NA,(length(X[,1])-length(W))))
      }
    }
    if(rcol[2]){
      for(a in 2:Gn){
        W <- X[!is.na(X[,a]),a]
        sim <- W
        shift <- sample(1:alen,1)
        for(i in 1:length(W)){
          if((shift+W[i])>alen){
            sim[i] <- shift+W[i]-alen
          } else {
            sim[i] <- shift+W[i]
          }
        }
        Xsim[,a] <- append(sim,rep(NA,(length(X[,1])-length(W))))
      } #next a
    } #end if rcol2
    simk[s,] <- k1d2(Xsim,start,end,stepsize,nstep,Gn,n,dmax,h)
    
  } #next s
  
  ##output confidence intervals
  
  o <- matrix(NA,2,nstep)
  for(i in 1:nstep){
    o[,i]<-quantile(simk[,i],perc)
  }
  return(o)
  
}

circular.sim <- cmpfun(circular.sim.s)

# ********************************************************************************
# FUNCTION:  random.sim
# Randomizes a vector by selecting random points without replacement
# 
# ********************************************************************************

random.sim.s <- function(k1d.obj,rcol,nsim,perc){
  X<-k1d.obj$X
  start<-k1d.obj$start
  end<-k1d.obj$end
  stepsize<-k1d.obj$stepsize
  nstep<-k1d.obj$nstep
  alen <- k1d.obj$record.length
  h <- k1d.obj$h
  Gn <- k1d.obj$Gn
  dmax<-k1d.obj$dmax
  n<-k1d.obj$n
  
  Xsim <- X
  simk <- matrix(NA,nsim,nstep) #holds all simulation results
  for(s in 1:nsim){
    if(rcol[1]){
      Xsim[,1] <- append(sample(1:alen,n[1]),rep(NA,(length(X[,1])-n[1])))
    }
    if(rcol[2]){
      for(a in 2:Gn){
        Xsim[,a] <- append(sample(1:alen,n[a]),rep(NA,(length(X[,1])-n[a])))
      } #next a
    } #end if rcol2
    simk[s,] <- k1d2(Xsim,start,end,stepsize,nstep,Gn,n,dmax,h)
    
  } #next s
  
  ##output confidence intervals
  
  o <- matrix(NA,2,nstep)
  for(i in 1:nstep){
    o[,i]<-quantile(simk[,i],perc)
  }
  return(o)
}

random.sim <- cmpfun(random.sim.s)

## *******
# Short function to help plot the Lhat function along with the raw data in a two-panel plot
#######
plot.k1d <- function(k1d.obj){
  par(mfrow=c(2,1),mar=c(4,4,1,1))
  plot(c(t$start,t$end),c(1,t$Gn),xlim=c(t$start,t$end),ylim=c(1,t$Gn),type='n',axes="F",ylab="",xlab="")
  axis(2,labels=names(X),at=1:t$Gn,las=1)
  axis(1)
  for(i in 1:k1d.obj$Gn){
    ev <- k1d.obj$X[,i][!is.na(k1d.obj$X[,i])]
    ev2 <- ev+k1d.obj$start-1
    points(ev2,rep(i,k1d.obj$n[i]),cex=0.5)
  }
  plot(k1d.obj$h,k1d.obj$Lhat,type="l",ylim=c(-5,5),xlab="Temporal scale (years)",ylab="Lhat")
} 	
