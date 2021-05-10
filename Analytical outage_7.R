#function to predict the porbability of a uevbs device being inside a ckluster (binomial distribution)
p_uevbs <- function(avg_n_clust, p_samp, n=1){
  p_list=c()
  for (i in n:avg_n_clust){
    pprob = dbinom(i, size = avg_n_clust, prob=p_samp) 
    p_list<-append(p_list,pprob)
  }
  return(sum(p_list))
}

#initial Rayleigh outage probability
init_outage_r <- function(SINR,SINRTH){
  res = 1 - exp(-SINRTH/SINR)
  return(res)
}
#Mid(multihop)  outage probability
mid_outage <- function(n,avg_radius,intensity,alpha,Npower,prob_pres,Spower,SINRTH){
  
  outlist = c()
  cnt <- 0
  while (cnt <= n){
    ps = exp(-SINRTH*(avg_radius^alpha)*(Npower/Spower))*
      exp(-pi*intensity*(SINRTH^(2/alpha))*(avg_radius^2)*gamma(1+(2/alpha))*gamma(1-(2/alpha)))
    pout = 1 - (prob_pres*ps)
    outlist<-append(outlist,(1-pout))
    cnt <- cnt + 1
  }
  fpout = 1 - prod(outlist)
  if (n==0){
    return(0)
  }else{
    return(fpout)
  }
}
#outage probability in a cluster (simillar theory to mid outage)

clust_outage <- function(radius,intensity,alpha,Npower,prob_pres,Spower,SINRTH){
  ps = exp(-SINRTH*(radius^alpha)*(Npower/Spower))*
    exp(-pi*intensity*(SINRTH^(2/alpha))*(radius^2)*gamma(1+(2/alpha))*gamma(1-(2/alpha)))
  pout = 1 - (prob_pres*ps)
  # print(pout)
  return(pout)
}

#assume mode of rayleigh fading is 1

alpha <- 4
gain_t <- 1
gain_r <- 1
ptrans <- 1
init_radius <- 0.5
loss <- init_radius ^ (alpha)
Spower <- (ptrans*gain_t*gain_r)/loss


# UE-VBS prob vars

avg_n_clust <- mean_amount_dp
p_samp <- uevbs_per/100
prob_pres<- p_uevbs(avg_n_clust, p_samp, n=1)

# asume that the average distnce for bus connections is fixed and not affected by intesity measure
#length of this affects the multiple hop results greatly!

avg_radius <- (max_range+min_range)/3

k<- 1.380649e-23
temp <- 293 #room temp 
wnpsd <- k*temp
BW <- 1000e6
Npower <- wnpsd * BW

CHBW <- 1000e6;
MinDR<- 300e6/CHBW
SINRTH = 2^MinDR - 1

#clust outage vars
radius <- clust_range

#loop varying intesity in network

lambda <- seq(from = 0.1, to = 6, by = 0.1)
outages0 <- c()
outages1 <- c()
outages2 <- c()
outages3 <- c()
outages4 <- c()

for (n in 0:4){
  outages <-c()
  for (i in lambda){
    
    Ipower <- i*(pi+(pi/alpha-2)*(1-(init_radius^(2-alpha))))
    SINR = Spower/(Npower + Ipower)
    
    out0 <- init_outage_r(SINR,SINRTH)
    out1 <- mid_outage(n,avg_radius,i,alpha,Npower,prob_pres,Spower,SINRTH)
    out2 <- clust_outage(radius,i,alpha,Npower,prob_pres,Spower,SINRTH)
    out <- 1-((1-out0)*(1-out1)*(1-out2))
    
    outages<-append(outages,out)
  }
  
  if (n==0){
    outages0 <- outages
  }else if (n==1){
    outages1 <- outages
  }else if (n==2){
    outages2 <- outages
  }else if (n==3){
    outages3 <- outages
  }else if (n==4){
    outages4 <- outages
  }
}
outlist=list(outages0,outages1,outages2,outages3,outages4)

#plotting results

plot(lambda,outages1, type="l",col="blue", xlim =c(0,6.2), ylim=c(0,1.1),
     ylab="Outage Probability",
     xlab="Intensity")
lines(lambda,outages0, col="purple")
lines(lambda,outages2, col="black")
lines(lambda,outages3, col="green")
lines(lambda,outages4, col="red")
legend(4, 0.5, legend=c("n=1", "n=2", "n=3","n=4","n=5"),
       col=c("purple","blue", "black","green","red"), lty=1:1, cex = 0.8)
loadedresult<- readRDS('iniresults_norm.rds')
zeroresvec <- loadedresult[[1]]
oneresvec <- loadedresult[[2]]
tworesvec <- loadedresult[[3]]
threeresvec <- loadedresult[[4]]
fourresvec <- loadedresult[[5]]
lines(seq(0.5,6,0.5),zeroresvec,type="o",lty=2, lwd=1, col="purple")
lines(seq(0.5,6,0.5),oneresvec,type="o",lty=2, lwd=1, col="blue")
lines(seq(0.5,6,0.5),tworesvec,type="o",lty=2, lwd=1, col="black")
lines(seq(0.5,6,0.5),threeresvec,type="o",lty=2, lwd=1, col="green")
lines(seq(0.5,6,0.5),fourresvec,type="o", lty=2, lwd=1,col="red")
grid()

#only numerical results
# plot(seq(0.5,6,0.5),zeroresvec, type="o",col="purple", xlim =c(0,6.2), ylim=c(0,1.1),
#      # main=expression("Outage analysis of UEVBS System"),
#      ylab="Outage Probability",
#      xlab="Intensity")
# legend(4, 0.5, legend=c("n=1", "n=2", "n=3","n=4","n=5"),
#        col=c("purple","blue", "grey","green","red"), lty=1:1, cex = 0.8)
# lines(seq(0.5,6,0.5),oneresvec,type="o", col="blue")
# lines(seq(0.5,6,0.5),tworesvec,type="o", col="grey")
# lines(seq(0.5,6,0.5),threeresvec,type="o", col="green")
# lines(seq(0.5,6,0.5),fourresvec,type="o", col="red")
# grid()
xcorr_vec=c()
for (i in seq(5)){
  allres <- ccf(loadedresult[[i]],outlist[[i]][seq(5,60,5)], lag.max = 0, type = c("correlation"),plot = FALSE)
  xcorr_vec <- append(xcorr_vec,allres$acf[1])
}
print(xcorr_vec)
print(mean(xcorr_vec))



