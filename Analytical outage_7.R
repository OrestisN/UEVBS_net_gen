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


# UE-VBS prob vars

avg_n_clust <- 25
p_samp <- 25/100
prob_pres<- p_uevbs(avg_n_clust, p_samp, n=1)

# asume that the average distnce for bus connections is fixed and not affected by intesity measure
#length of this affects the multiple hop results greatly!

avg_radius <- (0.5 + 0.3)/2

k<- 1.380649e-23
temp <- 293 #room temp 
wnpsd <- k*temp
BW <- 1e6
Npower <- wnpsd * BW

CHBW <- 1000e6;
MinDR<- 300e6/CHBW
SINRTH = 2^MinDR - 1

#clust outage vars
radius <- 0.25

#importing numerical outage resutls
loadedresult<- readRDS('iniresults_lowloss_dist_unit.rds')
zeroresvec <- loadedresult[[1]]
oneresvec <- loadedresult[[2]]
tworesvec <- loadedresult[[3]]
threeresvec <- loadedresult[[4]]
fourresvec <- loadedresult[[5]]

#importing SE numerical outage resutls
SEloadedresult<- readRDS("SE_init_results_dist_full_.rds")
ASEn0 <- SEloadedresult[[1]]
ASEn1 <- SEloadedresult[[2]]
ASEn2 <- SEloadedresult[[3]]
ASEn3 <- SEloadedresult[[4]]
ASEn4 <- SEloadedresult[[5]]

#importing EE numerical outage resutls
EEloadedresult<- readRDS("fixed_EE_init_results_low_dist_loss_175_0000003.rds")
AEEn0 <- EEloadedresult[[1]]
AEEn1 <- EEloadedresult[[2]]
AEEn2 <- EEloadedresult[[3]]
AEEn3 <- EEloadedresult[[4]]
AEEn4 <- EEloadedresult[[5]]

#loop varying intesity in network
lambda <- seq(from = 0.1, to = 6, by = 0.1)
lambdan <- seq(0.5,6,0.5)
outages0 <- c()
outages1 <- c()
outages2 <- c()
outages3 <- c()
outages4 <- c()
ASE0 <- c()
ASE1 <- c()
ASE2 <- c()
ASE3 <- c()
ASE4 <- c()
EE0 <- c()
EE1 <- c()
EE2 <- c()
EE3 <- c()
EE4 <- c()

for (n in 0:4){
  outages <-c()
  ASE <- c()
  Rates <- c()
  if (n==0){
    init_radius <- 0.9
  }else{
    init_radius <- 0.5
  }
  
  loss <- init_radius ^ (alpha)
  Spower <- (ptrans*gain_t*gain_r)/loss
  Spower_init <-((ptrans*1)*gain_t*gain_r)/loss
  for (i in lambda){
    #attempt on average distance loss depending on intensirty of PPP
    # loss <- gamma(alpha/2+1)/(pi*i)^(alpha/2)
    # Spower <- (ptrans*gain_t*gain_r)/loss
    # Spower_init <- ((ptrans*1)*gain_t*gain_r)/loss
    
    Ipower <- i*(pi+(pi/alpha-2)*(1-(init_radius^(2-alpha))))
    SINR = Spower_init/(Npower + Ipower)
    
    out0 <- init_outage_r(SINR,SINRTH)
    out1 <- mid_outage(n,avg_radius,i,alpha,Npower,prob_pres,Spower,SINRTH)
    out2 <- clust_outage(radius,i,alpha,Npower,prob_pres,Spower,SINRTH)
    out <- 1-((1-out0)*(1-out1)*(1-out2))
    
    #multiplied by 25 because of thomas-clustering, 
    #this is the average amount of 2d gaussian points per PPP point
    SE <- 25*i*log2(1+SINRTH) * (1-out)
    
    #makes sense that it should be i*25 in the below equation as well
    Rate <- (BW*i*25*log2(1+SINRTH)*(1-out))/(i*25)
    
    outages <- append(outages,out)
    ASE <- append(ASE,SE)
    Rates <- append(Rates,Rate)
  }
  if (n==0){
    ASE0 <- ASE
    outages0 <- outages
    EE0 <- Rates
  }else if (n==1){
    ASE1 <- ASE
    outages1 <- outages
    EE1 <- Rates
  }else if (n==2){
    ASE2 <- ASE
    outages2 <- outages
    EE2 <- Rates
  }else if (n==3){
    ASE3 <- ASE
    outages3 <- outages
    EE3 <- Rates
  }else if (n==4){
    ASE4 <- ASE
    outages4 <- outages
    EE4 <- Rates
  }
}


#plotting results
plot(lambda,outages1, type="l",col="blue", xlim =c(0,6.2), ylim=c(0,1.1),
     ylab="Outage Probability",
     xlab="PPP Intensity")
lines(lambda,outages0, col="purple")
lines(lambda,outages2, col="black")
lines(lambda,outages3, col="green")
lines(lambda,outages4, col="red")
legend(4, 0.5, legend=c("n=1", "n=2", "n=3","n=4","n=5"),
       col=c("purple","blue", "black","green","red"), lty=1:1, cex = 0.8)
lines(seq(0.5,6,0.5),zeroresvec,type="o",lty=2, lwd=1, col="purple")
lines(seq(0.5,6,0.5),oneresvec,type="o",lty=2, lwd=1, col="blue")
lines(seq(0.5,6,0.5),tworesvec,type="o",lty=2, lwd=1, col="black")
lines(seq(0.5,6,0.5),threeresvec,type="o",lty=2, lwd=1, col="green")
lines(seq(0.5,6,0.5),fourresvec,type="o", lty=2, lwd=1,col="red")
grid()

#ploting ASE
plot(lambda,ASE1, type="l",col="blue", xlim = c(0,6.2), ylim = c(0,4),
     ylab="SE per OFDM channel [bit/s/Hz]",
     xlab="PPP Intensity")
lines(lambda,ASE0, col="purple")
lines(lambda,ASE2, col="black")
lines(lambda,ASE3, col="green")
lines(lambda,ASE4, col="red")

lines(seq(0.5,6,0.5),ASEn0,type = "o",lty = 2, lwd=1, col="purple")
lines(seq(0.5,6,0.5),ASEn1,type = "o",lty = 2, lwd=1, col="blue")
lines(seq(0.5,6,0.5),ASEn2,type = "o",lty = 2, lwd=1, col="black")
lines(seq(0.5,6,0.5),ASEn3,type = "o",lty = 2, lwd=1, col="green")
lines(seq(0.5,6,0.5),ASEn4,type = "o", lty = 2, lwd=1,col="red")
legend(4, 4, legend=c("n=1", "n=2", "n=3","n=4","n=5"),
     col=c("purple","blue", "black","green","red"), lty=1:1, cex = 0.8)
grid()

#ploting rates
plot(lambda,EE1, type="l",col="blue",xlim = c(0,6.2), ylim = c(0,300e3),
     ylab="EE of UEVBS [bit/J]",
     xlab="PPP Intensity")
lines(lambda,EE0, col="purple")
lines(lambda,EE2, col="black")
lines(lambda,EE3, col="green")
lines(lambda,EE4, col="red")
lines(seq(0.5,6,0.5),AEEn0,type = "o",lty = 2, lwd=1, col="purple")
lines(seq(0.5,6,0.5),AEEn1,type = "o",lty = 2, lwd=1, col="blue")
lines(seq(0.5,6,0.5),AEEn2,type = "o",lty = 2, lwd=1, col="black")
lines(seq(0.5,6,0.5),AEEn3,type = "o",lty = 2, lwd=1, col="green")
lines(seq(0.5,6,0.5),AEEn4,type = "o", lty = 2, lwd=1,col="red")
legend(4, 300e3, legend=c("n=1", "n=2", "n=3","n=4","n=5"),
       col=c("purple","blue", "black","green","red"), lty=1:1, cex = 0.8)
grid()

#cross-correlation of all  results
outlist <- list(list(zeroresvec,oneresvec,tworesvec,threeresvec,fourresvec),
                list(ASEn0,ASEn1,ASEn2,ASEn3,ASEn4),
                list(AEEn0,AEEn1,AEEn2,AEEn3,AEEn4))
ASEres <- list(list(outages0,outages1,outages2,outages3,outages4),
               list(ASE0,ASE1,ASE2,ASE3,ASE4),
               list(EE0,EE1,EE2,EE3,EE4))
for (n in seq(3)){
  xcorr_vec=c()
  for (i in seq(5)){
    allres <- ccf(ASEres[[n]][[i]][seq(1,60,5)],outlist[[n]][[i]], lag.max = 0, type = c("correlation"),plot = FALSE)
    xcorr_vec <- append(xcorr_vec,allres$acf[1])
  }
  print(xcorr_vec)
  print(mean(xcorr_vec))
  
}
