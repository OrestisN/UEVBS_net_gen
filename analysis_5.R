# GENERAL ANALYSIS fucntions buggy and needs to be modified

# gen_stats <- function(net_obj,l_all_devos){
#   
#   net_cntr <- 0
#   net_ue_cntr <- 0
#   net_uevbs_cntr <- 0
#   clust_dev_cntr <- 0
#   stem_dev_cntr <- 0
#   
#   net_ue_cntr <- length(net_obj$ue_net$dev_list)
#   print(paste("UE devices directly connected to BS:",net_ue_cntr))
#   for (i in net_obj$uevbs_net$tree_nets){
#     
#     for (q in i$stem){
#       
#       stem_dev_cntr <- stem_dev_cntr +length(q$dev_list)
#     }
#     for (k in i$clust){
#       
#       clust_dev_cntr<- clust_dev_cntr + length(k$dev_list)
#     }
#   }
#   print(paste("Amount of uevbs devices in stem networks:",stem_dev_cntr))
#   print(paste("Amount of uevbs devices in cluster networks:",clust_dev_cntr))
#   net_uevbs_cntr <- stem_dev_cntr + clust_dev_cntr
#   print(paste("Amount of uevbs devices in our generated network:",net_uevbs_cntr))
#   net_cntr <- net_ue_cntr + net_uevbs_cntr
#   print(paste("Amount of  devices in our generated network:",net_cntr))
#   avg_nhops <- stem_dev_cntr/(length(net_obj$uevbs_net$tree_nets))
#   print(paste("Average amount of relay hops:",avg_nhops))
#   avg_nclust <- clust_dev_cntr/(length(net_obj$uevbs_net$tree_nets))
#   print(paste("Average amount of devices in our clusters:",avg_nclust))
#   #need to add the amount of channels that reach BS with and without the UE-VBS system.
#   
#   # this doesnt take into consideration devices in bad coverage areas (triangles)
#   # need to fix this
#   prox_dev_cntr <- 0
#   for (z in l_all_devos){
#     
#     init_dist <- sqrt((z$x)^2 + (z$y)^2)
#     if (init_dist <=1){
#       
#       prox_dev_cntr <- prox_dev_cntr +1
#     }
#   }
#   dev_diff <- net_cntr - prox_dev_cntr
#   print(paste("Amount of extra devices reached due to the UE-VBS system (when compared to a non-uevbs system):",dev_diff))
# } 
# 
# 
# all_dev_list <-append(list_oued,list_ouevbsd)
# gen_stats(network.0,all_dev_list)
# network.0<-test


PL_ABG_model <- function(dist, BW = 10e9){
  #ABG model for urban macro (45m-1174m)
  # BW <- needs to be a bandwidth in accordance to OFDM channels [in GHz]

  alpha <- 3.3
  beta <- 17.6
  gamma <- 2
  #sf_sd <- 9.9
  sf_sd <- 0
  
  Loss <- 10*alpha*log10(dist) + beta + 10*gamma*log10(BW / 1e9) + sf_sd
  
  return(Loss)
}

#Signal power to normally connected devices  
UES <- function(net_obj,Ptransmit,Gtramsmit,Greceive){
  prue <- c()
  for (i in net_obj$ue_net$dev_list){
    disto <- sqrt((0 - i$x)^2 + (0 - i$y)^2)
    loss <- PL_ABG_model(disto)
    Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
    prue <-append(prue,Pr)
  }
  return(prue)
}  

#Interference power from other UEVBS devices, to normally connected devices  
UEI <- function(net_obj,Ptransmit,Gtramsmit,Greceive){
  Irue <- c()
  for (i in net_obj$ue_net$dev_list){
    Idev <- c()
    for (k in net_obj$uevbs_net$tree_nets){
      for (q in k$stem[[1]]$dev_list){
      disto <- sqrt((q$x - i$x)^2 + (q$y - i$y)^2)
      loss <- PL_ABG_model(disto)
      Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
      Idev <-append(Idev,Pr)
      }
    }
    Irue <- append(Irue,sum(Idev))
  }
  return(Irue)
}

#Signal power to UEVBS connected devices  
UEVBStreeS <-function(net_obj,Ptransmit,Gtramsmit,Greceive){
  PUEVBS <- list()
  for (i in net_obj$uevbs_net$tree_nets){
    stemdevs<-c()
    flag<-TRUE
    for (k in i$stem[[1]]$dev_list ){
      if (flag == TRUE){
        disto <- sqrt((0 - k$x)^2 + (0 - k$y)^2)
        flag <- FALSE
      }else{
        disto <- sqrt((prevx - k$x)^2 + (prevy - k$y)^2)
      }
      loss <- PL_ABG_model(disto)
      Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
      stemdevs <-append(stemdevs,Pr)
      prevx<-k$x
      prevy<-k$y
    }
    clustdevs<-c()
    for (q in i$clust[[1]]$dev_list ){
      disto <- sqrt((k$x - q$x)^2 + (k$y - q$y)^2)
      loss <- PL_ABG_model(disto)
      Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
      clustdevs <-append(clustdevs,Pr)
      
    }
    treeS <- list(stemdevs,clustdevs)
    PUEVBS[[length(PUEVBS)+1]] <- treeS
  }
  return(PUEVBS)
} 


#Interference power from other UEVBS devices, to UEVBS connected devices  
UEVBSintetree <- function(net_obj,Ptransmit,Gtramsmit,Greceive){
  IUEVBSreslist <- list()
  for (i in net_obj$uevbs_net$tree_nets){
    treecheck <- attr(i,"class")
    stemdevs <- c()
    clustdevs<-c()
    intvec<-c()
    for (k in i$stem[[1]]$dev_list){
      tdevcheck <- attr(k,"class")
      treelen <- length(i$stem[[1]]$dev_list)
      if ((treelen!=1) & (tdevcheck != attr(i$stem[[1]]$dev_list[[1]],"class"))){
        disto <- sqrt((0 - k$x)^2 + (0 - k$y)^2)
        loss <- PL_ABG_model(disto)
        Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
        intvec<-append(intvec,Pr)
      }
      for(a in net_obj$uevbs_net$tree_net){
        tnow <- attr(a,"class")
        for (b in a$stem[[1]]$dev_list){
          dnow <- attr(b, "class")
          devlist <- lapply(a$stem[[1]]$dev_list, attr, which = "class")
          if (tnow==treecheck){
            if (dnow==tdevcheck){
              Pr<-0
            }else if (dnow == devlist[match(devlist,tdevcheck)-1]){
              Pr<-0
            }
          }else{
            disto <- sqrt((b$x - k$x)^2 + (b$y - k$y)^2)
            loss <- PL_ABG_model(disto)
            # print(disto)
            Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
          }
          intvec <- append(intvec,Pr)
        }
      }
      stemdevs <- append(stemdevs,sum(intvec))
    }
    intvec<-c()
    for (q in i$clust[[1]]$dev_list){
      cdevcheck <- attr(k,"class")
      
      disto <- sqrt((0 - q$x)^2 + (0 - q$y)^2)
      loss <- PL_ABG_model(disto)
      # print(disto)
      Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
      intvec <- append(intvec,Pr)
      for(a in net_obj$uevbs_net$tree_net){
        tnow <- attr(a,"class")
        for (b in a$stem[[1]]$dev_list){
          dnow<-attr(b,"class")
          if ((tnow==treecheck) & 
              (dnow==attr(a$stem[[1]]$dev_list[[length(a$stem[[1]]$dev_list)]],"class"))){
            Pr<-0
          }else{
            disto <- sqrt((b$x - q$x)^2 + (b$y - q$y)^2)
            loss <- PL_ABG_model(disto)
            # print(disto)
            Pr <- (Ptransmit*Gtramsmit*Greceive*rexp(1))/(10^(loss/10))
          }
          intvec <- append(intvec,Pr)
        }
      }
      clustdevs<-append(clustdevs,sum(intvec))
    }
    treeS <- list(stemdevs,clustdevs)
    IUEVBSreslist[[length(IUEVBSreslist)+1]] <- treeS
  }
  return(IUEVBSreslist)
}


#calculates the ratio of devices over a threshold SINR
NumOtage <- function(sue,iue,suevbs,iuevbs,thresh){
#NumOtage <- function(suevbs,iuevbs,thresh){
  # covered <- 0
  outage <- 0
  
  k<- 1.380649e-23
  temp <- 293 #room temp 
  wnpsd <- k*temp
  BW <- 1000e6
  Npower <- wnpsd * BW

  
  SINRue<-sue/(Npower+iue)
  
  SIRNuevbs <- list()
  if (length(suevbs)>0){
    for (i in seq(1,length(suevbs))){
      # print(seq(1,length(suevbs)))
      tSINRuevbs <- suevbs[[i]][[1]] / (iuevbs[[i]][[1]] + Npower)
      SIRNuevbs <- append(SIRNuevbs,tSINRuevbs)
      
      cSINRuevbs <- suevbs[[i]][[2]] / (iuevbs[[i]][[2]] + Npower)
      SIRNuevbs <- append(SIRNuevbs,cSINRuevbs)
    }
  }
  SINR <- append(SINRue, SIRNuevbs)
  #print(SIRNuevbs)
  #SINR <- SIRNuevbs
  boollist <- SINR < thresh
  
  for (r in boollist){
    if (r ==TRUE){
      outage <- outage +1
    # }else{
    #   covered <- covered + 1
    }
  }
  outageratio <-outage/length(boollist) 
  return(outageratio)
}
BW <- 1000e6
MinDR<- 300e6/BW
SINRTH = 2^MinDR - 1


#iterate through saved networks
zeroresvec<-c()
oneresvec<-c()
tworesvec<-c()
threeresvec<-c()
fourresvec<-c()
files <- c("zero hop","one hop","two hop","three hop","four hop")
for (d in files){
  fileslist <- list.files(path=d,pattern="int")
  resvec<-c()
  for (f in fileslist){
    avgavgvec<-c()
    for (n in list.files(paste(d,"\\",f,sep=""))){
      avgvec<-c()
      obj <- readRDS(paste(d,"\\",f,"\\",n,sep=""))
      #check for empty networks
      if ((length(obj$ue_net$dev_list)==0) & (length(obj$uevbs_net$tree_nets)==0)){
        avgvec <- append(avgvec,0)
      }else{
        for (p in seq(1,20)){
          #Calculate all power and inteference lists
          Snorm <- UES(obj,1,1,1)
          Inorm <- UEI(obj,1,1,1)
          Suevbs <- UEVBStreeS(obj,1,1,1)
          Iuevbs <- UEVBSintetree(obj,1,1,1)
          
          numout <- NumOtage(Snorm,Inorm,Suevbs,Iuevbs,SINRTH)
          if (is.nan(numout)){
            numout<-0
          }
          avgvec <- append(avgvec,numout)
        }
      }
      avgavgvec <- append(avgavgvec,mean(avgvec))
    }
    resvec <- append(resvec,mean(avgavgvec))
    print(resvec)
  }
  if (d==files[1]){
    zeroresvec<-resvec
  }else if (d==files[2]){
    oneresvec<-resvec
  }else if (d==files[3]){
    tworesvec<-resvec
  }else if (d==files[4]){
    threeresvec<-resvec
  }else if (d==files[5]){
    fourresvec<-resvec
  }
}

saveRDS(list(zeroresvec,oneresvec,tworesvec,threeresvec,fourresvec),"iniresults_lowloss_verylow.rds")