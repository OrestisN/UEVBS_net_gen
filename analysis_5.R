#ANALYSIS fucntions

gen_stats <- function(net_obj,l_all_devos){
  
  net_cntr <- 0
  net_ue_cntr <- 0
  net_uevbs_cntr <- 0
  clust_dev_cntr <- 0
  stem_dev_cntr <- 0
  
  net_ue_cntr <- length(net_obj$ue_net$dev_list)
  print(paste("UE devices directly connected to BS:",net_ue_cntr))
  for (i in net_obj$uevbs_net$tree_nets){
    
    for (q in i$stem){
      
      stem_dev_cntr <- stem_dev_cntr +length(q$dev_list)
    }
    for (k in i$clust){
      
      clust_dev_cntr<- clust_dev_cntr + length(k$dev_list)
    }
  }
  print(paste("Amount of uevbs devices in stem networks:",stem_dev_cntr))
  print(paste("Amount of uevbs devices in cluster networks:",clust_dev_cntr))
  net_uevbs_cntr <- stem_dev_cntr + clust_dev_cntr
  print(paste("Amount of uevbs devices in our generated network:",net_uevbs_cntr))
  net_cntr <- net_ue_cntr + net_uevbs_cntr
  print(paste("Amount of  devices in our generated network:",net_cntr))
  avg_nhops <- stem_dev_cntr/(length(net_obj$uevbs_net$tree_nets))
  print(paste("Average amount of relay hops:",avg_nhops))
  avg_nclust <- clust_dev_cntr/(length(net_obj$uevbs_net$tree_nets))
  print(paste("Average amount of devices in our clusters:",avg_nclust))
  prox_dev_cntr <- 0
  for (z in l_all_devos){
    
    init_dist <- sqrt((z$x)^2 + (z$y)^2)
    if (init_dist <=1){
      
      prox_dev_cntr <- prox_dev_cntr +1
    }
  }
  dev_diff <- net_cntr - prox_dev_cntr
  print(paste("Amount of extra devices reached due to the UE-VBS system:",dev_diff))
} 

all_dev_list <-append(list_oued,list_ouevbsd)
gen_stats(network.0,all_dev_list)