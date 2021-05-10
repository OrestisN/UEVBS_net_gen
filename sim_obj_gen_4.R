#creation of final simulation objects

#creating the final uevbs network object
euvbs_network <- function(tree_list,net_no){
  check_flag <- FALSE
  for (i in tree_list){
    
    if (length(i)!=2){
      
      check_flag <- TRUE
    }
  }
  if (check_flag==FALSE){
    
    values <- list(tree_nets=tree_list)
    class(values) <- paste("UEVBSnetwork",as.character(net_no))
    values
  }
  else{
    print("something is wrong with your spanning tree list...")
  }
}

net_cntr<-0
uevbs_net_name<-paste("uevbs_net.",as.character(net_cntr),sep = "")
assign(uevbs_net_name,euvbs_network(list_otreeo,net_cntr))

# creation of the "normal" network object,
# involves any device not included in the uevbs network (in proximity to the BS)
# need to make sure that devices in badc clusters are not connected (bug not fixed)

ue_network <-function(lo_list,net_no,ran_badc,n_badc){
  net_list<-list()
  for (i in lo_list ){
    
    check_place <- 0
    flag <- FALSE
    if (n_badc>0){
      for (t in seq(1,n_badc)){
        
        check_place <- point.in.polygon(i$x,i$y,
                                        unlist(ran_badc[[1]][t:(t*3)]),
                                        unlist(ran_badc[[2]][t:(t*3)]))
        if (check_place==1){
          flag <-TRUE
          print("PING")
        }
      }
    }
    dist_apart <- sqrt((i$x)^2 + (i$y)^2)
    if ((dist_apart <= 1) & (flag == FALSE)){
      
      net_list <- append(net_list,list(i))
    }
  }
  values <- list(dev_list = net_list)
  class(values) <- paste("UEnetwork",as.character(net_no))
  values
}

ue_net_name<-paste("ue_net.",as.character(net_cntr),sep = "")
assign(ue_net_name,ue_network(fleft_out_list,net_cntr,badc_clusr,n_tri))

network <-function(uevbs_neto,ue_neto,net_no){
  
  values <- list(uevbs_net=uevbs_neto,ue_net=ue_neto)
  class(values)<-paste("UEnetwork",as.character(net_no))
  values
}

net_name<-paste("network.",as.character(net_cntr),sep = "")
assign(net_name,network(uevbs_net.0,ue_net.0,net_cntr))


