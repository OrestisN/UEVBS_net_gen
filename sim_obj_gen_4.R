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
net_name<-paste("uevbs_net.",as.character(net_cntr),sep = "")
assign(net_name,euvbs_network(list_otreeo,net_cntr))

