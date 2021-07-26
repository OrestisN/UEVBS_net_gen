#creating the clusters and completing the network generated
#WARWNING function runs for a long time (need to make it faster...)
leafe_list_gen <- function(stem_list,all_left_dev_list,max_nl,min_nl,reach){
  
  av_dev_list <- all_left_dev_list
  clust_cnt <- 1
  out_list<- list()
  for (i in stem_list){
    
    leafe_list <-list()
    dist_list <- c()
    stem_len <- length(i)
    dev_no<-0
    for (k in all_left_dev_list){
      
      dist_apart<-sqrt((i[[stem_len]]$x - k$x)^2 + (i[[stem_len]]$y - k$y)^2)
      if ( (list(k) %in% av_dev_list) &
           (dist_apart <= reach)){
        
        leafe_list <- append(leafe_list,list(k))
        dist_list <- append(dist_list,dist_apart)
        dev_no <- dev_no + 1
      }
    }
    # print(dist_list)
    fin_leafe_list <- list()
    if (length(leafe_list) >= min_nl){
      
      leaf_dist_data <- data.frame(1:length(dist_list),dist_list)
      leaf_dist_data <- leaf_dist_data[order(leaf_dist_data[,2]),]
      leaf_dist_data <- leaf_dist_data[!duplicated(leaf_dist_data[,2]),]
      index_vec <- c()
      leafe_list<-leafe_list[!duplicated(leafe_list)]
      av_dev_amount <- length(leaf_dist_data[,1])
      if (av_dev_amount <= max_nl){
        dev_limit<-av_dev_amount
      }
      else{
        dev_limit<-max_nl
      }
      
      for (a in leaf_dist_data[,1][1:dev_limit]){
        
        fin_leafe_list <- append(fin_leafe_list,leafe_list[a])
        index_cntr<-1
        id<- attr(leafe_list[[a]],"class")
        for (z in av_dev_list){
  
          if(id==attr(z,"class")){
            
            index_vec <- append(index_vec,index_cntr)
          }
          index_cntr <- index_cntr +1
        }
      }
      index_vec<- sort(index_vec,decreasing = TRUE)
      for (c in index_vec){
        
        av_dev_list <- av_dev_list[-c]
      }
      out_list[[clust_cnt]] <- fin_leafe_list
      clust_cnt <- clust_cnt +1
    }
    else{
      
      out_list[[clust_cnt]] <- fin_leafe_list
      clust_cnt <- clust_cnt +1
    }
  }
  return(out_list)
}

gend_leafe_list <- leafe_list_gen(gen_stem_list,left_out_list,max_clust_no,min_clust_no,clust_range)


#generating the object list for clusters
ue_leaf_clust <- function(single_leaf_list,clust_no){
  
  llen<-length(single_leaf_list)
  if (llen >= 1){
    
    values <- list(dev_list=single_leaf_list)
    class(values) <- paste("UEVBScluster",as.character(clust_no),sep = "")
    values
  }
  else{
    
    print("This is an empty list...")
  }
}


clust_cnt <- 1
list_oclusto <- list()
for (k in gend_leafe_list){
  clust_name <- paste("uevbs_clust.",as.character(clust_cnt),sep = "")
  assign(clust_name,ue_leaf_clust(k,clust_cnt))
  list_oclusto[[clust_cnt]] <- eval(as.symbol(clust_name))
  clust_cnt <-clust_cnt + 1
}

#generating spannig network objects
ue_st_net <- function (stem_obj,clust_obj,net_no){
  
  slen<- length(stem_obj[[1]]$dev_list)
  if (clust_obj == "This is an empty list..."){
    clen <-1
    print(paste("cluster for stem",as.character(net_no),"is empty"))
  }
  else{
    clen<- length(clust_obj[[1]]$dev_list)
  }
  if ((slen >= 1) & (clen >= 1)){
    
    values <- list(stem=stem_obj,cluster=clust_obj)
    class(values) <- paste("UEVBStreenetwork",as.character(net_no))
    values
  }
  else{
    
  }
}

tree_cnt <- 1
list_otreeo <- list()
unused_av_uevbs_dev <- list()
d <- mapply(list,list_ouevbso,list_oclusto,SIMPLIFY = FALSE)
for (q in d){
  if (q[2]!="This is an empty list..."){
    clust_name <- paste("uevbs_tree.",as.character(tree_cnt),sep = "")
    assign(clust_name,ue_st_net(q[1],q[2],tree_cnt))
    list_otreeo[[tree_cnt]] <- eval(as.symbol(clust_name))
    tree_cnt<-tree_cnt + 1 
  }
  else{
    unused_av_uevbs_dev <- append(unused_av_uevbs_dev,q[[1]]$dev_list) 
  }
}

#A list of all left over devices that can be connected directly to the BS "normally"
left_over_b <- function(lo_list,clust_leaf_list,unused_stem_dev){
  out_list<-list()
  out_list<-append(out_list,unused_stem_dev)
  for (i in left_out_list){
    res <-FALSE
    for (k in clust_leaf_list){
      if (list(i) %in% k){
        
        # print("here")
        res<-TRUE
      }
    }
    if(res==FALSE){
      
      out_list<-append(out_list,list(i))
    }
  }
  return(out_list)
}

fleft_out_list <- left_over_b(left_out_list,gend_leafe_list,unused_av_uevbs_dev)
