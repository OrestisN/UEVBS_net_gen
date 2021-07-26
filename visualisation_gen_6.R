#graphing of results for checking purposes
#will make plotly (interractive) version ...soon...

#network.0 <-readRDS("four hop\\int6\\network7.rds")
bad_net_plot <- function(net_obj,unreachable_devs,ran_tri,n_ran_tri){
  par(mfrow=c(1,1))
  
  plot(0,0,cex=1.5,col="blue",xlim =c(-1.5,1.5),ylim=c(-1.5,1.5),
       main=expression("Generated Network"),
       sub=expression("Coloured spots represent the location of devices included in our network."),
       ylab="y-axis of area",
       xlab="x-axis of area")
  #polygon to show where there could be bad coverage, point.in.polygon can be used to check
  # if (n_ran_tri>0){
  #   for (u in seq(1,n_ran_tri)){
  #     
  #     polygon(unlist(ran_tri[[1]][u:(u*3)]),unlist(ran_tri[[2]][u:(u*3)]), col = NULL, border = "dark red")
  #   }
  # }
  for (z in unreachable_devs){

    points(z$x,z$y, col="black")
  }
  # for (i in network.0$uevbs_net$tree_nets){
  #   
  #   for (k in i$stem){
  #     
  #     for(q in k$dev_list){
  #       
  #       points(q$x,q$y,col="red")
  #     }
  #   }
  #   for(a in i$clust){
  #     
  #     for(w in a$dev_list){
  #       
  #       points(w$x,w$y,col="black")
  #     }
  #   }
  # }
  # for (t in network.0$ue_net$dev_list){
  #   points(t$x,t$y,col = "black")
  # }
  
  plot(0,0,cex=1.5,col="blue",xlim =c(-2,2),ylim=c(-2,2),
       main=expression("Generated Network"),
       sub=expression("Cluster networks"),
       ylab="y-axis of area",
       xlab="x-axis of area")
  if (n_ran_tri>0){
    for (u in seq(1,n_ran_tri)){
      
      polygon(unlist(ran_tri[[1]][u:(u*3)]),unlist(ran_tri[[2]][u:(u*3)]), col = NULL, border = "dark red")
    }
  }
  for (z in unreachable_devs){
    
    points(z$x,z$y, col="grey")
  }
  for (i in network.0$uevbs_net$tree_nets){
    for (k in i$stem){
      
      l_dev_no=length(k$dev_list)
      trail_x<-k$dev_list[[l_dev_no]]$x
      trail_y<-k$dev_list[[l_dev_no]]$y
      for(q in k$dev_list){
        
        points(q$x,q$y,col="red")
        
      }
    }
    for(a in i$clust){
      
      for(w in a$dev_list){
        
        points(w$x,w$y,col="orange")
        lines(c(trail_x,w$x),c(trail_y,w$y),col="black")
      }
    }
  }
  for (t in network.0$ue_net$dev_list){
    points(t$x,t$y,col = "green")
  }

  plot(0,0,cex=1.5,col="blue",xlim =c(-2,2),ylim=c(-2,2),
       main=expression("Generated Network"),
       sub=expression("Relay networks"),
       ylab="y-axis of area",
       xlab="x-axis of area")
  if (n_ran_tri>0){
    for (u in seq(1,n_ran_tri)){
      
      polygon(unlist(ran_tri[[1]][u:(u*3)]),unlist(ran_tri[[2]][u:(u*3)]), col = NULL, border = "dark red")
    }
  }
  for (z in unreachable_devs){
    
    points(z$x,z$y, col="grey")
  }
  for (i in network.0$uevbs_net$tree_nets){
    s_trail_x <- 0
    s_trail_y <- 0
    for (k in i$stem){
      
      for(q in k$dev_list){
        
        points(q$x,q$y,col="red")
        lines(c(s_trail_x,q$x),c(s_trail_y,q$y),col="black")
        s_trail_x <- q$x
        s_trail_y <- q$y
        
      }
    }
    for(a in i$clust){
      
      for(w in a$dev_list){
        
        points(w$x,w$y,col="orange")
      }
    }
  }
  for (t in network.0$ue_net$dev_list){
    points(t$x,t$y,col = "green")
  }
  
  plot(0,0,cex=1.5,col="blue",xlim =c(-1.5,1.5),ylim=c(-1.5,1.5),
       main=expression("Generated Network"),
       ylab="y-axis of area",
       xlab="x-axis of area")
  if (n_ran_tri>0){
    for (u in seq(1,n_ran_tri)){
      
      polygon(unlist(ran_tri[[1]][u:(u*3)]),unlist(ran_tri[[2]][u:(u*3)]), col = NULL, border = "dark red")
    }
  }
  
  for (i in network.0$uevbs_net$tree_nets){
    s_trail_x <- 0
    s_trail_y <- 0
    for (k in i$stem){
      
      l_dev_no=length(k$dev_list)
      trail_x<-k$dev_list[[l_dev_no]]$x
      trail_y<-k$dev_list[[l_dev_no]]$y
      for(q in k$dev_list){
        points(q$x,q$y,col="red")
        lines(c(s_trail_x,q$x),c(s_trail_y,q$y),col="blue")
        s_trail_x <- q$x
        s_trail_y <- q$y
        
      }
    }
    for(a in i$clust){
      
      for(w in a$dev_list){
        
        points(w$x,w$y,col="orange")
        lines(c(trail_x,w$x),c(trail_y,w$y),col="black")
      }
    }
  }
  for (t in network.0$ue_net$dev_list){
    points(t$x,t$y,col = "green")
  }
  
}

bad_net_plot(network.0,fleft_out_list,badc_clusr,n_tri)


