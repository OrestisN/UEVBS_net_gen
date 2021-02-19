#graphing of results for check
#will make plotly (interractive) version soon that only uses the network object...soon...


#plotttin the devices in the 2D space/graph
par(mfrow=c(1,1))
plot(ue_x,ue_y,xlim =c(-1,1),ylim=c(-1,1))
# plot(ue_x,ue_y,xlim=c(-(sq_side-1),sq_side-1),ylim=c(-(sq_side-1),sq_side-1))
points(0,0,cex=1.5,col="blue")
points(ue_vbs_x,ue_vbs_y,col="red")

#polygon to shpow where there could be bad coverage, point.in.polygon can be used to check
for (u in 0:(bc_no-1)){
  
  polygon(badc_clusr[[1]][(1+u*3):(3+u*3)],badc_clusr[[2]][(1+u*3):(3+u*3)], col = NULL, border = "dark red")
}



stem_cntr <- 1
for (i in gen_stem_list){
  
  trailx=c(0)
  traily=c(0)
  cntr=1 
  if(length(i)>=1){
    
    for (k in i){
      
      if (cntr==1){
        
        lines(c(trailx[cntr],k$x),c(traily[cntr],k$y),col="green")
      }
      else if(cntr==2){
        
        lines(c(trailx[cntr],k$x),c(traily[cntr],k$y),col="red")
      }
      else if(cntr==3){
        
        lines(c(trailx[cntr],k$x),c(traily[cntr],k$y),col="blue")
      }
      else if (cntr>=4){
        
        lines(c(trailx[cntr],k$x),c(traily[cntr],k$y),col="purple")
      }
      trailx=append(trailx,k$x)
      traily=append(traily,k$y)
      cntr<-cntr+1
    } 
  }
  for (q in gend_leafe_list[[stem_cntr]]){
    # print(k$x)
    # print(q$x)
    lines(c(k$x,q$x),c(k$y,q$y),col="black")
  }
  stem_cntr <- stem_cntr +1
} 

for (a in ue_net.0$dev_list){
  lines(c(0,a$x),c(0,a$y),col = "yellow")
}

# draw.circle(0,0,c(1))