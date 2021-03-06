#Generating random device objects and "environment" (bad ocverage area)


Gendatap<-function(sizesq,init_plambda,sd_dscale,m_dp,rvbs_per){
  
  #default testing genrative parameters
  # sizesq=3
  # init_plambda=10
  # sd_dscale=0.05
  # m_dp=10
  
  # Thomas clustering generation funcvtion
  Tclustgen<-function(ilambda,sdd,meand,squlen){
    clust<-rThomas(ilambda,sdd,meand,win=owin(c(squlen-(3*(squlen/2)),squlen-(squlen/2)),c(squlen-(3*(squlen/2)),squlen-(squlen/2))))
    return(clust)
  }
  
  # extra random points (poissson point process) 
  # uncomment to include them
  
  # extra_ppois<-function(lambda,squlen){
  #   ranpoi<-rpoispp(lambda,win=owin(c(squlen-(3*(squlen/2)),squlen-(squlen/2)),c(squlen-(3*(squlen/2)),squlen-(squlen/2))))
  #   return(ranpoi)
  # }
  
  test1<-Tclustgen(init_plambda,sd_dscale,m_dp,sizesq)
  # test1<-extra_ppois(init_plambda/2,sizesq)
  
  #cocantenating all generated values
  all_x<-append(test1$x,attr(test1,"parents")$x)
  # all_x<-append(all_x,etest1$x)
  all_y<-append(test1$y,attr(test1,"parents")$y)
  # all_y<-append(all_y,etest1$y)
    
  #random sampling of concatenated data to pick out ue-vbs devices
  uevbsx<-sample(all_x,round((rvbs_per/100)*length(all_x)))
  iuevbsx<-match(uevbsx,all_x)
  
  uevbsyext<-all_y[iuevbsx]
  uevbsy<-uevbsyext[1:length(uevbsx)]
  
  return(list(all_x[-iuevbsx],all_y[-iuevbsx],uevbsx,uevbsy))
  
}


rangenddata<-Gendatap(sq_side,init_intencity,sd_dp,mean_amount_dp,uevbs_per)
ue_x<-unlist(rangenddata[1])
ue_y<-unlist(rangenddata[2])
ue_vbs_x<-unlist(rangenddata[3])
ue_vbs_y<-unlist(rangenddata[4])

# Maybe the location and shape of the triangles shouldnt be random...
# to increase consistency in future simulations

tringle_gen <- function(Ran,not,prox){
  
  if (not>0){
    random_triangle <- function(n_o_tr,n_prox_c){
      list_out_x<- list()
      list_out_y<- list()
      if (n_o_tr>0){}
        for (i in 1:n_o_tr){
          
          if (rnorm(1) >=1){
            n_prox_c <- -n_prox_c
          }
          x_c = list()
          y_c = list()
          init_x = rnorm(1)/2 + n_prox_c
          x_c <- append(x_c,init_x)
          init_y = rnorm(1)/2 + n_prox_c
          y_c <- append(y_c,init_y)
          sec_x = (rnorm(1)/2 *cos(2*pi*rnorm(1))) + init_x
          x_c <- append(x_c,sec_x)
          sec_y = (rnorm(1)/2 *cos(2*pi*rnorm(1))) + init_y
          y_c <- append(y_c,sec_y)
          l_x=((init_x+sec_x)/2) + ((rnorm(1)/2)/sqrt((init_y-sec_y)^2+(init_x-sec_x)^2))*(init_y-sec_y)
          x_c <- append(x_c,l_x)
          l_y=((init_y+sec_y)/2) + ((rnorm(1)/2)/sqrt((init_y-sec_y)^2+(init_x-sec_x)^2))*(init_x-sec_x)
          y_c <- append(y_c,l_y)
          
          list_out_x <- append(list_out_x,x_c)
          list_out_y <- append(list_out_y,y_c)
        }
        return(list(list_out_x,list_out_y))
      }
      
      nonrandom_triangle <- function(n_prox_c, n_o_tr = 2){
        list_out_x<- list()
        list_out_y<- list()
        for (i in 1:n_o_tr){
          
          if (i >=1){
            n_prox_c <- -n_prox_c
          }
          
          x_c = list()
          y_c = list()
          init_x = i/2 + n_prox_c
          x_c <- append(x_c,init_x)
          init_y = -i/2 + n_prox_c
          y_c <- append(y_c,init_y)
          sec_x = (i/2 *cos(2*pi*1)) + init_x
          x_c <- append(x_c,sec_x)
          sec_y = (-i/2 *cos(2*pi*1)) + init_y
          y_c <- append(y_c,sec_y)
          l_x=((init_x+sec_x)/2) + ((i/2)/sqrt((init_y-sec_y)^2+(init_x-sec_x)^2))*(init_y-sec_y)
          x_c <- append(x_c,l_x)
          l_y=((init_y+sec_y)/2) + ((-i/2)/sqrt((init_y-sec_y)^2+(init_x-sec_x)^2))*(init_x-sec_x)
          y_c <- append(y_c,l_y)
          
          list_out_x <- append(list_out_x,x_c)
          list_out_y <- append(list_out_y,y_c)
        }
        return(list(list_out_x,list_out_y))
      }
      
      if (Ran==FALSE){
        res <-nonrandom_triangle(prox,n_o_tr=not)
      }
      else{
        res <-random_triangle(not,prox)
      }
      return (res)
  }else{
    return(NULL)
  }
}

badc_clusr <- tringle_gen(rt_check, n_tri, tri_dist)

#tunring point into device objects

dev_cnt<-0
ue_dev <- function(posx,posy,uevbs_en,dev_no){
  if (class(uevbs_en) != "logical"){
    stop("uevbs_en needs to be TRUE/FALSE!!")
  }
  values <- list(x=posx,y=posy,enabled=uevbs_en)
  class(values) <- paste("UEdevice",as.character(dev_no),sep="")
  values
}

list_oued = list()
for (i in 1:length(ue_x)){
  dev_cnt <- dev_cnt+1
  ue_device <- paste("ue_device",as.character(dev_cnt),sep=".")
  assign(ue_device,ue_dev(ue_x[i],ue_y[i],FALSE,dev_cnt))
  list_oued[[i]] <- eval(as.symbol(ue_device))
}

list_ouevbsd = list()
for (k in 1:length(ue_vbs_x)){
  dev_cnt <- dev_cnt+1
  ue_device <- paste("ue_device",as.character(dev_cnt),sep=".")
  assign(ue_device,ue_dev(ue_vbs_x[k],ue_vbs_y[k],TRUE,dev_cnt))
  list_ouevbsd[[k]]<-eval(as.symbol(ue_device))
}