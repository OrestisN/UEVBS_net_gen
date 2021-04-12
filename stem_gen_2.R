# Generating varible length stem networks

#Need to fix an issue where secodary nodes chosen in the stem are closer to  the centre (BS)
stem_list_gen<-function(n_stem_hops,l_ofeuvbs,ran_badc,n_badc,max_dist,min_dist,init_dist){
  
  stem_cnt<-1
  out_list<-list()
  av_uevbs_dev<-l_ofeuvbs
  
  for (i in l_ofeuvbs){
    stem_list<-list()
    check_place <- 0
    flag <-FALSE
    for (t in 0:(n_badc-1)){
      
      check_place <- point.in.polygon(i$x,i$y,ran_badc[[1]][(1+(t*3)):(3+(t*3))],ran_badc[[2]][(1+(t*3)):(3+(t*3))])
    }
    if (check_place==1){
      flag <- TRUE
    }
    trailx=c(0)
    traily=c(0)
    for (k in 1:n_stem_hops){
      
      if((k==1)&(flag==FALSE)&(list(i) %in% av_uevbs_dev)){
        
        dist_apart1 <- sqrt((trailx[k] - i$x)^2 + (traily[k] - i$y)^2)
        if (dist_apart1<=init_dist){
          stem_list <-append(stem_list,list(i))
          trailx <- append(trailx,i$x)
          traily <- append(traily,i$y)
        }
        else{
          break
        }
      }
      #not sure why the flag needs to be false here...doesnt work if that is not there....(FIXED in my head)
      else if ((k>1)&(flag==FALSE)&(list(i) %in% av_uevbs_dev)){
        posible_dev<-list()
        posible_dev_dist<-c()
        for (q in av_uevbs_dev){
          if (list(q) %in% av_uevbs_dev){
            
            dist_apart2<-sqrt((trailx[k] - q$x)^2 + (traily[k] - q$y)^2)
            if ((dist_apart2 <= max_dist) & (dist_apart2 >= min_dist)){
              
              posible_dev <- append(posible_dev,list(q))
              posible_dev_dist <- append(posible_dev_dist,dist_apart2)
            } 
          }
        }
        if (length(posible_dev) >= 1){
          
          orient_l <- orient_check(trailx[k],traily[k],posible_dev)
          if (TRUE %in% orient_l){
            index_of_choice <- match(TRUE,orient_l)
            print("good orientation!")
          }
          else{
            max_dist <- max(posible_dev_dist)
            index_of_choice <- match(max_dist,posible_dev_dist)
            # print("here2")
          }
          stem_list <- append(stem_list,posible_dev[index_of_choice])
          trailx <- append(trailx,posible_dev[[index_of_choice]]$x)
          traily <- append(traily,posible_dev[[index_of_choice]]$y)
        }
        else {
          break
        }
      }
    }
    if (length(stem_list)>=1){
      stem_list<-stem_list[!duplicated(stem_list)]
      vec_index=c()
      for (a in stem_list){
        index_cntr<-0
        id=attr(a,"class")
        # print(stem_list)
        # print(paste(1,id))
        for(z in av_uevbs_dev){
          
          index_cntr<-index_cntr+1
          if (attr(z,"class")==id){
            
            vec_index=append(vec_index,index_cntr)
          }
        }
      }
      vec_index<- sort(vec_index,decreasing = TRUE)
      for (c in vec_index){
        # print(paste(2,attr(av_uevbs_dev[[c]],"class")))
        # print(vec_index)
        av_uevbs_dev<-av_uevbs_dev[-c]
      }
      out_list[[stem_cnt]]<-stem_list
      stem_cnt<-stem_cnt+1
    }
  }
  return(out_list)
}

orient_check <- function(init_x,init_y,pos_points){
  or_list <- c()
  x_or <- init_x >= 0
  y_or <- init_y >= 0
  for (i in pos_points){
    x_check <- i$x >= 0
    y_check <- i$y >= 0
    if (((x_check == x_or) & (y_check == y_or))|
        ((x_check == x_or) & (y_check == y_or) &(x_check <= x_or)&(y_check <= y_or))){
      or_list <- append(or_list,TRUE)
    }
    else {
      or_list <- append(or_list,FALSE)
    }
  }
  return(or_list)
}


gen_stem_list = stem_list_gen(n_hops,list_ouevbsd,badc_clusr,bc_no,max_range,min_range,BS_range)


#generating the object list for stems
ue_stem <- function(single_stem_list,stem_no){
  
  dev_in_stem <- 1
  llen<-length(single_stem_list)
  if (llen >= 1){
    
    values <- list(dev_list=single_stem_list)
    class(values) <- paste("UEstem",as.character(stem_no),sep = "")
    values
  }
  else{
    
    print("This is an empty list...")
  }
}

stem_cnt <- 1
list_ouevbso <- list()
for (k in gen_stem_list){
  stem_name <- paste("ue_stem.",as.character(stem_cnt),sep = "")
  # print(stem_name)
  assign(stem_name,ue_stem(k,stem_cnt))
  list_ouevbso[[stem_cnt]] <- eval(as.symbol(stem_name))
  stem_cnt <-stem_cnt + 1
}


#creating a list of left over devices that could be uevbs enabled too
left_over_a <- function(ued_list,uevbsd_list,stem_list){
  
  left_over_out_list <- list()
  for (i in ued_list){
    
    left_over_out_list <- append(left_over_out_list,list(i))
  }
  for (k in uevbsd_list){
    res <- FALSE
    for (q in stem_list){
      
      if (list(k) %in% q){
        res <- TRUE
      }
    }
    if (res==FALSE){
      left_over_out_list <- append(left_over_out_list,list(k))
    }
  }
  return(left_over_out_list)
}

left_out_list <- left_over_a(list_oued,list_ouevbsd,gen_stem_list)
