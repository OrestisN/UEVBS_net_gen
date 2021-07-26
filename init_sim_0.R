#Initial file to declare all values of your simulation 


library(spatstat)
library(sp)
library(extraDistr)


#data generating parameters

#size of square area you are genrating data on [units are though of as in km]
sq_side <- 2.5
# initial intensity of cluster centres being generated (lambda)
#note that the random points that are genrated indipendantly form the thomas process ate half of this intensity
init_intencity <- 0.5
#standard deviation of daughter points of thomas clusters genrated ("the spread") 
sd_dp <- 0.1
#average ammount of points in the clusters generated
mean_amount_dp <- 25
#random sample percentage of generated data that is seen ad a UEVBS enabled device
uevbs_per <- 25
# false if you want statick position of bad c trinagls, TRUE if you want random positions
rt_check <- FALSE
# Number of triangles (above 2 will not work yet...)
n_tri <- 0
#Distance of initial point of triangle from origin
tri_dist <- 0.5

#network formulation parameters

#max number of hops alowed
n_hops <- 5
#initial max. range allowed from BS to UE devices
BS_range <- 1
#max. allowed range of next stem device
max_range<- 0.5
#min. allowed range of next stem device
min_range<- 0.3

#max. number of devices in clusters alowed
max_clust_no <- 14
#min. number of devices in clusters alowed
min_clust_no <- 8
#cluster max. reach range 
clust_range <- 0.25

#Run for a single network run (displaying purposes)
source("data_gen_1.R")
source("stem_gen_2.R")
source("leafe_gen_3.R")
source("sim_obj_gen_4.R")
source("visualisation_gen_6.R")

#RUNNING FUNCTION uncomment and change paths to create a new set of networks for simulation
#
# for (x in seq(0.5,6,0.5)){
# init_intencity <- x
# filename <- paste("int",toString(x),sep="")
# dir.create(filename)
#   for (o in seq(1,10)){
#     source("data_gen_1.R")
#     source("stem_gen_2.R")
#     source("leafe_gen_3.R")
#     source("sim_obj_gen_4.R")
#     #source("analysis_5.R")
#     objname <- paste("network",toString(o),".rds",sep="")
#     print(objname)
#     saveRDS(network.0, file = paste(filename,"\\",objname,sep=""))
#   }
# }
# source("visualisation_gen_6.R")