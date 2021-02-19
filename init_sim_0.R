#Initial file to declare all values of your simulation 
#and run it??? (in the future)

library(spatstat)
library(sp)
library(plotly)
# library(plotrix)

#data generating parameters

#size of square area you are genrating data on [units are though of as in km]
sq_side <- 3
# initial intensity of cluster centres being generated (lambda)
#note that the random points that are genrated indipendantly form the thomas process ate half of this intensity
init_intencity <- 10
#standard deviation of daughter points of thomas clusters genrated ("the spread") 
sd_dp <- 0.05
#average ammount of points in the clusters generated
mean_amount_dp <- 10
#number of bad coverage triangle areas to be genrated in the are 
bc_no <- 2

#network formulation parameters

#max number of hops alowed
n_hops <- 3
#max number of devices in clusters alowed
max_clust_no <- 15


