########################################################################################################
# Author: Eduarda Chagas
# Date : Nov 15, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
if(!require(sp)){
  install.packages("sp")
  require(sp)
}

# Analysis functions -----------------------------------------------------------------------------------
test.set.point <- function(HC, D, N, region){
  points = points.confidence.regions(region, D, N)
  test = rep(FALSE, dim(HC)[1])
  for(i in 1:dim(HC)[1]){
    test[i] = point.in.polygon(HC[i,1], HC[i,2], points$H, points$C)
  }
  return(test)
}

points.confidence.regions <- function(region, D, N){
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  if(region == 90){
    rect90 = data.frame(H = hc.points$H[1:4], C = hc.points$C[1:4])
    point = rect90
  }else if(region == 95){
    rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
    point = rect95
  }else if(region == 99){
    rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
    point = rect99
  }else if(region == 99.9){
    rect999 = data.frame(H = hc.points$H[13:16], C = hc.points$C[13:16])
    point = rect999
  }
  return(point)
}

#test.point(H = 0.9998, C = 0.0001, D = 3, N = 1000, region = 90)
