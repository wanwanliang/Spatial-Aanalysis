


setwd("D:/KnoxTN_LiDar_usgs/")

library(raster)
library(rgdal)
library(lidR)
library(modeest)
library(gstat)
library(parallel)
library(foreach)
library(doParallel)

las=list.files(".",".las$")
length(las)
head(las)

plys=readOGR(".","pred1SE")
dim(plys)
crs(plys)


# convert plys to state-plane projection
plys2=spTransform(plys,"+proj=lcc +lat_1=35.25 +lat_2=36.41666666666666
+lat_0=34.33333333333334 +lon_0=-86 +x_0=600000 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft
+no_defs ")



ctg = catalog('.',recursive=TRUE)
plot(ctg)

# remove potential outliers for each cell (Z out of 95% quantile range & Z<0)
lasfilternoise = function(las, res=1)
{
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), res)
  las <- lasmergespatial(las, p95, "p95")
  las <- lasfilter(las, Z < p95)
  las$p95 <- NULL
  # Remove negative values
  las <- lasfilter(las, Z > 0)
  
  return(las)
}




cl <- makeCluster(detectCores()-2) # use 10 cores here
registerDoParallel(cl)

ptime=system.time({
  
  result=foreach (i=1:500, .combine=rbind, .export=c("mfv","lasfilternoise","lasnormalize","grid_terrain","lasclip","extract"), .packages=c('raster',"rgdal","modeest","lidR")) %dopar% {
    
    polygon=plys2[i,]
    area=raster::area(polygon)
    
    lddt = lasclip(ctg,polygon)
    d=dim(lddt@data)[1]
    
    if (d!=0){
      
      #remove potential outliers
      lddt <- lasfilternoise(lddt)
      
      #generate dem
      dtm = try(grid_terrain(lddt, algorithm = kriging()) )
      
      if (class(dtm)!="try-error") {
        lddt_flat = lasnormalize(lddt, dtm,na.rm=T)
        
        #number of point
        Npoint=dim(lddt_flat@data)[1]
        
        #get dataset for point clouds
        dt=lddt_flat@data
        
        #only look at first return (canopy height)
        cnp=dt[dt$ReturnNumber==1,]
        
        meanZ=mean(cnp$Z) #mean
        sdZ=sd(cnp$Z) #sd
        varZ=var(cnp$Z) #variance
        intenZ=mean(cnp$Intensity) #mean of intensity
        
        
        n1=try(which.max(density(cnp$Z)$y))
        
        if (class(n1)!="try-error"){
          modeZ1st=density(cnp$Z)$x[n1]  #mode of canopy height (first return)
          n2=which.max(density(dt$Z)$y)
          modeZall=density(dt$Z)$x[n2] #mode of all point height
        }
        
      }
      
    }
    
    allZ=c(area,Npoint,meanZ,sdZ,varZ,intenZ,modeZ1st,modeZall)
    return(allZ)
    
  }
})
stopCluster(cl)
ptime

lidVars=as.data.frame(result)
dim(lidVars)
colnames(lidVars)=c("Area",'Npoints','MeanZ','SDZ','VarianceZ',"Intensity",'Ht_mode',"Allpts_mode")
lidVars$Density=lidVars$Npoints/lidVars$Area

head(lidVars)

write.csv(lidVars,"LidarFeatures.csv",row.names = F)

