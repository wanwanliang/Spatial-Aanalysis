library(devtools)
install_github("joelgombin/concaveman")

library(concaveman)
library(sp)

## Step 1 For each period, create Concave Hull for centroids of all cumulative infested geopolitical-units
setwd("work directory")
shp=list.files(".",pattern = ".shp$")
shp


for (i in 1:length(shp)){
  nm1=substr(shp[i],1,nchar(shp[i])-4)
  aoi=readOGR(".",nm1)
  coords=aoi@coords
  concave=concaveman(coords)
  
  p=Polygon(concave)
  ps=Polygons(list(p),1)
  sps=SpatialPolygons(list(ps))
  crs(sps)=crs(aoi)
  sps=SpatialPolygonsDataFrame(sps,data=as.data.frame("id"))
  
  nm2=paste("Concave",nm1,sep="")
  
  writeOGR(sps,".",nm2,driver="ESRI Shapefile")
}

