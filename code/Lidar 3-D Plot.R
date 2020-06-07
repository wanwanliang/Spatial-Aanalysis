
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

lasfilternoise2 = function(las, res=1)
{
  las <- lasfilter(las, Z > 0)
  las@data$Z = las@data$Z*0.3048
  return(las)
}

setwd("D:/KnoxTN_LiDar_usgs/")

library(raster)
library(rgdal)
library(lidR)
library(modeest)
library(rgeos)


kd=readOGR('.','kudzuRDsampleSPS') #State Plane 41000 projection


hbly=ogrListLayers("HB.kml")
hb=readOGR("HB.kml",layer=hbly,verbose = T)
hb=spTransform(hb,crs(kd))

gr=readOGR(".","grass2")
gr=spTransform(gr,crs(kd))

ob3=readOGR(".","lidarPlot")
ob3=spTransform(ob3,crs(kd))


ld=list.files('.','.las$')
ctg = catalog('.',recursive=TRUE)


fr=ob3[2,]
kd=ob3[3,]



grld = lasclip(ctg,gr)
frld = lasclip(ctg,fr)
kdld = lasclip(ctg,kd)
hbld = lasclip(ctg,hb)

gras=lastrees(grld,li2012())
col= random.colors(200)
plot(gras,bg="white",legend=T)

kdas=lastrees(kdld,li2012())
col= random.colors(200)
plot(kdas,bg="white",legend=T)

fras=lastrees(frld,li2012())
col= random.colors(200)
plot(fras,bg="white",legend=T)


hbas=lastrees(hbld,li2012())
col= random.colors(200)
plot(hbas,bg="white",legend=T)


grf= lasfilternoise(gras)
kdf= lasfilternoise(kdas)
frf= lasfilternoise(fras)
hbf= lasfilternoise(hbas)

dtm_grf =grid_terrain(grf, algorithm = kriging()) 
grf_flat = lasnormalize(grf, dtm_grf,na.rm=T)

dtm_kdf =grid_terrain(kdf, algorithm = kriging()) 
kdf_flat = lasnormalize(kdf, dtm_kdf,na.rm=T)

dtm_frf =grid_terrain(frf, algorithm = kriging()) 
frf_flat = lasnormalize(frf, dtm_frf,na.rm=T)

dtm_hbf =grid_terrain(hbf, algorithm = kriging()) 
hbf_flat = lasnormalize(hbf, dtm_hbf, na.rm=T)


grf_flat2= lasfilternoise2(grf_flat)
kdf_flat2= lasfilternoise2(kdf_flat)
frf_flat2= lasfilternoise2(frf_flat)
hbf_flat2= lasfilternoise2(hbf_flat)

plot(grf_flat2,bg="black",legend=T)
plot(kdf_flat2,bg="black",legend=T)
plot(frf_flat2,bg="black",legend=T)
plot(hbf_flat2,bg="black",legend=T)

plot(grf_flat2,bg="black",legend=F)
plot(kdf_flat2,bg="black",legend=F)
plot(frf_flat2,bg="black",legend=F)
plot(hbf_flat2,bg="black",legend=F)


plot(grf,bg="white",legend=T)
plot(kdf,bg="white",legend=T)
plot(frf,bg="white",legend=T)
plot(hbf,bg="white",legend=T)

