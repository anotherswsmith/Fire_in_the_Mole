#########################################################################################################################
#Fire map 2016
rm(list=ls())
require(raster)
require(rgdal)
#########################################################################################################################

#Molefires from Joana including 2016
#molefires<-read.table("C:\\Users\\speed\\Desktop\\Savanna fire\\DATA FROM 2000-2016.csv",header=T,sep=',')
setwd("/Users/stuartsmith/Documents/AfricanBioServices/Masters/Joana Awuah/")
molefires<-read.table("Fire_in_the_Mole/Fire_mapping/DATA FROM 2000-2016.csv",header=T,sep=',')

#Only high confidence
molefires90<-molefires[molefires$confidence>=90,]
molefires90
#Make spdf
molespdf<-SpatialPointsDataFrame(cbind(molefires$longitude,molefires$latitude),molefires)
mole90spdf<-SpatialPointsDataFrame(cbind(molefires90$longitude,molefires90$latitude),molefires90)
#Get woodycover raster
woodcov<-raster("Fire_in_the_Mole/Fire_mapping/MOD44B_V5_TRE.2001.PN2930.tif")
woodcovmole<-crop(woodcov,extent(bbox(mole90spdf)))
woodcovmole[101:200]<-NA
NAvalue(woodcovmole)<-200
plot(woodcovmole)
woodcovmoleutm<-projectRaster(woodcovmole,res=1000,crs="+proj=utm +zone=30 ellps=WGS84") 

r1<-rasterize(mole90spdf,woodcovmoleutm,'YEAR')

yearlyfires<-list()
yearlyfiresraster<-list()
yearlyfiresrasterutm<-list()
#Only look at fires above 80% conf
m<-c(-1,90,NA,90,101,1)
mmat<-matrix(m,ncol=3,byrow=T)
rcl<-list()
yearoffire<-list()
agfires<-list()
for(i in 1:17){
  yearlyfires[[i]]<-molespdf[molespdf$YEAR==1999+i,]
  yearlyfiresraster[[i]]<-rasterize(yearlyfires[[i]],woodcovmole,'confidence',fun='max') #Max confidence of a fire per cell per year
  yearlyfiresrasterutm[[i]]<-projectRaster(yearlyfiresraster[[i]],crs="+proj=utm +zone=30 ellps=WGS84") #Project to 1km cell
  agfires[[i]]<-aggregate(yearlyfiresrasterutm[[i]],4)
  mmat[2,3]<-1999+i
  rcl[[i]]<-reclassify(agfires[[i]],mmat)
  yearoffire[[i]]<-rcl[[i]]
  
}
yearoffire
stackfire<-stack(yearoffire)
plot(stackfire)

intervals <- seq(2000, 2016, 16) 
firefreq <- cut(stackfire, intervals, include.lowest=TRUE) 
freq(x, merge=TRUE) 
plot(firefreq)

lastfire <- max(stackfire, na.rm=TRUE)
lastfire
plot(lastfire)
lastfire2<-lastfire
#writeRaster(lastfire,'S:\\Supervision\\Savanna fire\\MoleYearLastFire',format='GTiff')

breakpoints <- c(2000,2005,2010,2015,2016)
colors <- c("yellow","orange","red",'darkred')
plot(lastfire,breaks=breakpoints,col=colors)
lastfirell<-projectRaster(lastfire,crs='+proj=longlat +datum=WGS84')
KML(lastfirell,"C:\\Users\\speed\\Desktop\\Savanna fire\\JamesFires2016",col=colors,breaks=breakpoints,overwrite=T)

road1<-readOGR("C:\\Users\\speed\\Desktop\\Savanna fire\\Roads",'ConnectRoad')
road2<-readOGR("C:\\Users\\speed\\Desktop\\Savanna fire\\Roads",'LoopRoad')
road3<-readOGR("C:\\Users\\speed\\Desktop\\Savanna fire\\Roads",'MoleRoad')
road4<-readOGR("C:\\Users\\speed\\Desktop\\Savanna fire\\Roads",'SmallRoad')
allroads<-c(road1,road2,road3,road4)

#  Get the Lines objects which contain multiple 'lines'
ll0 <- lapply( allroads , function(x) `@`(x , "lines") )
#  Extract the individual 'lines'
ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
#  Combine them into a single SpatialLines object
MoleRoads <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ),(CRS("+proj=longlat")) )

plot(lastfirell,breaks=breakpoints,col=colors)
lines(MoleRoads)

coordinates(MoleRoads)
crdl0 <- coordinates(MoleRoads)
crdl1 <- sapply(crdl0, function(x) do.call("rbind", x))
sppoints<-SpatialPoints(data.frame(matrix(crdl1,ncol=2,byrow=F)),CRS("+proj=longlat"))


moleroadrast<-rasterize(MoleRoads,woodcovmole,1)
moleroadrastutm<-projectRaster(moleroadrast,crs="+proj=utm +zone=30 ellps=WGS84")
moleroad3km<-aggregate(moleroadrastutm,10)

#Woodycover
woodcovmolestack<-projectRaster(woodcovmole,stackfirell)

#Cells within 3km of the roads
stackfirell<-projectRaster(stackfire,crs="+proj=longlat")
e1<-as.data.frame(extract(stack(stackfirell,woodcovmolestack),MoleRoads,buffer=3000,cellnumbers=T))
#e1<-as.data.frame(extract(stackfirell,sppoints,cellnumbers=T))
siteselection<-cbind(xyFromCell(stackfirell[[1]],e1$cell),e1)
names(siteselection)[4:20]<-2000:2016
names(siteselection)[1:2]<-c('long','lat')
names(siteselection)[21]<-'WoodyCover'
siteselection[is.na(siteselection)]<-0
siteselection$lastfire<-apply(siteselection[,4:20],1,max,na.rm=T)
siteselection$classification<-siteselection$lastfire
siteselection$classification[siteselection$lastfire>=2015]<-'1 year'
siteselection$classification[siteselection$lastfire>=2010 & siteselection$lastfire<2015]<-'2-4 years'
siteselection$classification[siteselection$lastfire>=2005 & siteselection$lastfire<2010]<-'5-10 years'
siteselection$classification[siteselection$lastfire>=2000 & siteselection$lastfire<2005]<-'10-15 years'
siteselection$classification[siteselection$lastfire<2000]<-'NotKnownBurnt'
siteselection$name<-paste(siteselection$classification,siteselection$cell,sep='_')
head(siteselection)

write.csv(siteselection,'C:\\Users\\speed\\Desktop\\Savanna fire\\JamesSiteSelection.csv',row.names=F)

#Managed fires and late fires
dim(droplevels(molefires90[molefires90$YEAR=="2006",]))
barplot(with(molefires90,tapply(confidence,list(Month),length)),main='Distribution of fires (90%conf) by month')
barplot(with(molefires90,tapply(confidence,list(YEAR),length)),main='Distribution of fires (90%conf) by year')
with(molefires90,aggregate(confidence,list(Month,YEAR),length))
with(molefires90,aggregate(confidence,list(Month,YEAR),mean))
(with(molefires90,aggregate(confidence,list(FireType,YEAR),length)))
barplot(with(molefires90,tapply(confidence,list(FireType,YEAR),length)),legend=T,main='Fire frequency (90%conf)')

dim(molefires90)
aggregate(confidence~YEAR,data=molefires90,length)

# Recent early burn, late burn, old late burn, unburnt
dim(molefires90) # 9082


#Raterize both fire types
manfires<-mole90spdf[mole90spdf$ManagedFire==1,]
latefires<-mole90spdf[mole90spdf$LateFire==1,]

#Managedfires
yearlymanagedfires<-list()
yearlymanagedfiresraster<-list()
yearlymanagedfiresrasterutm<-list()
rclmanaged<-list()
yearofmanagedfire<-list()
agmanagedfires<-list()
for(i in c(1,2,4:16)){
  yearlymanagedfires[[i]]<-manfires[manfires$YEAR==1999+i,]
  yearlymanagedfiresraster[[i]]<-rasterize(yearlymanagedfires[[i]],woodcovmole,'confidence',fun='max') #Max confidence of a fire per cell per year
  yearlymanagedfiresrasterutm[[i]]<-projectRaster(yearlymanagedfiresraster[[i]],crs="+proj=utm +zone=30 ellps=WGS84") #Project to 1km cell
  agmanagedfires[[i]]<-aggregate(yearlymanagedfiresrasterutm[[i]],4)
  mmat[2,3]<-1999+i
  rclmanaged[[i]]<-reclassify(agmanagedfires[[i]],mmat)
  yearofmanagedfire[[i]]<-rclmanaged[[i]]
  
}
yearofmanagedfire[[3]]<-raster(yearofmanagedfire[[2]])
plot(stack(yearlymanagedfiresrasterutm))
plot(stack(agmanagedfires))
stackmanagedfire<-stack(yearofmanagedfire[1:16])
lastmanagedfire <- max(stackmanagedfire, na.rm=TRUE)
lastmanagedfire
plot(lastmanagedfire)
lastmanagedfirell<-projectRaster(lastmanagedfire,crs='+proj=longlat +datum=WGS84')
plot(lastmanagedfirell,main='Most Recent Early Fire')
lines(MoleRoads)

#Latefires
yearlylatefires<-list()
yearlylatefiresraster<-list()
yearlylatefiresrasterutm<-list()
m<-c(-1,90,NA,90,101,1)
mmat<-matrix(m,ncol=3,byrow=T)
rcllate<-list()
yearoflatefire<-list()
aglatefires<-list()
for(i in 1:16){ #Skip 2000 as not late fires before start of imagery
  yearlylatefires[[i]]<-latefires[latefires$YEAR==2000+i,]
  yearlylatefiresraster[[i]]<-rasterize(yearlylatefires[[i]],woodcovmole,'confidence',fun='max') #Max confidence of a fire per cell per year
  yearlylatefiresrasterutm[[i]]<-projectRaster(yearlylatefiresraster[[i]],crs="+proj=utm +zone=30 ellps=WGS84") #Project to 1km cell
  aglatefires[[i]]<-aggregate(yearlylatefiresrasterutm[[i]],4) #Aggregate to 1km
  mmat[2,3]<-2000+i
  rcllate[[i]]<-reclassify(aglatefires[[i]],mmat)
  yearoflatefire[[i]]<-rcllate[[i]]
  
}
plot(stack(yearlylatefiresrasterutm))
plot(stack(aglatefires))
stacklatefire<-stack(yearoflatefire)
lastlatefire <- max(stacklatefire, na.rm=TRUE)
lastlatefire
plot(lastlatefire)

lastlatefirell<-projectRaster(lastlatefire,crs='+proj=longlat +datum=WGS84')
plot(lastlatefirell,main='Most Recent Late Fire')
lines(MoleRoads)

breakpoints <- c(2000,2005,2010,2012,2014,2016)
colors <- c("yellow","orange","red",'darkred','black')
x11(24,16)
par(mfrow=c(1,2))
plot(lastmanagedfirell,breaks=breakpoints,col=colors,main="Early Fires")
lines(MoleRoads,lwd=2,col='blue')
plot(lastlatefirell,breaks=breakpoints,col=colors,main='Late Fires')
lines(MoleRoads,lwd=2,col='blue')

writeRaster(lastlatefirell,'S:\\Supervision\\Savanna fire\\lastlatefire.tif')
writeRaster(lastmanagedfirell,'S:\\Supervision\\Savanna fire\\lastearlyfire.tif')

#ExtractAllFires
pairs(stack(lastlatefirell,lastmanagedfirell))

stackmanagedfirell<-projectRaster(stackmanagedfire,crs="+proj=longlat")
stacklatefirell<-projectRaster(stacklatefire,crs="+proj=longlat")
ext1<-as.data.frame(extract(stack(stackmanagedfirell,stacklatefirell,woodcovmolestack),MoleRoads,cellnumbers=T,buffer=1000))
siteselection_managelate<-cbind(xyFromCell(stackfirell[[1]],ext1$cell),ext1)
names(siteselection_managelate)[1:2]<-c('long','lat')
names(siteselection_managelate)[4:18]<-paste('manage',2001:2015,sep="_")
names(siteselection_managelate)[19:34]<-paste('late',2001:2016,sep="_")
names(siteselection_managelate)[35]<-'WoodyCover'

siteselection_managelate$earlysum<-apply(siteselection_managelate[,4:18],1,sum,na.rm=T)
siteselection_managelate$latesum<-apply(siteselection_managelate[,19:34],1,sum,na.rm=T)
siteselection_managelate[siteselection_managelate$earlysum<2017 & siteselection_managelate$earlysum>1999,]
siteselection_managelate[siteselection_managelate$latesum<2017 & siteselection_managelate$latesum>1999,]

par(mfrow=c(1,2))
extent1<-extent(-2.05,-1.8,9.23,9.42)

manfires2<-crop(manfires,extent1)
latefires2<-crop(latefires,extent1)
col1<-colorRampPalette(c('yellow','red'))
plot(manfires2[manfires2$YEAR==2015,])
lines(MoleRoads)
plot(latefires2[latefires2$YEAR>=2015,],col=latefires2$YEAR[latefires2$YEAR>=2015])
lines(MoleRoads)


lastlatefires2<-crop(lastlatefirell,extent1)
plot(lastlatefires2,main='Late Fires')
lines(MoleRoads)
points(latefires2[latefires2$YEAR>=2015,],pch=16,cex=0.2)
lastearlyfires2<-crop(lastmanagedfirell,extent1)
plot(lastearlyfires2,main="Early Fires")
lines(MoleRoads)
points(manfires2[manfires2$YEAR==2015,],pch=16,cex=0.2)

lastlatefires3<-lastlatefires2
lastlatefires3[lastearlyfires2>1999]<-NA
lastearlyfires3<-lastearlyfires2
lastearlyfires3[lastlatefires2>1999]<-NA

notburnt<-is.na(lastfirell)
notburnt1<-crop(notburnt,extent1)

plot(notburnt1)
lines(MoleRoads)
points(manfires,cex=0.3,pch=16)
points(latefires,cex=0.3,pch=16,col=2)
#Here is how the samples got taken
#notburntsamplepoints<-click(notburnt,xy=T,type='p',id=T)
#notburnext<-extract(stack(stackmanagedfirell,stacklatefirell,woodcovmolestack),notburntsamplepoints[,1:2],cellnumbers=T)
#notburntsamples<-cbind(notburntsamplepoints[,1:2],notburnext)
#names(notburntsamples)[1:2]<-c('long','lat')
#names(notburntsamples)[4:18]<-paste('manage',2001:2015,sep="_")
#names(notburntsamples)[19:34]<-paste('late',2001:2016,sep="_")
#names(notburntsamples)[35]<-'WoodyCover'
#notburntsamples$lastburn<-apply(notburntsamples[,4:34],1,max,na.rm=T)
#notburntsamples<-notburntsamples[order(notburntsamples$lastburn),]
#notburntsamples<-notburntsamples[1:15,]
#notburntsamples$name<-paste('NotBurnt',rownames(notburntsamples),sep='_')
#write.csv(notburntsamples,'C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\NotBurntSites.csv')
notburntpoints<-read.csv('C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\NotBurntSites.csv')
points(notburntpoints[,2:3],col=4,pch=16)

#Late fires selection
plot(lastlatefires3,main='Late Fires')
lines(MoleRoads)
points(manfires,cex=0.3,pch=16)
points(latefires,cex=0.3,pch=16,col=2)
#latefiresamplepoints<-click(lastlatefires3,xy=T,type='p',id=T)
#extlate<-extract(stack(stackmanagedfirell,stacklatefirell,woodcovmolestack),latefiresamplepoints[,1:2],cellnumbers=T)
#latesamples<-cbind(latefiresamplepoints[,1:2],extlate)
#names(latesamples)[1:2]<-c('long','lat')
#names(latesamples)[4:18]<-paste('manage',2001:2015,sep="_")
#names(latesamples)[19:34]<-paste('late',2001:2016,sep="_")
#names(latesamples)[35]<-'WoodyCover'
#latesamples$lastburn<-apply(latesamples[,19:34],1,max,na.rm=T)
#latesamples$name<-paste('Late',latesamples$lastburn,rownames(latesamples),sep='_')
#latesamples<-latesamples[1:19,]
#latesamples<-latesamples[order(latesamples$lastburn),]
#write.csv(latesamples,'C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\LateFires.csv')
latefirepoints<-read.csv('C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\LateFires.csv')
points(latefirepoints[,2:3],col=4,pch=16)


#EarlyFires
plot(lastearlyfires3,main='Early Fires')
lines(MoleRoads)
points(manfires,cex=0.3,pch=16)
points(latefires,cex=0.3,pch=16,col=2)
#earlyfiresamplepoints<-click(lastearlyfires3,xy=T,type='p',id=T)
#extearly<-extract(stack(stackmanagedfirell,stacklatefirell,woodcovmolestack),earlyfiresamplepoints[,1:2],cellnumbers=T)
#earlysamples<-cbind(earlyfiresamplepoints[,1:2],extearly)
#names(earlysamples)[1:2]<-c('long','lat')
#names(earlysamples)[4:18]<-paste('manage',2001:2015,sep="_")
#names(earlysamples)[19:34]<-paste('late',2001:2016,sep="_")
#names(earlysamples)[35]<-'WoodyCover'
#earlysamples$lastburn<-apply(earlysamples[,4:18],1,max,na.rm=T)
#earlysamples$name<-paste('early',earlysamples$lastburn,rownames(earlysamples),sep='_')
#earlysamples<-earlysamples[order(earlysamples$lastburn),]
#earlysamples<-earlysamples[3:28,]
#write.csv(earlysamples,'C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\earlyFires.csv')
earlyfirepoints<-read.csv('C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\earlyFires.csv')
points(earlyfirepoints[,2:3],col=4,pch=16)

#Look at all
plot(crop(lastfirell,extent1))
points(earlyfirepoints[,2:3],col=4,pch=16)
points(latefirepoints[,2:3],col=2,pch=16)
points(notburntpoints[,2:3],col=1,pch=16)
legend('topl',pch=16,col=c(1,2,4),c("NotBurnt",'Late','Early'))
lines(MoleRoads)

#All points
allsamples<-rbind(latefirepoints,earlyfirepoints,notburntpoints)
names(allsamples)[2]<-'lon'
allsamples_simple<-allsamples[,c(2,3,38)]
write.csv(allsamples,'C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\allsamples.csv',row.names=F)
write.csv(allsamples_simple,'C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\allsamples_simple.csv',row.names=F)
KML(SpatialPoints(allsamples[,2:3],proj4string=CRS('+proj=longlat +datum=WGS84')),'C:\\Users\\speed\\Desktop\\Savanna fire\\FinalSiteSelection\\allsamples',overwrite=T)



#########################################################################################################################
#### Area Mole National Park burn season and history ####
require(rgdal)
require(rasterVis)
require(gridExtra)
library(rgeos)
library(sf)
#########################################################################################################################

setwd("/Users/stuartsmith/Documents/AfricanBioServices/Masters/Joana Awuah/")

lateraster<-raster('Fire_in_the_Mole/Fire_mapping/lastlatefire.tif')
earlyraster<-raster('Fire_in_the_Mole/Fire_mapping/lastearlyfire.tif')
lastfire<-raster('Fire_in_the_Mole/Fire_mapping/MoleYearLastFire.tif')
lastfirell<-projectRaster(lastfire,crs='+proj=longlat +datum=WGS84')
levelplot(stack(lateraster,earlyraster),margin=F,rasterTheme=YlOrRdTheme)

# Count cells from raster 
nps<-readOGR('Fire_in_the_Mole/Fire_mapping/WDPA_Feb2016_GHA-shapefile-polygons','WDPA_Feb2016_GHA-shapefile-polygons') # All national parks ghana
levels(as.factor(nps$NAME)) #Mole
npsMOLE<-nps[nps$NAME=="Mole",]
extent(npsMOLE)
plot(npsMOLE)
npsMOLE
#Ghana_crs<-"+proj=utm +zone=30 +ellps=WGS84 +units=m +no_defs "
#npsMOLE<-spTransform(npsMOLE,Ghana_crs)

#### Calculate burn areas for different fire histories #### 

#Late burns
lateraster[is.na(lateraster==0)] <- 1
laterasterMOLE<-raster::mask(lateraster,npsMOLE)
plot(laterasterMOLE);plot(npsMOLE, add=T)
laterasterMOLE[laterasterMOLE==1] <- NA
plot(laterasterMOLE);plot(npsMOLE, add=T)
cellStats(laterasterMOLE, function(i, ...) sum(!is.na(i))) # 1752
# Park = 5332
1752/5332 # 33% late burns

# Recent late burns
lateraster[lateraster==1] <- NA
RecentLateBurns  <- c(2016,2015,2014,2013)
Rlateraster<-lateraster
Rlateraster<-round(Rlateraster,0) %in% RecentLateBurns
plot(Rlateraster)
Rlateraster[Rlateraster==0] <- NA
RlaterasterMOLE<-raster::mask(Rlateraster,npsMOLE)
plot(RlaterasterMOLE);plot(npsMOLE, add=T)
cellStats(RlaterasterMOLE, function(i, ...) sum(!is.na(i))) # 453
453/5332 # 8 % recent late burns

# Early burns
earlyraster[earlyraster==0] <- 1
earlyrasterMOLE<-raster::mask(earlyraster,npsMOLE)
earlyrasterMOLE[earlyrasterMOLE==1] <- NA
plot(earlyrasterMOLE);plot(npsMOLE, add=T)
cellStats(earlyrasterMOLE, function(i, ...) sum(!is.na(i))) # 4228
4228/5332 # 79% early season burns
earlyraster

# Recent Early burns
earlyraster[earlyraster==1] <- NA
Recentearlyraster <- c(2016,2015,2014,2013,2012)
Rearlyraster<-earlyraster
plot(Rearlyraster)
Rearlyraster<-round(Rearlyraster,0) %in% Recentearlyraster
plot(Rearlyraster)
Rearlyraster[Rearlyraster==0] <- NA
plot(Rearlyraster);plot(npsMOLE, add=T)
cellStats(Rearlyraster, function(i, ...) sum(!is.na(i))) # 1640
1640/5332 # 31 % recent early season burns

# Unburnt
x <- list(earlyraster, lateraster)
m <- do.call(merge, x)
m[is.na(m==0)] <- 1
mMOLE<-raster::mask(m,npsMOLE)
plot(mMOLE);plot(npsMOLE, add=T)
mMOLE1<-round(mMOLE,0) %in% 1
plot(mMOLE1);plot(npsMOLE, add=T)
mMOLE1[mMOLE1==0] <- -999
mMOLE1[mMOLE1>0]<-NA
cellStats(mMOLE1, function(i, ...) sum(is.na(i))) # 697
697/5332 # 13 % unburnt

#########################################################################################################################
#### Figure ####
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
#########################################################################################################################

#Working directory
setwd("/Users/stuartsmith/Documents/AfricanBioServices/Masters/Joana Awuah/")

# Fire layers
lateraster<-raster('Fire_in_the_Mole/Fire_mapping/lastlatefire.tif')
earlyraster<-raster('Fire_in_the_Mole/Fire_mapping/lastearlyfire.tif')
lastfire<-raster('Fire_in_the_Mole/Fire_mapping/MoleYearLastFire.tif')
lastfirell<-projectRaster(lastfire,crs='+proj=longlat +datum=WGS84')
levelplot(stack(lateraster,earlyraster),margin=F,rasterTheme=YlOrRdTheme)

### Make colors transparent
makeTransparent<-function(someColor, alpha=75)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
} #End function

#Mole NP
#gha<-getData('GADM',country='GHA',level=1)
gha<-ne_states(country = 'ghana')#  scale = 'medium')
nps<-readOGR('Fire_in_the_Mole/Fire_mapping/WDPA_Feb2016_GHA-shapefile-polygons','WDPA_Feb2016_GHA-shapefile-polygons')
studypoints<-read.csv('Fire_in_the_Mole/Fire_mapping/Coordinates.study.sites.csv')

#Study sites
spsp<-SpatialPointsDataFrame(cbind(studypoints$Longitude,studypoints$Latitude),studypoints,proj4string = crs(lateraster))
#Bounding box
bb <- as(extent(spsp), "SpatialPolygons")
proj4string(bb) <- crs(lateraster)

plot(gha)
#plot(nps[nps$NAME=='Mole',],col='red',add=T)
plot(lastfirell,add=T,col=rev(heat.colors(100)))
plot(bb,add=T)

levelplot(stack(lateraster,earlyraster),margin=F,rasterTheme=YlOrRdTheme)+
  layer(sp.points(spsp[spsp$Type=='Unburnt',],pch=1))+
  layer(sp.polygons(bb))

plot(nps,add=T)

studystack<-stack(lateraster,earlyraster)
names(studystack)<-c('Late fires','Early fires')

laterasterMOLE<-raster::mask(lateraster,npsMOLE)
earlyrasterMOLE<-raster::mask(earlyraster,npsMOLE)
studystack_m<-stack(laterasterMOLE,earlyrasterMOLE) # Check 
names(studystack_m)<-c('Late fires','Early fires')
levelplot(studystack_m,par.settings='YlOrRdTheme')

studystack_c<-crop(studystack,bb)
levelplot(studystack_c,par.settings='YlOrRdTheme') 
          layer(sp.points(spsp[spsp$Type=='Unburnt',],pch=3,col=1))+
  layer(sp.points(spsp[spsp$Type=='Old_late',],pch=2,col=1))+
  layer(sp.points(spsp[spsp$Type=='Recent_early season',],pch=1,col=1))+
  layer(sp.points(spsp[spsp$Type=='Recent_late season',],pch=16,col=1))


plot(studystack_c,col=rev(heat.colors(100)))
points(spsp[spsp$Type=='Unburnt',],pch=3)
  layer(sp.points(spsp[spsp$Type=='Old_late',],pch=2))+
  layer(sp.points(spsp[spsp$Type=='Recent_early season',],pch=1))+
  layer(sp.points(spsp[spsp$Type=='Recent_late season',],pch=16))
  
#### Plotting figure
scaleMin <- floor(2000)
scaleMax <- ceiling(2017)

numberOfBreaks <- 17
brksUniv <- round(seq(scaleMin,scaleMax, length.out=numberOfBreaks),0)

myColorkey <- list(at=brksUniv, labels=list(at=(2000:2016), labels=round((2000:2016),0)))

# Ghana - Mole National Park  
lastfire_ext<-extend(lastfirell,gha)
levelplot(lastfire_ext,margin=F,par.settings=YlOrRdTheme)+
  layer(sp.polygons(nps[nps$NAME=='Mole',]))+
  layer(sp.polygons(gha))+
  layer(sp.polygons(bb))
 
#Joint figures
lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)
p1<-levelplot(lastfire_ext,margin=F,par.settings=YlOrRdTheme,#at=brksUniv, colorkey=myColorkey,
              key = list(space = 'left', text = list(c('Unburnt','Old_late', 'Recent_early season', 'Recent_late season'))
                         , points = list(pch=c(15,0,16,1),lwd=2,col=c('darkgreen','tan4','lightgreen','black'))))+
  latticeExtra::layer(sp.polygons(nps[nps$NAME=='Mole',]))+
  latticeExtra::layer(sp.polygons(gha))+
  latticeExtra::layer(sp.polygons(bb,fill=makeTransparent("blue"),border='blue'))

lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)

p2<-levelplot(studystack_m,par.settings='YlOrRdTheme',#at=brksUniv, colorkey=myColorkey,
              names.attr=c('Late fires','Early fires'))+
  latticeExtra::layer(sp.polygons(nps[nps$NAME=='Mole',]))+
  latticeExtra::layer(sp.polygons(bb,fill=makeTransparent("blue"),border='blue'))

lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0))
)

p3<-levelplot(studystack_c,par.settings='YlOrRdTheme',#at=brksUniv, colorkey=myColorkey,
              names.attr=c('Late fires','Early fires'))+ 
  #   key = list(space = 'top', text = c('Unburnt','Old_late', 'Recent_early season', 'Recent_late season')
  #             , points = list(pch=c(3,2,1,16))))+
  latticeExtra::layer(sp.points(spsp[spsp$Type=='Unburnt',],pch=15,col='darkgreen'))+
  latticeExtra::layer(sp.points(spsp[spsp$Type=='Old_late',],pch=0,col='tan4',lwd=2))+
  latticeExtra::layer(sp.points(spsp[spsp$Type=='Recent_early season',],pch=16,col='lightgreen'))+
  latticeExtra::layer(sp.points(spsp[spsp$Type=='Recent_late season',],pch=1,col=1,lwd=2))
#tiff('S:\\Supervision\\Savanna Fire\\Manuscript\\MapFig.tif',res=300,width=9,height=7,units='in')
#tiff('/Users/stuartsmith/Documents/AfricanBioServices/Masters/Joana Awuah/Fire_in_the_Mole/Fire_mapping/MapFig.tif',res=300,width=9,height=7,units='in')
#grid.arrange(p1,p2,p3)
#dev.off()

filename <- paste0("/Users/stuartsmith/Documents/AfricanBioServices/Masters/Joana Awuah/Fire_in_the_Mole/Fire_mapping/", "Fig_fire_map",".jpeg" )
jpeg(filename,width= 24, height = 18,units ="cm",bg ="transparent", res = 600)
grid.arrange(p1,p2,p3)
dev.off()

range2<-c(0,1)
p0<-levelplot(lastfire_ext,scales = list(draw = FALSE), par.settings=list(axis.line = list(col = "white")),
              margin=F,names.attr=c(''),title="",xlab = "", ylab = "", at = seq(range2[1], range2[2], length = 100),colorkey=NULL)
p0

## combine panels from both plots
combo <- c(p1,p0,p2,p3)
## rearrange in pairs
library(svglite)# Save as SVG
setwd("/Users/stuartsmith/Documents/AfricanBioServices/Masters/Joana Awuah/Fire_in_the_Mole/Fire_mapping/")
svg("Fire_map_MoleNP.svg",width= 10, height = 8)
update(combo, scales = list(draw = T),
       names.attr=c('Late fires','Early fires'),
       layout = c(2, 3), between = list(x = c(0, 0.5), y = 0.5))
dev.off()

#########################################################################################################################
#### END ####
#########################################################################################################################