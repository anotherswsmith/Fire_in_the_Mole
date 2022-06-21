#Fire map 2016
require(raster)
require(rgdal)

#Molefires from Joana including 2016
#molefires<-read.table("C:\\Users\\speed\\Desktop\\Savanna fire\\DATA FROM 2000-2016.csv",header=T,sep=',')
molefires<-read.table("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/DATA FROM 2000-2016.csv",header=T,sep=',')


#Only high confidence
molefires90<-molefires[molefires$confidence>=90,]
molefires90

#Make spdf
molespdf<-SpatialPointsDataFrame(cbind(molefires$longitude,molefires$latitude),molefires)
mole90spdf<-SpatialPointsDataFrame(cbind(molefires90$longitude,molefires90$latitude),molefires90)
#Get woodycover raster
#woodcov<-raster("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/MOD44B_V5_TRE.2001.PN2930.tif")
woodcov<-raster("WoodyCover/Woodycover/MOD44B_V5_TRE.2001.PN2930.tif")

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
#freq(x, merge=TRUE) 
plot(firefreq)

lastfire <- max(stackfire, na.rm=TRUE)
lastfire
plot(lastfire)
lastfire2<-lastfire
writeRaster(lastfire,'S:\\Supervision\\Savanna fire\\MoleYearLastFire',format='GTiff')

breakpoints <- c(2000,2005,2010,2015,2016)
colors <- c("yellow","orange","red",'darkred')
plot(lastfire,breaks=breakpoints,col=colors)
lastfirell<-projectRaster(lastfire,crs='+proj=longlat +datum=WGS84')
KML(lastfirell,"C:\\Users\\speed\\Desktop\\Savanna fire\\JamesFires2016",col=colors,breaks=breakpoints,overwrite=T)




#Date of last fire
#Convert acq date to a Date object
molespdf$date<-as.Date(molespdf$acq_date,"%d/%m/%Y")

#Set an origin (mid of sampling period)
dateorigin<-as.Date("2016-01-07")
molespdf$daysBeforeSampling<-as.numeric(molespdf$date-dateorigin)
molespdf$daysBeforeSampling

#Rasterize
dayslastFireRas<-rasterize(molespdf[molespdf$confidence>=90,],field='daysBeforeSampling',woodcovmole,fun='max')
dayslastFireRasUTM<-projectRaster(dayslastFireRas,crs="+proj=utm +zone=30 ellps=WGS84")
dayslastFireRasUTMag<-aggregate(dayslastFireRasUTM,4,fun='max')
plot(dayslastFireRasUTMag)

Coordinates_study_sites <- read.csv("Coordinates.study.sites.csv")
studysitecoordinatesspdf<-SpatialPointsDataFrame(cbind(Coordinates_study_sites$Longitude,Coordinates_study_sites$Latitude),
                                                 proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs'),Coordinates_study_sites)
plot(dayslastFireRasUTMag)
studysitecoordinatesspdfUTM<-spTransform(studysitecoordinatesspdf,CRS=CRS("+proj=utm +zone=30 ellps=WGS84"))
points(studysitecoordinatesspdfUTM)
studysitecoordinatesspdfUTM$lastFireDaysBeforeSampling<-extract(dayslastFireRasUTMag,studysitecoordinatesspdfUTM)
#studysitecoordinatesspdfUTM$lastFireYear<-extract(dayslastFireRasUTMag,studysitecoordinatesspdfUTM)

studysitecoordinatesspdfUTM$dateoflastFire<-as.Date(studysitecoordinatesspdfUTM$lastFireDaysBeforeSampling,origin = dateorigin)

View(studysitecoordinatesspdfUTM@data)
boxplot(studysitecoordinatesspdfUTM$lastFireDaysBeforeSampling~studysitecoordinatesspdfUTM$Type)








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
barplot(with(molefires90,tapply(confidence,list(Month),length)),main='Distribution of fires (90%conf) by month')
barplot(with(molefires90,tapply(confidence,list(YEAR),length)),main='Distribution of fires (90%conf) by year')
with(molefires90,aggregate(confidence,list(Month,YEAR),length))
with(molefires90,aggregate(confidence,list(Month,YEAR),mean))
(with(molefires90,aggregate(confidence,list(FireType,YEAR),length)))
barplot(with(molefires90,tapply(confidence,list(FireType,YEAR),length)),legend=T,main='Fire frequency (90%conf)')

names(molefires90)
aggregate(confidence~YEAR,data=molefires90,length)


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


#Set an origin (mid of sampling period)
dateorigin<-as.Date("2016-07-07")
manfires$daysBeforeSampling<-as.numeric(manfires$date-dateorigin)
manfires$daysBeforeSampling

#Rasterize
dayslastManagedFire<-rasterize(manfires,field='daysBeforeSampling',woodcovmole,fun='max')
dayslastManagedFireUTM<-projectRaster(dayslastManagedFire,crs="+proj=utm +zone=30 ellps=WGS84",method='ngb')
dayslastManagedFireUTMag<-aggregate(dayslastManagedFireUTM,4,fun='max')
plot(dayslastManagedFireUTMag)

Coordinates_study_sites <- read.csv("Coordinates.study.sites.csv")
studysitecoordinatesspdf<-SpatialPointsDataFrame(cbind(Coordinates_study_sites$Longitude,Coordinates_study_sites$Latitude),
                                                 proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs'),Coordinates_study_sites)
plot(dayslastManagedFireUTMag)
studysitecoordinatesspdfUTM<-spTransform(studysitecoordinatesspdf,CRS=CRS("+proj=utm +zone=30 ellps=WGS84"))
points(studysitecoordinatesspdfUTM)
studysitecoordinatesspdfUTM$lastManagedFireDaysBeforeSampling<-extract(dayslastManagedFireUTMag,studysitecoordinatesspdfUTM)
studysitecoordinatesspdfUTM$lastManagedFireDate<-as.Date(studysitecoordinatesspdfUTM$lastManagedFireDaysBeforeSampling,origin = dateorigin)
View(studysitecoordinatesspdfUTM@data)

#One fire has the wrong year so omit this from calculation
mean(studysitecoordinatesspdfUTM$lastManagedFireDaysBeforeSampling[studysitecoordinatesspdfUTM$Type=="Recent_early season"& abs(studysitecoordinatesspdfUTM$lastManagedFireDaysBeforeSampling)<2000])
sd(studysitecoordinatesspdfUTM$lastManagedFireDaysBeforeSampling[studysitecoordinatesspdfUTM$Type=="Recent_early season"& abs(studysitecoordinatesspdfUTM$lastManagedFireDaysBeforeSampling)<2000])


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


#Rasterize
dayslastLateFire<-rasterize(latefires[latefires$YEAR<2016   & latefires$Month<=4,],field='daysBeforeSampling',woodcovmole,fun=max)
dayslastLateFireUTM<-projectRaster(dayslastLateFire,crs="+proj=utm +zone=30 ellps=WGS84",method="ngb")
dayslastLateFireUTMag<-aggregate(dayslastLateFireUTM,4,fun=max)
plot(dayslastLateFireUTMag)

plot(dayslastLateFireUTMag)
points(studysitecoordinatesspdfUTM)
studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling<-extract(dayslastLateFireUTMag,studysitecoordinatesspdfUTM)
studysitecoordinatesspdfUTM$lastLateFireDate<-as.Date(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling, origin = dateorigin)
View(studysitecoordinatesspdfUTM@data)

mean(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling[studysitecoordinatesspdfUTM$Type=="Recent_late season"& abs(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling)<3000],na.rm=T)
sd(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling[studysitecoordinatesspdfUTM$Type=="Recent_late season"& abs(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling)<3000])

mean(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling[studysitecoordinatesspdfUTM$Type=="Old_late"],na.rm=T)
sd(studysitecoordinatesspdfUTM$lastLateFireDaysBeforeSampling[studysitecoordinatesspdfUTM$Type=="Old_late"],na.rm=T)



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

#Late finres selection
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



####################
#Figure
require(rgdal)
require(rasterVis)
require(gridExtra)

lateraster<-raster('S:\\Supervision\\Savanna fire\\lastlatefire.tif')
earlyraster<-raster('S:\\Supervision\\Savanna fire\\lastearlyfire.tif')

lastfire<-raster('S:\\Supervision\\Savanna fire\\MoleYearLastFire.tif')
lastfirell<-projectRaster(lastfire,crs='+proj=longlat +datum=WGS84')
levelplot(stack(lateraster,earlyraster),margin=F,rasterTheme=YlOrRdTheme)


#Mole NP

gha<-getData('GADM',country='GHA',level=1)
nps<-readOGR('S:\\Supervision\\Savanna fire\\WDPA_Feb2016_GHA-shapefile','WDPA_Feb2016_GHA-shapefile-polygons')
studypoints<-read.csv('S:\\Supervision\\Savanna fire\\Coordinates of study sites.csv')
#Study sites
spsp<-SpatialPointsDataFrame(cbind(studypoints$Longitude,studypoints$Lat.),studypoints,proj4string = crs(lateraster))
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
p1<-levelplot(lastfire_ext,margin=F,par.settings=YlOrRdTheme,
              key = list(space = 'left', text = list(c('Unburnt','Old_late', 'Recent_early season', 'Recent_late season'))
                         , points = list(pch=c(15,0,16,1),lwd=2,col=c('darkgreen','tan4','lightgreen','black'))))+
  layer(sp.polygons(nps[nps$NAME=='Mole',]))+
  layer(sp.polygons(gha))+
  layer(sp.polygons(bb,fill='blue',border='blue'))

p2<-levelplot(studystack_c,par.settings='YlOrRdTheme',names.attr=c('Late fires','Early fires'))+ 
  #   key = list(space = 'top', text = c('Unburnt','Old_late', 'Recent_early season', 'Recent_late season')
  #             , points = list(pch=c(3,2,1,16))))+
  layer(sp.points(spsp[spsp$Type=='Unburnt',],pch=15,col='darkgreen'))+
  layer(sp.points(spsp[spsp$Type=='Old_late',],pch=0,col='tan4',lwd=2))+
  layer(sp.points(spsp[spsp$Type=='Recent_early season',],pch=16,col='lightgreen'))+
  layer(sp.points(spsp[spsp$Type=='Recent_late season',],pch=1,col=1,lwd=2))
tiff('S:\\Supervision\\Savanna Fire\\Manuscript\\MapFig.tif',res=300,width=9,height=7,units='in')
grid.arrange(p1,p2)
dev.off()
