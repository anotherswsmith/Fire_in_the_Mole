### LIBRARY  #############
rm(list=ls())
library(readr)
require(dplyr)
require(ggplot2)
require(reshape2)
#standard error
se <- function(x) sd(x, na.rm=T)/sqrt(length(x))

##### IMPORT DATASET called 'Ghana.belowground' ##############

df <- read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.belowground2.csv", sep=",",header=TRUE)
df$Treatment <- as.factor(df$Treatment)
df$Treatment
#Levels: Old late, Recent early, Recent late, Unburnt

# Reorder fire histories
# (1) Recent early, (2) Recent late, (3) Old late, (4) Unburnt
df$Treatment <- factor(df$Treatment, levels(df$Treatment)[c(2,3,1,4)])

#df$Horizon <- as.factor(df$Horizon)
# adding covariates
#productivity  <- read.csv("M:/Anders L Kolstad/systherb data/exported cvs/productivity_index_sustherbSites.csv")
#CN_data <- within(CN_data, PI <- productivity$PI[match(CN_data$locationID, productivity$lokalitetid)])


################################################
# Using soil C stocks (SOC)
################################################

#names(df)
#df2 <- aggregate(cbind(SOC=df$SOC), list(Depth=df$Horizon, Treatment=df$Treatment), FUN=mean, na.rm=T)
#df2$SE <- aggregate(cbind(SOC=df$SOC), list(Depth=df$Horizon, Treatment=df$Treatment), FUN=se)[,3]
#df2$Treatment

# Make depth-plot # Using soil C stock
gp <- ggplot(df2, aes(x=Depth, y=SOC, group=Treatment))
pd <- position_dodge(width=0.1)  #keeps error bars from overlapping

plt <- gp + geom_line(aes(linetype=Treatment,color=Treatment),
                      size=1.1, position=pd) + 
  geom_point(aes(color=Treatment), size=4, position=pd) + 
  geom_errorbar(aes(ymax=SOC+SE, ymin=SOC-SE, color=Treatment), width=.1, size=1.1, position=pd) +
  theme_bw() + scale_colour_grey() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_line( size=0.01, color="gray" )
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)) +
  theme(axis.line = element_line(color = 'black')) +
  coord_flip()+
  scale_y_continuous(position = "top")+
  ylab(expression(paste("Soil C ( kg ",m^-2,")")))+
  xlab("Depth (cm)")+
  theme(legend.position = "bottom")
plt
################################################################
# Using soil C density

################################################################

df2 <- aggregate(cbind(SOC=df$C.density), list(Depth=df$Horizon, Treatment=df$Treatment), FUN=mean, na.rm=T)
df2$SE <- aggregate(cbind(SOC=df$C.density), list(Depth=df$Horizon, Treatment=df$Treatment), FUN=se)[,3]
as.factor(df2$SOC)
df2$Pval<-c("ab","b","b","b",
            "a","a","a","a",
            "ab","b","b","b",
            "b","b","b","b")
df2
#1-(22.81403/42.33982) # Decline in recent late season 
#65.78289/69.09668

# Colours by fire history
df2$Treatment
color_pallete_function <- colorRampPalette(
  colors = c("light green","black", "tan4", "dark green"),
  space = "Lab" 
)

num_colors <- nlevels(df2$Treatment)
num_colors
diamond_color_colors <- color_pallete_function(num_colors)
df2$pt.col<-diamond_color_colors[df2$Treatment]
levels(as.factor(df2$pt.col))


# Make depth-plot # Using soil C density
pd <- position_dodge(width=0.1)  #keeps error bars from overlapping

# This works - how to store it?
cat(paste0('"', paste(levels(as.factor(df2$pt.col)), collapse="\", \""), '"'))
colourset<-paste(levels(as.factor(df2$pt.col))) # Leveling does not relate to Treatment!

gp <- ggplot(df2, aes(x=Depth, y=SOC, fill=Treatment, colour=Treatment))
gp <- gp + geom_line(stat="identity",size=1.1, position=pd) 
gp <- gp+geom_errorbar(stat="identity",aes(ymax=SOC+SE, ymin=SOC-SE), 
                  width=.1, size=1.1,position=pd) 
gp <- gp+ geom_point(stat="identity",aes(colour=Treatment,shape=Treatment),
                     size=4, stroke = 1.3, position=pd)
gp <- gp+scale_color_manual(values=c("light green","black", "tan4","dark green")) # This does not when when df2$pt.col
gp <- gp+scale_fill_manual(values=c("light green", "white", "white","dark green") ) 
gp <- gp+scale_shape_manual(values=c(21,21,22,22)) 
gp <- gp+geom_text(aes(size=12,label=df2$Pval, fontface="bold"),
           check_overlap=T,hjust=-1, vjust=1,show.legend = F)
gp <- gp+ylab(expression(paste("Soil C ( kg ",m^-2,")")))+xlab("Depth (cm)")
gp <- gp+scale_y_continuous(position = "top")+ scale_x_reverse(position = "bottom")
gp <- gp+coord_flip()
gp<- gp+ theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_line( size=0.01, color="gray" )
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.title=element_text(size=14)
        ,legend.text=element_text(size=12)) +
  theme(axis.line = element_line(color = 'black')) +
  theme(legend.position = "bottom")
gp


# make a table of means
# rearrange rows so they match the plot lay-out
#df2 <- df2[c(7,3,8,4,5,1,6,2),]
#df2$n <- c(15,15,15,15,15,15,14,14)


#require(gridExtra)
#tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
# http://www.magesblog.com/2015/04/plotting-tables-alsongside-charts-in-r.html
#tbl <- tableGrob(df2, rows=NULL, theme=tt)  # make the table

# Export - remember to set working directory
ggsave("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Soil.C.density.jpeg", width= 15, height = 20,units ="cm",
       dpi = 400, limitsize = TRUE)

###############################################################################
# Aboveground C pool - stripchart panel
se<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
###############################################################################

# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.aboveground.old.csv", sep=",",header=TRUE)

dim(GhanaCabove) # 140 rows # 4 columns
str(GhanaCabove)
head(GhanaCabove) # NAs
names(GhanaCabove)

# Reorder factors
GhanaCabove$Treatment <- factor(GhanaCabove$Treatment, levels(GhanaCabove$Treatment)[c(2,3,1,4)])
levels(GhanaCabove$Treatment)
# Seperate out Trees, Shrubs, Dead wood + litter and Herb veg
levels(GhanaCabove$Pool)
GhanaCaboveTree<-GhanaCabove[GhanaCabove$Pool=="Tree",]
GhanaCaboveShrub<-GhanaCabove[GhanaCabove$Pool=="Shrub",]
GhanaCaboveDeadwood<-GhanaCabove[GhanaCabove$Pool=="deadwood",]
GhanaCabovelitter<-GhanaCabove[GhanaCabove$Pool=="litter",]
GhanaCaboveHerbveg<-GhanaCabove[GhanaCabove$Pool=="Herb.veg",]

#Plot means next to one another - similar colour to graph 4 - grey scale for fire histories
# Graph option - all pools next to one another - but then cannot see difference
#Tree
GhanaCaboveTreeX<-aggregate(GhanaCaboveTree$C.stock.kg.m2,by=list(Treatment=GhanaCaboveTree$Treatment),na.rm=T,mean)
GhanaCaboveTreeSE<-aggregate(GhanaCaboveTree$C.stock.kg.m2,by=list(Treatment=GhanaCaboveTree$Treatment),se)
#Shrub
GhanaCaboveShrubX<-aggregate(GhanaCaboveShrub$C.stock.kg.m2,by=list(Treatment=GhanaCaboveShrub$Treatment),na.rm=T,mean)
GhanaCaboveShrubSE<-aggregate(GhanaCaboveShrub$C.stock.kg.m2,by=list(Treatment=GhanaCaboveShrub$Treatment),se)
#Deadwood
GhanaCabovelitterX<-aggregate(GhanaCabovelitter$C.stock.kg.m2,by=list(Treatment=GhanaCabovelitter$Treatment),na.rm=T,mean)
GhanaCabovelitterSE<-aggregate(GhanaCabovelitter$C.stock.kg.m2,by=list(Treatment=GhanaCabovelitter$Treatment),se)
#litter
GhanaCaboveDeadwoodX<-aggregate(GhanaCaboveDeadwood$C.stock.kg.m2,by=list(Treatment=GhanaCaboveDeadwood$Treatment),na.rm=T,mean)
GhanaCaboveDeadwoodSE<-aggregate(GhanaCaboveDeadwood$C.stock.kg.m2,by=list(Treatment=GhanaCaboveDeadwood$Treatment),se)
#Herbveg
GhanaCaboveHerbvegX<-aggregate(GhanaCaboveHerbveg$C.stock.kg.m2,by=list(Treatment=GhanaCaboveHerbveg$Treatment),na.rm=T,mean)
GhanaCaboveHerbvegSE<-aggregate(GhanaCaboveHerbveg$C.stock.kg.m2,by=list(Treatment=GhanaCaboveHerbveg$Treatment),se)

# Produce a graphic file for the image file
#filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/", "Aboveground.C.graph1", "_",Sys.Date(), ".jpeg" )
#jpeg (filename, width=20, height=10, res=400, unit="cm")
# Graph parameters
#par(mfrow=c(2,3),mar=c(5,4,2,2), xpd =F, yaxs ="r", xaxs = "r")
#stripchart(C.stock.kg.m2~Treatment,method ="jitter", data=GhanaCaboveTree,
#           axes=F,mgp=c(2,1,0), bg="white", col="white",
#           ylim=c(0,3.5), xlim=c(.5,4.5), cex=1.75,cex.lab=1.25,vertical=T,
#           pch=c(21),
#           xlab="Aboveground C pool", ylab=expression(paste("Soil C ( kg ",m^-2,")")),
#           main="")
#x axis
#axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(1:4),labels=c("Herbaceous","Litter and Deadwood","Shrubs","Trees"),
#     las=1, col = "black", cex =1.15,col.axis = "black", pos=-0.15, cex.axis=1.1, lwd =1.5, tck=0.02)
#axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.5,4),labels=NA,
#     las=1, col = "black", cex =1.15,col.axis = "black", pos=-0.15, cex.axis=1.1, lwd =1.5, tck=0.00)
#y axis
#axis(2,yaxs="r",mgp=c(2,0.7,0),at=seq(0,4,1),labels=seq(0,4,1),
#     las=2, col = "black", cex =1.15,col.axis = "black", pos=0.5, cex.axis=1.1, lwd =1.5, tck=0.02)
#axis(2,yaxs="r",mgp=c(2,0.7,0),at=seq(-.15,4,1),labels=NA,
#     las=2, col = "black", cex =1.15,col.axis = "black", pos=0.5, cex.axis=1.1, lwd =1.5, tck=0.00)
#Points + error bars
#par(xpd=NA)

#Herbveg
#arrows(c(1,1,1,1), GhanaCaboveHerbvegX$x-GhanaCaboveHerbvegSE$x,c(1,1,1,1), GhanaCaboveHerbvegX$x+GhanaCaboveHerbvegSE$x, length=0.05, angle=90, code=3, col="black")
#points(jitter(c(1,1,1,1),amount=.01), GhanaCaboveHerbvegX$x, col ="black", pch =c(21), lwd =.5, bg = c("black","gray30","grey60","grey90"), cex =2.5)
#Deadwood
#arrows(c(2,2,2,2), GhanaCaboveDeadwoodX$x-GhanaCaboveDeadwoodSE$x,c(2,2,2,2), GhanaCaboveDeadwoodX$x+GhanaCaboveDeadwoodSE$x, length=0.05, angle=90, code=3, col="black")
#points(jitter(c(2,2,2,2),amount=.01), GhanaCaboveDeadwoodX$x, col ="black", pch =c(21), lwd =.5, bg = c("black","gray30","grey60","grey90"), cex =2.5)
#Shrubs
#arrows(c(3,3,3,3), GhanaCaboveShrubX$x-GhanaCaboveShrubSE$x,c(3,3,3,3), GhanaCaboveShrubX$x+GhanaCaboveShrubSE$x, length=0.05, angle=90, code=3, col="black")
#points(jitter(c(3,3,3,3),amount=.01), GhanaCaboveShrubX$x, col ="black", pch =c(21), lwd =.5, bg = c("black","gray30","grey60","grey90"), cex =2.5)
#Trees
#arrows(c(4,4,4,4), GhanaCaboveTreeX$x-GhanaCaboveTreeSE$x,c(4,4,4,4), GhanaCaboveTreeX$x+GhanaCaboveTreeSE$x, length=0.05, angle=90, code=3, col="black")
#points(jitter(c(4,4,4,4),amount=.01), GhanaCaboveTreeX$x, col ="black", pch =c(21), lwd =.5, bg = c("black","gray30","grey60","grey90"), cex =2.5)
#par(xpd=F)

#Legend
#legend(1,3.75,c("Recent late","Recent early","Late old","Unburnt"),pch=21,pt.bg =c("black","gray30","grey60","grey90"), pt.cex = 2.25,cex=1.1,
#       y.intersp =1.1,x.intersp =.8, text.col="black", col=c("black"), bty= "n")
#dev.off()

###############################################################################
#### Alternative graph 5 panels - all data points - mean points with error bars
# Graph 5 panels for each aboveground C pool
###############################################################################

# Ensure fire histories are ordered correctly
# (1) Recent early, (2) Recent late, (3) Old late, (4) Unburnt
# Reorder main data
#GhanaCaboveTree$Treatment <- factor(GhanaCaboveTree$Treatment, levels(GhanaCaboveTree$Treatment)[c(2,1,3,4)])
#GhanaCaboveShrub$Treatment <- factor(GhanaCaboveShrub$Treatment, levels(GhanaCaboveShrub$Treatment)[c(2,1,3,4)])
#GhanaCaboveHerbveg$Treatment <- factor(GhanaCaboveHerbveg$Treatment, levels(GhanaCaboveHerbveg$Treatment)[c(2,1,3,4)])
#GhanaCaboveDeadwood$Treatment <- factor(GhanaCaboveDeadwood$Treatment, levels(GhanaCaboveDeadwood$Treatment)[c(2,1,3,4)])
#GhanaCabovelitter$Treatment <- factor(GhanaCabovelitter$Treatment, levels(GhanaCabovelitter$Treatment)[c(2,1,3,4)])

# Reorder mean
#GhanaCaboveTreeX$Treatment <- factor(GhanaCaboveTreeX$Treatment, levels(GhanaCaboveTreeX$Treatment)[c(2,1,3,4)])
#GhanaCaboveShrubX$Treatment <- factor(GhanaCaboveShrubX$Treatment, levels(GhanaCaboveShrubX$Treatment)[c(2,1,3,4)])
#GhanaCaboveHerbvegX$Treatment <- factor(GhanaCaboveHerbvegX$Treatment, levels(GhanaCaboveHerbvegX$Treatment)[c(2,1,3,4)])
#GhanaCaboveDeadwoodX$Treatment <- factor(GhanaCaboveDeadwoodX$Treatment, levels(GhanaCaboveDeadwoodX$Treatment)[c(2,1,3,4)])
#GhanaCabovelitterX$Treatment <- factor(GhanaCabovelitterX$Treatment, levels(GhanaCabovelitterX$Treatment)[c(2,1,3,4)])

# Reorder SE
#GhanaCaboveTreeSE$Treatment <- factor(GhanaCaboveTreeSE$Treatment, levels(GhanaCaboveTreeSE$Treatment)[c(2,1,3,4)])
#GhanaCaboveShrubSE$Treatment <- factor(GhanaCaboveShrubSE$Treatment, levels(GhanaCaboveShrubSE$Treatment)[c(2,1,3,4)])
#GhanaCaboveHerbvegSE$Treatment <- factor(GhanaCaboveHerbvegSE$Treatment, levels(GhanaCaboveHerbvegSE$Treatment)[c(2,1,3,4)])
#GhanaCaboveDeadwoodSE$Treatment <- factor(GhanaCaboveDeadwoodSE$Treatment, levels(GhanaCaboveDeadwoodSE$Treatment)[c(2,1,3,4)])
#GhanaCabovelitterSE$Treatment <- factor(GhanaCabovelitterSE$Treatment, levels(GhanaCabovelitterSE$Treatment)[c(2,1,3,4)])

# Create file for graph
filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/", "Aboveground.C.Ghana.colour", "_",Sys.Date(), ".tiff" )
jpeg (filename, width=17, height=10, res=400, unit="cm")

# Graph parameters
#par(mfrow=c(1,5),mar=c(5,3,2,.5), xpd =F, yaxs ="r", xaxs = "r")
par(mfrow = c(2,3), oma = c(5,4,0,0) + 0.1, mar = c(2,1,.5,1) + 0.1)
#it goes c(bottom, left, top, right) 
# Tree panel
stripchart(C.stock.kg.m2~Treatment,method ="jitter", data=GhanaCaboveTree,
           axes=F,mgp=c(2,.5,0),bg="gray90", col="gray75",
           ylim=c(0,8), xlim=c(.5,4.5), cex=1.75,cex.lab=1.25,vertical=T,
           pch=c(21,21,22,22),
           xlab="", ylab=expression(paste("Carbon ( kg ",m^-2,")")),
           main="")
axis(1, cex =.9,las=1,lwd =1.5, tck=-0.02,at=c(1:4),labels=c("","","",""))
#text(seq(1, 4, by=1), par("usr")[3] - 0.9, labels = c("Recent early","Recent late","Old late","Unburnt"),pos = 1,xpd = NA, srt = 45)
text(-.75, 4, expression(paste("Carbon ( kg ",m^-2,")")),cex=1.25,xpd = NA, srt = 90)
axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.3,4),labels=NA, las=1, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)
axis(2, las=2,lwd =1.5, tck=-0.02)
axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(-1,8,.2),labels=NA, las=2, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)

#x axis
#axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(1:4),labels=c("Recent late","Recent early","Old late","Unburt"),
#     las=1, col = "black", cex =1.15,col.axis = "black", pos=-0.5, cex.axis=1.1, lwd =1.5, tck=0.02)
#axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.5,4),labels=NA,
#    las=1, col = "black", cex =1.15,col.axis = "black", pos=-0.5, cex.axis=1.1, lwd =1.5, tck=0.00)
#y axis
#axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(0,8,2),labels=seq(0,8,2),
#     las=2, col = "black", cex =1.15,col.axis = "black", pos=0.5, cex.axis=1.1, lwd =1.5, tck=0.02)
#axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(-.5,8,.2),labels=NA,
#     las=2, col = "black", cex =1.15,col.axis = "black", pos=0.5, cex.axis=1.1, lwd =1.5, tck=0.00)

#Subplot title
par(xpd=NA)
text(1.2,8,"(a) Trees", cex=1.25)
par(xpd=T)

#Mean points with SE - Trees
levels(GhanaCaboveTreeX$Treatment)
arrows(c(1,2,3,4), GhanaCaboveTreeX$x-GhanaCaboveTreeSE$x,c(1,2,3,4), GhanaCaboveTreeX$x+GhanaCaboveTreeSE$x, length=0.05, angle=90, code=3, lwd = 1, col=c("light green","black", "tan4", "dark green"))
points(GhanaCaboveTreeX$Treatment, GhanaCaboveTreeX$x, col =c("light green","black", "tan4", "dark green"), pch =c(21,21,22,22), lwd =2, bg = c("light green","white", "white", "dark green"), cex =2.5,xpd = NA)
text(GhanaCaboveTreeX$Treatment, (GhanaCaboveTreeX$x+1.5), labels=c("a","a","a","b"), cex= 1.45)

# Shrub panel
stripchart(C.stock.kg.m2~Treatment,method ="jitter", data=GhanaCaboveShrub,
           axes=F,mgp=c(2,1,0), bg="gray90", col="gray75",
           ylim=c(0,5), xlim=c(.5,4.5), cex=1.75,cex.lab=1.25,vertical=T,
           pch=c(21,21,22,22),
           xlab="", ylab="",
           main="")

#Mean points with SE - Shrubs
levels(GhanaCaboveShrubX$Treatment)
arrows(c(1,2,3,4), GhanaCaboveShrubX$x-GhanaCaboveShrubSE$x,c(1,2,3,4), GhanaCaboveShrubX$x+GhanaCaboveShrubSE$x, length=0.05, angle=90, code=3, col=c("light green","black", "tan4", "dark green"))
points(GhanaCaboveShrubX$Treatment, GhanaCaboveShrubX$x, col =c("light green","black", "tan4", "dark green"), pch =c(21,21,22,22), lwd =2, bg = c("light green","white", "white", "dark green"), cex =2.5,xpd = NA)

# Axes
axis(1, cex =.9,las=1,lwd =1.5, tck=-0.02,at=c(1:4),labels=c("","","",""))
#text(seq(1, 4, by=1), par("usr")[3] -.43, labels = c("Recent early","Recent late","Old late","Unburnt"), srt = 45, pos = 1, xpd = NA)
axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.35,4),labels=NA, las=1, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)
axis(2, las=2,lwd =1.5, tck=-0.02)
axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(-.2,5,.2),labels=NA, las=2, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)

#Subplot title
par(xpd=NA)
text(1.2,5,"(b) Shrubs", cex=1.25)
par(xpd=T)

# Herb veg panel
stripchart(C.stock.kg.m2~Treatment,method ="jitter", data=GhanaCaboveHerbveg,
           axes=F,mgp=c(2,1,0), bg="gray90", col="gray75",
           ylim=c(0,.15), xlim=c(.5,4.5), cex=1.75,cex.lab=1.25,vertical=T,
           pch=c(21,21,22,22),
           xlab="", ylab="",
           main="")

#Mean points with SE - Litter
levels(GhanaCaboveHerbvegX$Treatment)
arrows(c(1,2,3,4), GhanaCaboveHerbvegX$x-GhanaCaboveHerbvegSE$x,c(1,2,3,4), GhanaCaboveHerbvegX$x+GhanaCaboveHerbvegSE$x, length=0.05, angle=90, code=3, col=c("light green","black", "tan4", "dark green"))
points(GhanaCaboveHerbvegX$Treatment, GhanaCaboveHerbvegX$x, col =c("light green","black", "tan4", "dark green"), pch =c(21,21,22,22), lwd =2, bg = c("light green","white", "white", "dark green"), cex =2.5,xpd = NA)

# Axes
axis(1, cex =.9,las=1,lwd =1.5, tck=-0.02,at=c(1:4),labels=c("","","",""))
#text(seq(1, 4, by=1), par("usr")[3]-.015, labels = c("Recent early","Recent late","Old late","Unburnt"), srt = 45, pos = 1, xpd = NA)
axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.35,4),labels=NA, las=1, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)
axis(2, las=2,lwd =1.5, tck=-0.02)
axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(-.006,.15,.1),labels=NA, las=2, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)

#Subplot title
par(xpd=NA)
text(1.7,.15,"(c) Herbaceous", cex=1.25)
par(xpd=T)

#par(mar = c(5,4,0,0) + 0.1)

# Deadwood panel
stripchart(C.stock.kg.m2~Treatment,method ="jitter", data=GhanaCaboveDeadwood,
           axes=F,mgp=c(2,2,0), bg="gray90", col="gray75",
           ylim=c(0,.8), xlim=c(.5,4.5), cex=1.75,cex.lab=1.25,vertical=T,
           pch=c(21,21,22,22),
           xlab="", ylab=expression(paste("Carbon ( kg ",m^-2,")")),
           main="")

#Mean points with SE - Deadwood
levels(GhanaCaboveDeadwoodX$Treatment)
arrows(c(1,2,3,4), GhanaCaboveDeadwoodX$x-GhanaCaboveDeadwoodSE$x,c(1,2,3,4), GhanaCaboveDeadwoodX$x+GhanaCaboveDeadwoodSE$x, length=0.05, angle=90, code=3,col=c("light green","black", "tan4", "dark green"))
points(GhanaCaboveDeadwoodX$Treatment, GhanaCaboveDeadwoodX$x, col =c("light green","black", "tan4", "dark green"), pch =c(21,21,22,22), lwd =2, bg = c("light green","white", "white", "dark green"), cex =2.5,xpd = NA)

# Axes
axis(1, cex =.9,las=1,lwd =1.5, tck=-0.02,at=c(1:4),labels=c("","","",""))
text(seq(1, 4, by=1), par("usr")[3] -.1, labels = c("Recent early","Recent late","Old late","Unburnt"), srt = 45, pos = 1, xpd = NA)
text(-.75, .4, expression(paste("Carbon ( kg ",m^-2,")")),cex=1.25,xpd = NA, srt = 90)
axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.4,4),labels=NA, las=1, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)
axis(2, las=2,lwd =1.5, tck=-0.02)
axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(-.03,.8,.2),labels=NA, las=2, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)

#Subplot title
par(xpd=NA)
text(1.5,.8,"(d) Deadwood", cex=1.25)
par(xpd=T)

# Litter panel
stripchart(C.stock.kg.m2~Treatment,method ="jitter", data=GhanaCabovelitter,
           axes=F,mgp=c(2,1,0), bg="gray90", col="gray75",
           ylim=c(0,.2), xlim=c(.5,4.5), cex=1.75,cex.lab=1.25,vertical=T,
           pch=c(21,21,22,22),
           xlab="", ylab="",
           main="")

#Mean points with SE - Litter
arrows(c(1,2,3,4), GhanaCabovelitterX$x-GhanaCabovelitterSE$x,c(1,2,3,4), GhanaCabovelitterX$x+GhanaCabovelitterSE$x, length=0.05, angle=90, code=3, col=c("light green","black", "tan4", "dark green"))
points(GhanaCabovelitterX$Treatment, GhanaCabovelitterX$x, col =c("light green","black", "tan4", "dark green"), pch =c(21,21,22,22), lwd =2, bg = c("light green","white", "white", "dark green"), cex =2.5,xpd = NA)
text(GhanaCabovelitterX$Treatment, (GhanaCabovelitterX$x+0.03), labels=c("ab","a","ab","b"), cex= 1.45)

# Axes
axis(1, cex =.9,las=1,lwd =1.5, tck=-0.02,at=c(1:4),labels=c("","","",""))
text(seq(1, 4, by=1), par("usr")[3] -.025, labels = c("Recent early","Recent late","Old late","Unburnt"), srt = 45, pos = 1, xpd = NA)
axis(1,yaxs="r",mgp=c(2,0.5,0),at=c(0.35,4),labels=NA, las=1, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)
axis(2, las=2,lwd =1.5, tck=-0.02)
axis(2,yaxs="r",mgp=c(2,0.5,0),at=seq(-.008,.2,.2),labels=NA, las=2, col = "black", cex =1.15,col.axis = "black", cex.axis=1.1, lwd =1.5, tck=0.00)

#Subplot title
par(xpd=NA)
text(1.1,.2,"(e) Litter", cex=1.25)
par(xpd=T)

#Xlab
par(xpd=NA)
text(2,-.12,"Fire history and season", cex=1.25)
par(xpd=T)

# Plot legend outside last panel
par(xpd=NA) 
legend(6, .2,legend=c("Recent early","Recent late","Old late","Unburnt"), pch =c(21,21,22,22), col =c("light green","black", "tan4", "dark green"),
       pt.lwd =2, pt.bg = c("light green","white", "white", "dark green"),cex=1.15, pt.cex =2.5, y.intersp = 1.4, bty="n")
par(xpd=T) 

# Produce a graphic file for the image file # alien cover by source acitiy
dev.off()


################################################################################
# Stack barplot of Ecosystem C seperated aboveground and belowground
################################################################################
# Stack barplot of Aboveground pools
# Stack barplot of belowground C horizons
# Error bars = total aboveground and total belowground
################################################################
# Ecosystem C storage
library(lme4)
library(car)
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
################################################################
# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.aboveground.csv", sep=",",header=TRUE)
# Import data -Belowground C storage
GhanaCbelow<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.belowground.csv", sep=",",header=TRUE)

# Total above and below C stocks
aboveC<-aggregate(GhanaCabove$C.stock.kg.m2, by=list(Treatment=GhanaCabove$Treatment,Location=GhanaCabove$Location),na.rm=T,sum)
belowC<-aggregate(GhanaCbelow$SOC, by=list(Treatment=GhanaCbelow$Treatment,Location=GhanaCbelow$Location),na.rm=T,sum)
GhanaEcosystemC<-merge(aboveC,belowC, by=c("Location","Treatment"))
dim(GhanaEcosystemC) # 27  4
GhanaEcosystemC$EcosystemC<-GhanaEcosystemC$x.x+GhanaEcosystemC$x.y
colnames(GhanaEcosystemC)<-c("Location","Treatment","AboveC","BelowC","EcosystemC")
dim(GhanaEcosystemC) #28  5 
GhanaEcosystemC

# Total aboveground SE
aboveC_mean<-aggregate(GhanaEcosystemC$AboveC, by=list(Treatment=GhanaEcosystemC$Treatment),mean)
aboveC_SE<-aggregate(GhanaEcosystemC$AboveC, by=list(Treatment=GhanaEcosystemC$Treatment),SE)
colnames(aboveC_mean)[2]<-"mean"
aboveCx<-cbind(aboveC_mean,aboveC_SE[2])
colnames(aboveCx)[3]<-"SE"
# Total belowground SE
belowC_mean<-aggregate(GhanaEcosystemC$BelowC, by=list(Treatment=GhanaEcosystemC$Treatment),mean)
belowC_SE<-aggregate(GhanaEcosystemC$BelowC, by=list(Treatment=GhanaEcosystemC$Treatment),SE)
colnames(belowC_mean)[2]<-"mean"
belowCx<-cbind(belowC_mean,belowC_SE[2])
colnames(belowCx)[3]<-"SE"

# Summaries above and below C stocks
#Aboveground C pools
names(GhanaCbelow)
aboveC_pools<-aggregate(GhanaCabove$C.stock.kg.m2, by=list(Treatment=GhanaCabove$Treatment,Pool=GhanaCabove$Pool),na.rm=T,mean)
colnames(aboveC_pools)[3]<-"Carbon"
aboveC_pools$abovebelow<-"above"
belowC_hoz<-aggregate(GhanaCbelow$SOC, by=list(Treatment=GhanaCbelow$Treatment,Pool=GhanaCbelow$Horizon),na.rm=T,mean)
belowC_hoz$Pool<-as.factor(belowC_hoz$Pool)
colnames(belowC_hoz)[3]<-"Carbon"
belowC_hoz$abovebelow<-"below"
Cpools<-rbind(aboveC_pools, belowC_hoz)

#Aboveground means
library(Rmisc) 
library(plyr)
# Aboveground means and SE
abgroup <- summarySE(GhanaCabove, measurevar="C.stock.kg.m2", 
                      groupvars=c("Treatment","Pool"), na.rm = TRUE)

abgroupsum <- ddply(abgroup,.(Treatment),summarize,mean = sum(C.stock.kg.m2))
abgroupSE <- ddply(abgroup,.(Treatment),summarize,SE = SE(C.stock.kg.m2))
ab4 <- merge(abgroup,abgroupsum)
ab5 <- merge(ab4,abgroupSE)

# Belowground means and SE
bpgroup <- summarySE(GhanaCbelow, measurevar="SOC", 
                     groupvars=c("Treatment","Horizon"), na.rm = TRUE)
bpgroup$Horizon<-as.factor(bpgroup$Horizon)
bpgroupsum <- ddply(bpgroup,.(Treatment),summarize,mean = sum(SOC))
bpgroupSE <- ddply(bpgroup,.(Treatment),summarize,SE = SE(SOC))
bp4 <- merge(bpgroup,bpgroupsum)
bp5 <- merge(bp4,bpgroupSE)

#bp6<-bp5
#colnames(bp6)[2]<-"Pool"
#colnames(bp6)[4]<-"C.stock.kg.m2"
#abbp1<-rbind(ab5,bp6)

#abbp1$pool_code<-as.factor(with(abbp1, paste(Treatment, Pool, sep="")))

# Reorder factors
# Reorder fire history and season # "Recent early","Recent late", "Old late", "Unburnt"
ab5$Treatment <- factor(ab5$Treatment , levels = c("Recent early","Recent late", "Old late", "Unburnt"))
bp5$Treatment <- factor(bp5$Treatment , levels = c("Recent early","Recent late", "Old late", "Unburnt"))
#abbp1$Treatment <- factor(abbp1$Treatment , levels = c("Recent early","Recent late", "Old late", "Unburnt"))
# Reorder aboveground pools
#shrubs, then herb veg and then litter+deadwood.
levels(ab5$Pool)
ab5$Pool <- factor(ab5$Pool  , levels = c("Tree","Shrub", "litter.deadwood", "Herb.veg"))
levels(ab5$Pool)<-c("Tree","Shrub", "Litter & Deadwood", "Herbaceous")
# Apply colour pallette to factors
#color_pallete_function <- colorRampPalette(
#  colors = c("light green","black","tan4", "dark green"),
#  space = "Lab" 
#)
# ORDER OF PT.COL not working when relvel treatment
#color_pallete_function <- colorRampPalette(
#  colors = c("tan4","light green","black", "dark green"),
#  space = "Lab" 
#)

#num_colors <- nlevels(abbp1$pool_code)
#num_colors
#diamond_color_colors <- color_pallete_function(num_colors)
#abbp1$pt.col<-diamond_color_colors[abbp1$pool_code]
#levels(as.factor(abbp1$pt.col))

#plot(C.stock.kg.m2~as.numeric(pool_code),data=abbp1, pch=21, bg=pt.col)
# Colours are good

# Split combined colour coding
# Aboveground
#ab6<-abbp1[1:16,]
#ab5<-ab6
#ab5$Treatment 

#plot(C.stock.kg.m2~as.numeric(pool_code),data=ab5, pch=21, bg=pt.col)
# Colours are good

#ab.col<-list(ab5$pt.col)

# Belowground
#bp7<-abbp1[17:32,]
#bp5<-bp7
#bp5$Treatment 
#colnames(bp5)[2]<-"Horizon"
#colnames(bp5)[4]<-"SOC"
#droplevels(bp5)
bp5$Horizon
levels(bp5$Horizon)<-c("0-2cm","2-7cm","7-12cm","12-17cm")
bp5$Horizon <- factor(bp5$Horizon  , levels = c("12-17cm","7-12cm","2-7cm","0-2cm"))

#levels(bp5$Horizon)
#bp5$Horizon <- factor(bp5$Horizon, levels(bp5$Horizon)[c(4,3,2,1)])
library(grid)

# Plot aboveground C
ap <- ggplot(data=ab5, aes(x=Treatment, y=C.stock.kg.m2))
ap<- ap+ geom_bar(aes(fill=Pool),stat="identity", show.legend=T)
ap<- ap+ scale_fill_grey(start = 0.90, end = 0.65)
#ap<- ap+ scale_fill_manual(values=c(ab5$pt.col))
ap<- ap+ scale_y_continuous(breaks=c(0,2,4,6,8,10), expand = c(0, 0),limits=c(0, 12.5))
ap<-ap+geom_errorbar(aes(x=Treatment,ymin=mean-SE, ymax=mean+SE),
                     size=0.5, width=.2,show.legend=F)
ap <- ap + xlab("") + ylab(expression(paste("Carbon ( kg ",m^-2,")")))
ap <- ap + theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_line( size=0.01, color="gray" )
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.text.x=element_blank()
        ,axis.title.y=element_text(size=14, hjust=-.55)
        ,legend.text=element_text(size=12)
        ,axis.ticks.x = element_blank()
        #,axis.ticks.length=unit(-0.2, "cm")
        #,axis.text.y=element_text(hjust=-.4) # HACK MESSY!
        ,axis.line.y = element_line(color="black", size = .25)
        ,legend.position=c(.25, .8)
        ,plot.margin = unit(c(5,8.5,-4.7,5), "mm")) # 8 expanded right margin
#ap <- ap + guides(fill=guide_legend(title="Aboveground pools"))
#ap <- ap + annotate(geom="text", x=4.75, y=4.8, label="Litter",color="black")
#ap <- ap + annotate(geom="text", x=4.72, y=4.4, label="Deadwood",color="black")
#ap <- ap + annotate(geom="text", x=4.75, y=4, label="Herb veg",color="black")
#ap <- ap + annotate(geom="text", x=4.75, y=3.1, label="Shrub",color="black")
#ap <- ap + annotate(geom="text", x=4.75, y=1.2, label="Trees",color="black")
ap
gt <- ggplot_gtable(ggplot_build(ap)) # Turn off clippig = annotate works
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)


# Plot belowground C
bp <- ggplot(data=bp5, aes(x=Treatment, y=SOC))
bp<- bp+ geom_bar(aes(fill=Horizon),stat="identity",show.legend=T) 
#bp<- bp+ geom_bar(aes(fill=factor(Horizon, levels=c("17","12","7","2")))
#                  ,stat="identity",show.legend=F) 
bp<- bp+ scale_fill_grey(start = 0.55, end = 0.2)
#bp<- bp+ scale_fill_manual(values=c(bp5$pt.col)) 
bp<-  bp + scale_y_reverse(breaks=c(2,4,6,8,10,12),labels=c(2,4,6,8,10,12),expand = c(0, 0))
bp<-bp+geom_errorbar(aes(x=Treatment,ymin=mean-SE, ymax=mean+SE),
                     size=0.5, width=.2)
bp <- bp + xlab("Fire history and season") + ylab("")
bp <- bp + theme_bw() +
  theme(plot.background = element_blank()
        #,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,panel.grid.major.x = element_line( size=0.01, color="gray" )
        ,panel.grid.major.y = element_blank() 
        ,axis.text=element_text(size=12)
        ,axis.title.y=element_text(size=14, hjust=-.55)
        ,axis.title.x=element_text(size=14,vjust=-.4)
        ,legend.text=element_text(size=12)
        ,axis.ticks.x = element_blank()
        #,axis.ticks.length=unit(-0.2, "cm")
        #,axis.text.y=element_text(hjust=-.4) # HACK MESSY!
        ,axis.line.y = element_line(color="black", size = .25)
        ,legend.position=c(.75, 1.8)
        ,plot.margin = unit(c(-1.45,8.5,5,5), "mm"))
bp <- bp + guides(fill=guide_legend(title="Soil horizons",reverse=TRUE)) # Need to reverse the legend
#bp <- bp + guides(fill=guide_legend(title="Aboveground pools"))
#bp <- bp + annotate(geom="text", x=4.75, y=2, label="2 cm",color="black")
#bp <- bp + annotate(geom="text", x=4.75, y=3.8, label="7 cm",color="black")
#bp <- bp + annotate(geom="text", x=4.75, y=6, label="12 cm",color="black")
#bp <- bp + annotate(geom="text", x=4.75, y=9, label="17 cm",color="black")
bp
bt <- ggplot_gtable(ggplot_build(bp)) # Turn off clippig = annotate works
bt$layout$clip[bt$layout$name == "panel"] <- "off"
grid.draw(bt)


#library(grid)
filename <- paste0("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/", "EcoC_Ghana.stackbar.BWpool",
                   "_",Sys.Date(), ".jpeg" )
jpeg (filename, width=15, height=20, res=400, unit="cm")

grid.newpage()
#grid.draw(rbind(ggplotGrob(ap), ggplotGrob(bp), size = "last"))
grob.ap<-ggplotGrob(ap)
grob.bp<-ggplotGrob(bp)
grob.ap$layout$clip[grob.ap$layout$name=="panel"] <- "off"
grob.bp$layout$clip[grob.bp$layout$name=="panel"] <- "off"
grid.draw(rbind(grob.ap, grob.bp, size = "last"))

dev.off()
