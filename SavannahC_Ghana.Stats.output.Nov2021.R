#########################################################################################################################
####Ghana: savannah fire history - C storage ####
#Stuart Smith
#17/6/2017 
#########################################################################################################################
#clear system & package libraries
rm(list=ls())
library(lattice)
library(MASS)
library(dplyr)
library(sp)
library(rgdal)
library(geosphere)
library(lme4)
library(DHARMa) 
library(broom.mixed)
################################################################

################################################################
#### Aboveground C storage ####
################################################################
# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("./Ghana.aboveground.copy.csv", sep=",",header=TRUE)

dim(GhanaCabove) # 140 rows # 4 columns
str(GhanaCabove)
head(GhanaCabove) # NAs
names(GhanaCabove)

# SE - NAS 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#Housekeeping
GhanaCabove$Pool<-as.factor(GhanaCabove$Pool)
GhanaCabove$Burn_history<-as.factor(GhanaCabove$Burn_history)
GhanaCabove$Location<-as.factor(GhanaCabove$Location)
table(GhanaCabove$Location,GhanaCabove$Burn_history) # Location and burn history non-independent

#### Spatial cluster coordinates ####
# Insert coordinates
GhanaCcoords<-read.csv("./Coordinates.study.sites.csv", sep=",",header=TRUE)
# example data from the thread
x<-GhanaCcoords$Latitude
y<-GhanaCcoords$Longitude

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
xy$Location<-GhanaCcoords$Location

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in this case 5000 m
d=9000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)
xy<-as.data.frame(xy)
xy$clust
clustxy<-xy[,c("Location","clust")]
clustxy$Location<-as.factor(clustxy$Location)
GhanaCabove$Location
clustxy$Location<-gsub(" ", "",clustxy$Location)
GhanaCabove<-merge(GhanaCabove,clustxy, by=c("Location"), all.x=T)

table(GhanaCabove$clust,GhanaCabove$Burn_history) # Location and burn history non-independent

# Explore data - Trt x soil depth - boxplot
bwplot(C.stock.kg.m2~Burn_history|Pool, data=GhanaCabove) 
# Trees huge in unburnt - not so much in tree pool
#Unburnt deadwood and litter higher
# Shrub Recent early higher, but 

# Seperate graphs for smaller pools deadwood, Herb Veg and litter
par(mfrow=c(1,3))
# Deadwood
GhanaCaboveDeadwood<-GhanaCabove[GhanaCabove$Pool=="Deadwood",]
boxplot(C.stock.kg.m2~Burn_history, data=GhanaCaboveDeadwood, main="Deadwood")
#Herb.veg
GhanaCaboveHerb.veg<-GhanaCabove[GhanaCabove$Pool=="Herb.veg",]
boxplot(C.stock.kg.m2~Burn_history, data=GhanaCaboveHerb.veg, main="Herb veg")
#litter
GhanaCabovelitter<-GhanaCabove[GhanaCabove$Pool=="Litter",]
boxplot(C.stock.kg.m2~Burn_history, data=GhanaCabovelitter, main="Litter")

# Deadwood has all the zeroes that may influence the analysis 
aggregate(GhanaCabove$C.stock.kg.m2, by=list(Pool=GhanaCabove$Pool),FUN=function(x) sum(x== 0))
#Pool  C.stock.kg.m2 = 0
#1 deadwood 17
#2 Herb.veg  0
#3   litter  0
#4    Shrub  0
#5     Tree  0

aggregate(GhanaCabove$C.stock.kg.m2, by=list(Burn_history= GhanaCabove$Burn_history, Pool=GhanaCabove$Pool),FUN=function(x) sum(x== 0))
# Limited bias towards a particular fire history
#Treatment     Pool x
#1      Old late deadwood 3
#2  Recent early deadwood 4
#3   Recent late deadwood 5
#4       Unburnt deadwood 5

poolC<-aggregate(C.stock.kg.m2~Pool,GhanaCabove,mean)
sum(poolC$C.stock.kg.m2)
(0.03517505/2.863034)*100 # 1.3% # Deadwood
(0.03280402/2.863034)*100 # 1.4% # Herb.veg
(0.06578942/2.863034)*100 # 2.3% # Litter
(1.76016907/2.863034)*100 # 65% # Shrub
(0.96909661/2.863034)*100 # 33% # Tree
sEcoC<-aggregate(C.stock.kg.m2~Pool+Burn_history,GhanaCabove,mean)
SumEcoC<-aggregate(C.stock.kg.m2~Burn_history,sEcoC,sum)
colnames(SumEcoC)[2]<-"Ecosystem_C"
SumEcoC<-merge(sEcoC,SumEcoC,by=c("Burn_history"))
SumEcoC$prop_C<-(SumEcoC$C.stock.kg.m2/SumEcoC$Ecosystem_C)*100
aggregate(prop_C~Pool+Burn_history,SumEcoC,mean)

# Correlation between shrub C and tree C 
GhanaCaboveshrub<-GhanaCabove[GhanaCabove$Pool=="Shrub",]
GhanaCabovetree<-GhanaCabove[GhanaCabove$Pool=="Tree",]
par(mfrow=c(1,1))
plot(GhanaCaboveshrub$C.stock.kg.m2~GhanaCabovetree$C.stock.kg.m2, pch =21)
# No relationship between Tree and shrub C

### Reference Unburnt history
GhanaCabove$Burn_history<- relevel(GhanaCabove$Burn_history, ref = "Unburnt")

### Reference Herb veg
levels(GhanaCabove$Pool)
GhanaCabove$Pool<- relevel(GhanaCabove$Pool, ref = "Herb.veg")
GhanaCabove$Pool<- relevel(GhanaCabove$Pool, ref = "Shrub")

#### Power analysis: aboveground ####
# https://slcladal.github.io/pwr.html
# Green, Peter, and Catriona J. MacLeod. 2016b. “Simr: An r Package for Power Analysis of Generalised Linear Mixed Models by Simulation.” Methods in Ecology and Evolution 7 (4): 493–98. https://doi.org/10.1111/2041-210X.12504.
#the size of the effect (bigger effects are easier to detect)
#the variability of the effect (less variability makes it easier to detect an effect), and
#the sample size (the bigger the sample size, the easier it is to detect an effect)
library(pwr)
library(simr)
library(effectsize)
library(sjPlot)

# calculate minimal sample size
table(GhanaCabove$Burn_history,GhanaCabove$Pool)
table(GhanaCabove$Burn_history,GhanaCabove$clust)
table(GhanaCabove$Burn_history,GhanaCabove$Location) # 28 sites, 7 per burn history

# Create unique site code based on location
#x<-GhanaCcoords$Latitude
#y<-GhanaCcoords$Longitude
#xy$coords<-as.factor(with(xy, paste(coords.x1,coords.x2, sep="_")))
#sites<-xy %>% select(Location,coords)
#colnames(sites)[2]<-"site"
#sites$Location<-gsub(" ", "",sites$Location)
#GhanaCabove<-merge(GhanaCabove,sites,by=c("Location"))
GhanaCabove$site<-GhanaCabove$Location

# One way anova
#Effect size small ≥ 0.02, medium ≥ 0.15, and large ≥ 0.35 # Cohen 1988
pwr.anova.test(k=4*5,            # Per group
               f=.25,          # moderate effect size
               sig.level=.05,  # alpha/sig. level = .05
               n=7)   # power = 0.31
# We would only detect a minimum size effect in 31% of chances

# Power analysis - LLM
# Intercept + slopes for fixed effects 
# (Intercept + Group, SentenceType, WordOrder, and an interaction between Group * SentenceType)
fixed <- c(rep(.52,20)) # Effective size at .52 # Odds ratio 1.68 weak effects
# Random intercepts for Sentence and ID
rand <- list(0.1)
# res. variance
res <- 0.2 # Higher than field osbervations 
m1 <- makeLmer(y~Burn_history+Pool+Pool/Burn_history+(1|site), 
               fixef=fixed, 
               VarCorr=rand, 
               sigma=res, 
               data=GhanaCabove)
#sjPlot::tab_model(m1)
sim_pool_burn <- simr::powerSim(m1, nsim=20,test = fcompare(y ~ Burn_history+Pool)) 
sim_pool_burn # 100% power 

# Plotting 
m1_as <- simr::extend(m1, along="site", n=30)
pcAbove<-simr::powerCurve(m1_as, test = fcompare(y ~ Burn_history+Pool),
                      along="site",nsim=100)
filenameAbove <- paste0("./", "Power_analysis_aboveground_carbon", ".jpeg" )
jpeg(filenameAbove ,width= 12, height = 10,units ="cm",bg ="transparent", res = 800)
plot(pcAbove)
dev.off()

# extract fixed effect estimates
estimatesfixedeffects <- fixef(m1)
# convert estimates into odds ratios
exp(estimatesfixedeffects)
#small effect (Cohen’s d 0.2, OR = 1.68)
#medium effect (Cohen’s d 0.5, OR = 3.47)
#strong effect (Cohen’s d 0.8, OR = 6.71)

#### LMM: Aboveground mixed effect model #### 
# Fixed factors Burn history nested within Depth and random factor is site
GhanaCmixedAbove<-lmer((C.stock.kg.m2^.2)~Burn_history+Pool+Pool/Burn_history+(1|clust),
                data = GhanaCabove)
summary(GhanaCmixedAbove)
anova(GhanaCmixedAbove) # Pool significant
AIC(GhanaCmixedAbove) # 11.95403

# Export summary
GAboveCsummary <- as.data.frame(broom.mixed::tidy(GhanaCmixedAbove, conf.int = T))
write.csv(GAboveCsummary,file="./Model_summaries/GAboveCsummary.csv")

# Residual plot
res <- simulateResiduals(GhanaCmixedAbove, plot = T) # Excellent QQ and resid vs predict issue lower portion
plot(GhanaCmixedAbove)

# Reduce model?
drop1(GhanaCmixedAbove,test="Chisq") # Nested interaction NS

# Update models to remove terms
GhanaCmixedAbove2<-lmer((C.stock.kg.m2^.2)~Burn_history+Pool+(1|clust),data = GhanaCabove)
GhanaCmixedAbove3<-lmer((C.stock.kg.m2^.2)~Burn_history+(1|clust),data = GhanaCabove)
GhanaCmixedAbove4<-lmer((C.stock.kg.m2^.2)~Pool+(1|clust),data = GhanaCabove)

anova(GhanaCmixedAbove,GhanaCmixedAbove2) # NS
anova(GhanaCmixedAbove2,GhanaCmixedAbove3) # Pool significant
anova(GhanaCmixedAbove2,GhanaCmixedAbove4) # NS

# Contrasts
library(emmeans)
test1 <- emmeans(GhanaCmixedAbove,~Burn_history)
test1<-contrast(regrid(test1),method = "pairwise") #dunnett
AboveC1 <- as.data.frame(broom.mixed::tidy(test1, conf.int = T)) # All significant different
AboveC1

test2 <- emmeans(GhanaCmixedAbove,~Pool)
test2<-contrast(regrid(test2),method = "pairwise") #dunnett
AboveC <- as.data.frame(broom.mixed::tidy(test2, conf.int = T)) # All significant different - not tree
AboveC

test3 <- emmeans(GhanaCmixedAbove,~+Pool/Burn_history)
test3<-contrast(regrid(test3),method = "pairwise") #dunnett
AboveC3 <- as.data.frame(broom.mixed::tidy(test3, conf.int = T)) # All significant different
AboveC3
AboveCP05L<-droplevels(AboveC3[AboveC3$adj.p.value<0.05 & !is.na(AboveC3$adj.p.value) ,])
AboveCP05L

#Pairwise contrasts accounting for df and missing values use *lsmeans*
library(multcompView)
library(lsmeans)
library(lmerTest)
library(Hmisc)
library(pbkrtest)

#GhanaCmixedAbove<-lmer((C.stock.kg.m2^.2)~Burn_history+Pool+Pool/Burn_history+(1|clust),data = GhanaCabove)
#anova(GhanaCmixedAbove)
lsmeans(GhanaCmixedAbove, pairwise~Burn_history, adjust="Tukey") # Correct number of rows - but all factors signifcant - not true
lsmeans(GhanaCmixedAbove, pairwise~Pool, adjust="Tukey") # Correct number of rows - but all factors signifcant except her and litter

leastsquare <-lsmeans(GhanaCmixedAbove, pairwise~Burn_history|Pool, adjust="Tukey") # Correct number of rows - but all factors signifcant - not true
leastsquare

#leastsquareLT <-lsmeansLT(GhanaCmixedAbove,test.effs= "Pool/Burn_history" ) # Correct number of rows - but all factors signifcant - not true
#leastsquareLT

#leastsquare.inter <-difflsmeans(GhanaCmixedAbove,test.effs= "Burn_history:Pool" ) # Correct number of rows - but all factors signifcant - not true
#leastsquare.inter 

################################################################
#### Belowground C storage ####
################################################################

# Import data - belowground C storage data for Ghana
GhanaC<-read.csv("./Ghana.belowground_correct.csv", sep=",",header=TRUE)

dim(GhanaC) # 109 rows # 6 columns
str(GhanaC)
head(GhanaC) # NAs
names(GhanaC)

# SE - NAS 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# Add the cluster
GhanaCbelow<-merge(GhanaC,clustxy, by=c("Location"), all.x=T)

#Housekeeping
GhanaCbelow$Horizon<-as.factor(GhanaCbelow$Horizon)
GhanaCbelow$Depth<-as.factor(GhanaCbelow$Depth)
GhanaCbelow$Burn_history<-as.factor(GhanaCbelow$Burn_history)
GhanaCbelow$Location<-as.factor(GhanaCbelow$Location)
table(GhanaCbelow$clust,GhanaCbelow$Burn_history) # Location and burn history non-independent

# Explore data - Trt x soil depth - boxplot
bwplot(C.density ~Burn_history|Horizon, data=GhanaCbelow,
       strip=strip.custom(var.name="",
                          factor.levels=c("0-2 cm","2-7 cm", "7-12cm","12-17cm"),
                          strip.levels=rep(TRUE,4))) 

# Reduction in recent late soil C throughout lowest at depth, lower deeper

### Reference Unburnt
GhanaCbelow$Horizon<- relevel(GhanaCbelow$Horizon, ref = "2")
GhanaCbelow$Burn_history<- relevel(GhanaCbelow$Burn_history, ref = "Unburnt")

#### Power analysis: belowground ####

# calculate minimal sample size
table(GhanaCbelow$Burn_history,GhanaCbelow$Horizon)
table(GhanaCbelow$Burn_history,GhanaCbelow$clust)
table(GhanaCbelow$Burn_history,GhanaCbelow$Location) # 28 sites, 7 per burn history
GhanaCbelow$site<-GhanaCbelow$Location

# One way anova
#Effect size small ≥ 0.02, medium ≥ 0.15, and large ≥ 0.35 # Cohen 1988
pwr.anova.test(k=4*4,            # Per group
               f=.25,          # moderate effect size
               sig.level=.05,  # alpha/sig. level = .05
               n=7)   # power = 0.2757416
# We would only detect a minimum size effect in 28% of chances

# Power analysis - LLM
# Intercept + slopes for fixed effects 
# (Intercept + Group, SentenceType, WordOrder, and an interaction between Group * SentenceType)
fixed2 <- c(rep(.52,16)) # Effective size at .52 # Odds ratio 1.68 weak effects
# Random intercepts for Sentence and ID
rand2 <- list(0.1)
# res. variance
res2 <- 0.2
m2 <- makeLmer(y~Burn_history+Horizon+Horizon/Burn_history+(1|site), 
               fixef=fixed2, 
               VarCorr=rand2, 
               sigma=res2, 
               data=GhanaCbelow)
#sjPlot::tab_model(m1)
sim_pool_burn <- simr::powerSim(m2, nsim=20,test = fcompare(y ~ Burn_history+Horizon)) 
sim_pool_burn # 100%

# Plotting 
m2_as <- simr::extend(m2, along="site", n=30)
pcBelow<-simr::powerCurve(m2_as, test = fcompare(y ~ Burn_history+Horizon),
                          along="site",nsim=100)
filenameBelow <- paste0("./", "Power_analysis_belowground_carbon", ".jpeg" )
jpeg(filenameBelow ,width= 12, height = 10,units ="cm",bg ="transparent", res = 800)
plot(pcBelow)
dev.off()
# extract fixed effect estimates
estimatesfixedeffects <- fixef(m1)
# convert estimates into odds ratios
exp(estimatesfixedeffects)
#small effect (Cohen’s d 0.2, OR = 1.68)
#medium effect (Cohen’s d 0.5, OR = 3.47)
#strong effect (Cohen’s d 0.8, OR = 6.71)





#### LMM: Belowground mixed effect model ###
GhanaCmixed<-lmer(C.density~Burn_history+Horizon+Horizon/Burn_history+(1|clust),na.action=na.omit, data = GhanaCbelow, REML=T)
summary(GhanaCmixed)
vcov(GhanaCmixed) # Horizon comparison across burn histories 	
anova(GhanaCmixed) # Burn history and horizon significant

# Export summary
GBelowCsummary <- as.data.frame(broom.mixed::tidy(GhanaCmixed, conf.int = T))
write.csv(GBelowCsummary,file="./Model_summaries/GBelowCsummary.csv")

# Residual plot
res2 <- simulateResiduals(GhanaCmixed, plot = T) # All good
plot(GhanaCmixed)

# Reduce model?
drop1(GhanaCmixed,test="Chisq") # Nested interaction NS

# Update models to remove terms
GhanaCmixed2<-lmer(C.density~Burn_history+Horizon+(1|clust),na.action=na.omit, data = GhanaCbelow, REML=T)
GhanaCmixed3<-lmer(C.density~Burn_history+(1|clust),na.action=na.omit, data = GhanaCbelow, REML=T)
GhanaCmixed4<-lmer(C.density~Horizon+(1|clust),na.action=na.omit, data = GhanaCbelow, REML=T)

anova(GhanaCmixed,GhanaCmixed2) # NS
anova(GhanaCmixed2,GhanaCmixed3) # Horizon significant
anova(GhanaCmixed2,GhanaCmixed4) # Burn history significant

# Contrasts
lsmeans(GhanaCmixed, pairwise~Burn_history, adjust="Tukey") # Correct number of rows - but all factors signifcant - not true
lsmeans(GhanaCmixed, pairwise~Horizon, adjust="Tukey") # Correct number of rows - but all factors signifcant - not true

leastsquare2 <-lsmeans(GhanaCmixed, pairwise~Burn_history|Horizon, adjust="Tukey") # Correct number of rows - but all factors signifcant - not true
leastsquare2

# Contrasts (emmeans)
library(emmeans)
test3 <- emmeans(GhanaCmixed,~Burn_history)
test3<-contrast(regrid(test3),method = "pairwise")
test3
#AboveC <- as.data.frame(broom.mixed::tidy(test3, conf.int = T))
#AboveCP05L<-droplevels(AboveC[AboveC$adj.p.value<0.05 & !is.na(AboveC$adj.p.value) ,])
#contrast                   estimate   SE    df t.ratio p.value
#Unburnt - Old late         -0.05928 0.1006  2.40 -0.589  0.9281 
#Unburnt - Recent early      0.00261 0.0983  2.40  0.027  1.0000 
#Unburnt - Recent late       0.28336 0.0926  2.40  3.060  0.1873 
#Old late - Recent early     0.06190 0.0827  8.41  0.748  0.8750 
#Old late - Recent late      0.34264 0.0851  8.41  4.027  0.0147 
#Recent early - Recent late  0.28075 0.0818 12.34  3.432  0.0216 

test4 <- emmeans(GhanaCmixed,~Horizon)
test4<-contrast(regrid(test4),method = "pairwise")
test4 # 2 cm smaller compared to all other horizons

#Horizon carbon
OldLate<-droplevels(GhanaC[GhanaC$Burn_history=="Old late",])
levels(as.factor(OldLate$Yr_last_fire))
OldLateNew<-droplevels(OldLate[OldLate$Yr_last_fire<2009,])
OldLateOld<-droplevels(OldLate[OldLate$Yr_last_fire>2009,])
OldLateN<-aggregate(C.density~Horizon,OldLateNew,mean)
OldLateO<-aggregate(C.density~Horizon,OldLateOld,mean)
mean((((OldLateO-OldLateN)/OldLateO)*100)$C.density)
# 6.13 % higher for older LATE OLD

################################################################
#### Ecosystem C storage ####
################################################################

# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("Fire_in_the_Mole/Ghana.aboveground.copy.csv", sep=",",header=TRUE)
names(GhanaCabove)

# Import data -Belowground C storage
GhanaCbelow<-read.csv("Fire_in_the_Mole/Ghana.belowground_correct.csv", sep=",",header=TRUE)
names(GhanaCbelow)
head(GhanaCbelow)

#Remove columns from GhanaCabove and GhanaCbelow that do not match: pool, horizon, depth and SOC 
GhanaCabove2<-GhanaCabove[,-c(3)] # Removes pool
head(GhanaCabove2)
GhanaCbelow2<-GhanaCbelow[,-c(3:5,7)] # Removes horizon, depth and SOC
head(GhanaCbelow2)
colnames(GhanaCbelow2)<-c("Burn_history","Location","C.stock.kg.m2")

#Combine above and belowground datasets
GhanaEcosystemC<-rbind(GhanaCabove2,GhanaCbelow2)

GhanaCabove2L<-aggregate(C.stock.kg.m2~Burn_history+Location,GhanaCabove2,sum)
aggregate(C.stock.kg.m2~Burn_history,GhanaCabove2L,mean)
aggregate(C.stock.kg.m2~Burn_history,GhanaCabove2L,sd)

GhanaCbelow2L<-aggregate(C.stock.kg.m2~Burn_history+Location,GhanaCbelow2,sum)
aggregate(C.stock.kg.m2~Burn_history,GhanaCbelow2L,mean)
aggregate(C.stock.kg.m2~Burn_history,GhanaCbelow2L,sd)

# Summaries Ecosystem C
aggregate(GhanaEcosystemC$C.stock.kg.m2, by=list(Burn_history=GhanaEcosystemC$Burn_history,Location=GhanaEcosystemC$Location),na.rm=T,sum)
GhanaEcosystemCTot<-aggregate(GhanaEcosystemC$C.stock.kg.m2, by=list(Burn_history=GhanaEcosystemC$Burn_history,Location=GhanaEcosystemC$Location),na.rm=T,sum)
colnames(GhanaEcosystemCTot)<-c("Burn_history","Location","C.stock.kg.m2")
dim(GhanaEcosystemCTot) #28  3 - only 28 datapoints - sufficient for mixed model? 
(1-(2.823239/3.862695))*100 # 26.91012 difference late vs early season

# Add the cluster
GhanaEcosystemCTot<-merge(GhanaEcosystemCTot,clustxy, by=c("Location"), all.x=T)

### Reference Unburnt history
GhanaEcosystemCTot$Burn_history<-as.factor(GhanaEcosystemCTot$Burn_history)
GhanaEcosystemCTot$Burn_history<- relevel(GhanaEcosystemCTot$Burn_history, ref = "Unburnt")

# Mixed effect model # Ecosystem C
library(lme4)
names(GhanaEcosystemCTot)
GhanaCmixedEco<-lmer(C.stock.kg.m2~Burn_history+(1|clust),na.action=na.omit, data = GhanaEcosystemCTot, REML=T)
summary(GhanaCmixedEco)
anova(GhanaCmixedEco) # Treatment marginal for Ecosystem C 

# Residual plot
res3 <- simulateResiduals(GhanaCmixedEco, plot = T) # All good - not many data points
plot(GhanaCmixedEco)

# Reduce model?
drop1(GhanaCmixedEco, test="Chisq") # Marginally significant

# Contrasts Burn_history
test5 <- emmeans(GhanaCmixedEco,~Burn_history)
test5<-contrast(regrid(test5),method = "pairwise")
test5 # None significant

#######################################################################################
#### Tree and shrub densities and allometerics in relationship soil carbon ####
#######################################################################################
# Import data - Tree shrub data

GhanaTreeShrub<-read.csv("Fire_in_the_Mole/Trees.shrubs.allomet.csv", sep=",",header=TRUE)

# Data structure
names(GhanaTreeShrub)
dim(GhanaTreeShrub) # 616 rows # 12 columns
str(GhanaTreeShrub) # 29 levels for plots? Uneven
head(GhanaTreeShrub) # NAs

###########################################
# Tree/shrub frequency - density
##########################################

# Counts of shrubs and trees in each fire history and plot
library(plyr)
names(GhanaTreeShrub)
TreeShrubCount<-count(GhanaTreeShrub, c("Type","Treatment","Plot","Species"))
TreeShrubCount
dim(TreeShrubCount)  # 122 data points

# Biplot of count data for shrubs and trees
bwplot(freq~Treatment|Type, data=TreeShrubCount) 

# Summary of tree densities 
names(TreeShrubCount)
TreeShrubdenseL<-aggregate(((freq/625)*10000)~Treatment+Type+Plot, data=TreeShrubCount, mean)
TreeShrubdense<-aggregate(((freq/625)*10000)~Treatment+Type, data=TreeShrubCount, mean)
TreeShrubdenseT<-droplevels(TreeShrubdense[TreeShrubdense$Type=="Tree",])
mean(TreeShrubdense$`((freq/625) * 10000)`) # 76.09425

# Mixed effect model # account for plot
library(lme4)
# Convert to density = divide by 25*25=625 m2 (*10000 for ha)

# Analyse count data = use poisson?
names(TreeShrubCount)
GhanaCountGLM<-glmer(freq~Type/Treatment+(1|Plot),na.action=na.omit, family =poisson(link="log") ,data =TreeShrubCount)
summary(GhanaCountGLM)# First part is of interest - second part is cntrasting shurbs vs trees in same treatment
anova(GhanaCountGLM) # Difference in number of Treatment within type (i,e, shrubs and trees) 

# Check model assumptions
par(mfrow=c(2,2))
plot(fitted(GhanaCountGLM),residuals(GhanaCountGLM)) # Looks very conical but rest are good due to smaller values
qqnorm(resid(GhanaCountGLM))
qqline(resid(GhanaCountGLM))
hist(residuals(GhanaCountGLM), col="darkgray") 

# Overdispersion due to poisson - which never works....
overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(GhanaCountGLM)
#       chisq        ratio          rdf            p 
#3.552230e+02 3.036094e+00 1.170000e+02 7.737083e-26 # Highly significant 

# Reorder date - tree first
TreeShrubCount$Type<- factor(TreeShrubCount$Type, levels(TreeShrubCount$Type)[c(2,1)])
GhanaCountGLM<-glmer(freq~Treatment/Type+(1|Plot)+(1|Species),na.action=na.omit, family =poisson(link="log") ,data =TreeShrubCount)
summary(GhanaCountGLM)
anova(GhanaCountGLM)  # First compoent here important - 

# Contrasting models with and withoutn Treatment and type
GhanaCountGLM<-glmer(freq~TREATMENT/Type+(1|PLOT)+(1|Species),na.action=na.omit, family =poisson(link="log") ,data =TreeShrubCount)
summary(GhanaCountGLM)
GhanaCountGLM2<-glmer(freq~1+(1|PLOT)+(1|Species),na.action=na.omit, family =poisson(link="log") ,data =TreeShrubCount)
summary(GhanaCountGLM)

# Analyse densities
GhanaDense<-lmer((freq/625)~TREATMENT/Type+(1|PLOT),na.action=na.omit, data =TreeShrubCount)
summary(GhanaDense)
anova(GhanaDense) # Difference in number of No Treatment differences

# Check assumptions
par(mfrow=c(2,2))
plot(fitted(GhanaDense),residuals(GhanaDense)) # conical
qqnorm(resid(GhanaDense))
qqline(resid(GhanaDense)) # Good fit - again one very low outlier...
hist(residuals(GhanaDense), col="darkgray") # One low outlier

# Using density does not improve model - does not work

###########################################
# Tree/shrub frequency - diameters
##########################################
names(GhanaTreeShrub)
# Tree dbh
aggregate(GhanaTreeShrub$DBH, by=list(Treatment=GhanaTreeShrub$TREATMENT,Plot=GhanaTreeShrub$PLOT,Type=GhanaTreeShrub$Type),na.rm=T,mean)
GhanaTreedbh<-aggregate(GhanaTreeShrub$DBH, by=list(Treatment=GhanaTreeShrub$TREATMENT,Plot=GhanaTreeShrub$PLOT,Type=GhanaTreeShrub$Type),na.rm=T,mean)

# All shrubs have NaNs
GhanaTreedbh<-GhanaTreedbh[! is.na(GhanaTreedbh$x), ] # removes with NaN - only trees left
colnames(GhanaTreedbh)<-c("Treatment","Plot","Type","Diameter")
head(GhanaTreedbh)

bwplot(Diameter~Treatment|Type, data=GhanaTreedbh) # Late old shrubs have very low basal diameter

# Shrub basal diameter
aggregate(GhanaTreeShrub$DSB, by=list(Treatment=GhanaTreeShrub$TREATMENT,Plot=GhanaTreeShrub$PLOT,Type=GhanaTreeShrub$Type),na.rm=T,mean)
GhanaShrubdsb<-aggregate(GhanaTreeShrub$DSB, by=list(Treatment=GhanaTreeShrub$TREATMENT,Plot=GhanaTreeShrub$PLOT,Type=GhanaTreeShrub$Type),na.rm=T,mean)

# All trees have NaNs
GhanaShrubdsb<-GhanaShrubdsb[! is.na(GhanaShrubdsb$x), ] # removes with NaN - only trees left
colnames(GhanaShrubdsb)<-c("Treatment","Plot","Type","Diameter")
head(GhanaShrubdsb)

bwplot(Diameter~Treatment|Type, data=GhanaShrubdsb) # Late old shrubs have very low basal diameter

# Combine summaries for mean tree and shurb data by 
TreeShrubdi<-rbind(GhanaTreedbh,GhanaShrubdsb)

# Plot tree and shrub diameter in relation to fire history
bwplot(Diameter~Treatment|Type, data=TreeShrubdi) # Late old shrubs have very low basal diameter

###################################
# Shrub basal diameter only analysis
####################################
# Shrub only data
GhanaShrubs<-GhanaTreeShrub[GhanaTreeShrub$Type=="Shrub",]
GhanaShrubs

# Mixed model of basal diameter of shrubs by fire history
names(GhanaShrubs)
GhanaShrubsmixed<-lmer(DSB~TREATMENT+(1|PLOT),na.action=na.omit, data = GhanaShrubs)
summary(GhanaShrubsmixed)
anova(GhanaShrubsmixed) # Likely NS

# Check assumptions
par(mfrow=c(2,2))
plot(fitted(GhanaShrubsmixed),residuals(GhanaShrubsmixed)) # poor fit - still conical - due to zeroes
qqnorm(resid(GhanaShrubsmixed))
qqline(resid(GhanaShrubsmixed)) # Poor - higher values depart from qqline
hist(residuals(GhanaShrubsmixed), col="darkgray") # Slightly skewed

# Log transform
GhanaShrubsmixed<-lmer(log(DSB)~TREATMENT+(1|PLOT),na.action=na.omit, data = GhanaShrubs)
summary(GhanaShrubsmixed)
anova(GhanaShrubsmixed) # Likely NS

# Check assumptions
par(mfrow=c(2,2))
plot(fitted(GhanaShrubsmixed),residuals(GhanaShrubsmixed)) 
qqnorm(resid(GhanaShrubsmixed))
qqline(resid(GhanaShrubsmixed)) 
hist(residuals(GhanaShrubsmixed), col="darkgray") # Better model 

# Post hoc treatments
library(multcomp)
summary(glht(GhanaShrubsmixed, mcp(TREATMENT="Tukey")))
#Linear Hypotheses:
#Estimate Std. Error z value Pr(>|z|)  
#LATE NEW - EARLY NEW == 0  0.07944    0.15852   0.501   0.9588  
#LATE OLD - EARLY NEW == 0 -0.32705    0.15501  -2.110   0.1499  
#UNBURNT - EARLY NEW == 0  -0.18629    0.15655  -1.190   0.6331  
#LATE OLD - LATE NEW == 0  -0.40649    0.15524  -2.619   0.0437 *
#  UNBURNT - LATE NEW == 0   -0.26573    0.15678  -1.695   0.3262  
#UNBURNT - LATE OLD == 0    0.14076    0.15323   0.919   0.7949  

#########################################################################################################################
#### END ####
#########################################################################################################################

