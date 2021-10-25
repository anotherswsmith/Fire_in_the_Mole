#Ghana: savannah fire history - C storage
#Stuart Smith
#17/6/2017 
#########################################################################################################################
#clear system & package libraries
rm(list=ls())
library(lattice)
library(MASS)
################################################################

################################################################
# Aboveground C storage
################################################################

# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.aboveground.copy.csv", sep=",",header=TRUE)

dim(GhanaCabove) # 140 rows # 4 columns
str(GhanaCabove)
head(GhanaCabove) # NAs
names(GhanaCabove)

# SE - NAS 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# Explore data - Trt x soil depth - boxplot
bwplot(C.stock.kg.m2~Treatment|Pool, data=GhanaCabove) 
# Trees huge in unburnt - not so much in tree pool
#Unburnt deadwood and litter higher
# Shrub Recent early higher, but 

# Seperate graphs for smaller pools deadwood, Herb Veg and litter
par(mfrow=c(1,3))
# Deadwood
GhanaCaboveDeadwood<-GhanaCabove[GhanaCabove$Pool=="deadwood",]
boxplot(C.stock.kg.m2~Treatment, data=GhanaCaboveDeadwood, main="Deadwood")
#Herb.veg
GhanaCaboveHerb.veg<-GhanaCabove[GhanaCabove$Pool=="Herb.veg",]
boxplot(C.stock.kg.m2~Treatment, data=GhanaCaboveHerb.veg, main="Herb veg")
#litter
GhanaCabovelitter<-GhanaCabove[GhanaCabove$Pool=="litter",]
boxplot(C.stock.kg.m2~Treatment, data=GhanaCabovelitter, main="Litter")

# Deadwood has all the zeroes that may influence the analysis 
aggregate(GhanaCabove$C.stock.kg.m2, by=list(Pool=GhanaCabove$Pool),FUN=function(x) sum(x== 0))
#Pool  C.stock.kg.m2 = 0
#1 deadwood 17
#2 Herb.veg  0
#3   litter  0
#4    Shrub  0
#5     Tree  0
aggregate(GhanaCabove$C.stock.kg.m2, by=list(Treatment= GhanaCabove$Treatment, Pool=GhanaCabove$Pool),FUN=function(x) sum(x== 0))
# Limited bias towards a particular fire history
#Treatment     Pool x
#1      Old late deadwood 3
#2  Recent early deadwood 4
#3   Recent late deadwood 5
#4       Unburnt deadwood 5

# Correlation between shrub C and tree C 
GhanaCaboveshrub<-GhanaCabove[GhanaCabove$Pool=="Shrub",]
GhanaCabovetree<-GhanaCabove[GhanaCabove$Pool=="Tree",]
par(mfrow=c(1,1))
plot(GhanaCaboveshrub$C.stock.kg.m2~GhanaCabovetree$C.stock.kg.m2, pch =21, bg =GhanaCaboveshrub$Treatment)
# No relationship between Tree and shrub C

# Mixed effect model # Fixed factors TRT nested within Depth and random factor is site
library(lme4)

GhanaCmixedAbove<-lmer(C.stock.kg.m2~Treatment/Pool+(1|Location),na.action=na.omit, data = GhanaCabove, REML=T)
summary(GhanaCmixedAbove)
vcov(GhanaCmixedAbove) # Treatment nested within Pool
anova(GhanaCmixedAbove) # Treatment differences within pool - less between treatments
str(GhanaCmixedAbove)

# Check assumptions
plot(fitted(GhanaCmixedAbove),residuals(GhanaCmixedAbove)) # conical = needs transformation
qqnorm(resid(GhanaCmixedAbove))
qqline(resid(GhanaCmixedAbove)) # Poor fit
hist(residuals(GhanaCmixedAbove), col="darkgray") # Narrow histogram

#Log transform the aboveground stocks
GhanaCmixedAbove<-lmer(log(C.stock.kg.m2+1)~Treatment/Pool+(1|Location),na.action=na.omit, data = GhanaCabove, REML=T)
summary(GhanaCmixedAbove)
vcov(GhanaCmixedAbove) # Treatment nested within depth 	
anova(GhanaCmixedAbove)
str(GhanaCmixedAbove)

# Check assumptions
plot(fitted(GhanaCmixedAbove),residuals(GhanaCmixedAbove)) # poor fit - still conical - due to zeroes
qqnorm(resid(GhanaCmixedAbove))
qqline(resid(GhanaCmixedAbove)) # Good fit
hist(residuals(GhanaCmixedAbove), col="darkgray") # Narrow historgram - long tails

# Remove rows with zeroes - deadwood

GhanaCaboveNoZero<-GhanaCabove[! GhanaCabove$C.stock.kg.m2==0, ] # Remove blanks without spp
dim(GhanaCaboveNoZero) # 123   4
aggregate(GhanaCaboveNoZero$C.stock.kg.m2, by=list(Pool=GhanaCaboveNoZero$Pool),FUN=function(x) sum(x== 0))
#No zeroes

#NO ZEROES in Deadwood and log transform the aboveground stocks
GhanaCmixedAbove<-lmer(log(C.stock.kg.m2)~Treatment/Pool+(1|Location),na.action=na.omit, data = GhanaCaboveNoZero, REML=T)
summary(GhanaCmixedAbove)
vcov(GhanaCmixedAbove) # Treatment nested within depth 	
anova(GhanaCmixedAbove)
str(GhanaCmixedAbove)

# Check assumptions
plot(fitted(GhanaCmixedAbove),residuals(GhanaCmixedAbove)) # good spread
qqnorm(resid(GhanaCmixedAbove))
qqline(resid(GhanaCmixedAbove)) # Good fit
hist(residuals(GhanaCmixedAbove), col="darkgray") # Good sread to histogram

GhanaCabove
# Constrast full model without factors
GhanaCmixedAbove2<-lmer((C.stock.kg.m2)^.2~Treatment/Pool+(1|Location), na.action=na.omit, data = GhanaCabove, REML=F)
GhanaCmixedAboveTRT<-lmer((C.stock.kg.m2)^.2~Pool+(1|Location),na.action=na.omit, data = GhanaCabove, REML=F)
GhanaCmixedAbovePool<-lmer((C.stock.kg.m2)^.2~Treatment+(1|Location),na.action=na.omit, data = GhanaCabove, REML=F)

anova(GhanaCmixedAbove2,GhanaCmixedAboveTRT) # Removal of TRT significant
anova(GhanaCmixedAbove2,GhanaCmixedAbovePool) # Removal of Pool:Trt significant

#       Df     AIC     BIC  logLik deviance Chisq Chi Df Pr(>Chisq)  
#..1     7 -67.519 -46.927 40.759  -81.519                           
#object 22 -60.694   4.022 52.347 -104.694 23.176     15    0.08047 .# Removal of TRT significant
#object 22 -60.694   4.022  52.347  -104.69 211.1     16  < 2.2e-16 *** # Removal of Pool:Trt significant


#Pairwise contrasts accounting for df and missing values use *lsmeans*
library(multcompView)
library(lsmeans)
library(lmerTest)
library(Hmisc)
library(pbkrtest)


GhanaCmixedAbove<-lmer((C.stock.kg.m2)^.2~Treatment/Pool+(1|Location),na.action=na.omit, data = GhanaCabove, REML=T)
anova(GhanaCmixedAbove)
leastsquare <-lsmeans(GhanaCmixedAbove, pairwise~Treatment|Pool, adjust="Tukey") # Correct number of rows - but all factors signifcant - not true

leastsquareLT <-lsmeansLT(GhanaCmixedAbove, 
                          test.effs= "Treatment:Pool" ) # Correct number of rows - but all factors signifcant - not true
leastsquareLT
leastsquare.inter <-difflsmeans(GhanaCmixedAbove, 
                                test.effs= "Treatment:Pool" ) # Correct number of rows - but all factors signifcant - not true
leastsquare.inter 


################################################################
# Belowground C storage
################################################################

# Import data - belowground C storage data for Ghana
GhanaC<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.belowground.csv", sep=",",header=TRUE)

dim(GhanaC) # 112 rows # 2 columns
str(GhanaC)
head(GhanaC) # NAs
names(GhanaC)

# SE - NAS 
SE<- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# Explore data - Trt x soil depth - boxplot
bwplot(C.density ~Treatment|Horizon, data=GhanaC,
       strip=strip.custom(var.name="",
                          factor.levels=c("0-2 cm","2-7 cm", "7-12cm","12-17cm"),
                          strip.levels=rep(TRUE,4))) 
# Reduction in soil C 2 - 17 c, with late new
# Lack of evidence of depth * TRT interaction apart from 0 - 2 cm = all trts 
# Late old deep 12-17 cm outlier
# Mixed effect model # Fixed factors TRT nested within Depth and random factor is site
library(lme4)
names(GhanaC)

GhanaC$Horizon<-as.factor(GhanaC$Horizon)
is.factor(GhanaC$Horizon)

GhanaCmixed<-lmer(sqrt(C.density)~Treatment/Horizon+(1|Location),na.action=na.omit, data = GhanaC, REML=T)
summary(GhanaCmixed)
vcov(GhanaCmixed) # Treatment nested within depth 	
anova(GhanaCmixed) # Treatment significant
str(GhanaCmixed)

# Check assumptions
par(mfrow=c(2,2))
plot(fitted(GhanaCmixed),residuals(GhanaCmixed)) # residuals conical - larger outlier
qqnorm(resid(GhanaCmixed))
qqline(resid(GhanaCmixed)) # Good fit - again one very low outlier...
hist(residuals(GhanaCmixed), col="darkgray") # One low outlier

# Contrast full model without factors - test significance of depth and TRT to main model
GhanaCmixed<-lmer(C.density~Treatment/Horizon+(1|Location),na.action=na.omit, data = GhanaC, REML=F)
GhanaCmixedTRT<-lmer(C.density~Horizon+(1|Location),na.action=na.omit, data = GhanaC, REML=F)
GhanaCmixedDepth<-lmer(C.density~Treatment+(1|Location),na.action=na.omit, data = GhanaC, REML=F)

anova(GhanaCmixed,GhanaCmixedTRT) # Removal of TRT NS
anova(GhanaCmixed,GhanaCmixedDepth) # Removal of Depth NS

# Treatment difference looks significant - NS result driven by outlier?!

# Repeat analysis without outlier
dotchart(GhanaC$Bel.carbon) 
plot(GhanaC$Bel.carbon)
#identify(GhanaC$Bel.carbon) # Entry 105 # 0.04656969
GhanaC2<-GhanaC[-105,] # Remove entry
GhanaC2$C.density # Outlier removed

# Mixed effect model # Fixed factors TRT nested within Depth and random factor is site
library(lme4)

GhanaCmixed2<-lmer(C.density~Treatment/Horizon+(1|Location),na.action=na.omit, data = GhanaC2, REML=T)
summary(GhanaCmixed2)
vcov(GhanaCmixed2) # Treatment nested within depth 	
anova(GhanaCmixed2)
str(GhanaCmixed2)

# Check assumptions
par(mfrow=c(2,2))
plot(fitted(GhanaCmixed2),residuals(GhanaCmixed2)) 
abline(0,0, col="red")# Model improved - but good spread around zero without outlier
qqnorm(resid(GhanaCmixed2))
qqline(resid(GhanaCmixed2)) # Good fit - outlier at bottom left removed
hist(residuals(GhanaCmixed2), col="darkgray") # Good spread - no outlier

# Contrast full model without factors - test significance of depth and TRT to main model
GhanaCmixed<-lmer(C.density~Treatment/Horizon+(1|Location),na.action=na.omit, data = GhanaC2, REML=F)
GhanaCmixedTRT<-lmer(C.density~Horizon+(1|Location),na.action=na.omit, data = GhanaC2, REML=F)
GhanaCmixedDepth<-lmer(C.density~Treatment+(1|Location),na.action=na.omit, data = GhanaC2, REML=F)

anova(GhanaCmixed,GhanaCmixedTRT) # Removal of TRT marginally significant - when outlier removed - if horizon numeric significant
anova(GhanaCmixed,GhanaCmixedDepth) # Removal of Depth NS

# Contrast interaction
library(multcomp)
GhanaC$TD<-interaction(GhanaC$Treatment,GhanaC$Horizon)
as.factor(GhanaC$Horizon)

GhanaCmixed<-lmer(C.density~Treatment/Horizon+(1|Location),na.action=na.omit, data = GhanaC2, REML=T)
summary(GhanaCmixed)	
anova(GhanaCmixed)
glme41 <- glht(GhanaCmixed, linfct=mcp("Treatment:Horizon" = "Tukey"))
summary(glme41) #NS

#Construct a contrast matrix
levels(GhanaC$TRT)
#Contrast matrix for treatment
K <- rbind("EARLYNEW - LATENEW" = c( 1, -1,  0, 0),     
           "EARLYNEW - LATEOLD" = c(-1,  0,  1, 0),       
           "EARLYNEW - UNBURNT" = c( 1,  0, 0  -1),     
           "LATENEW - LATEOLD" = c( 0, 1,  -1, 0),
           "LATENEW - UNBURNT" = c( 0, 1,  -1, 0),
           "LATEOLD - UNBURNT" = c( 0, 0,  -1, 1)           )
K
glme41 <- glht(GhanaCmixed, linfct=mcp(TRT = K))
summary(glme41) 

#Pairwise contrasts accounting for df and missing values use *lsmeans*
library(multcompView)
library(lsmeans)
library(lmerTest)
library(Hmisc)
library(pbkrtest)

GhanaCmixedAbove<-lmer(log(C.stock.kg.m2+1)~Treatment/Pool+(1|Location),na.action=na.omit, data = GhanaCabove, REML=T)
anova(GhanaCmixedAbove)

GhanaC2$Horizon<-as.factor(GhanaC2$Horizon)
is.factor(GhanaC2$Horizon)
GhanaCmixedbelow<-lmer(C.density~Treatment/Horizon+(1|Location),na.action=na.omit, data = GhanaC, REML=T)
anova(GhanaCmixedbelow)

leastsquare <-lsmeans(GhanaCmixedbelow, pairwise~Treatment|Horizon, adjust="tukey" ) # Correct number of rows - but all factors signifcant - not true


leastsquareLT <-lsmeansLT(GhanaCmixedbelow, 
				test.effs= "Treatment:Horizon" ) 
leastsquareLT 

# Correct number of rows - but all factors signifcant - not true

leastsquareLT
leastsquare.inter <-difflsmeans(GhanaCmixedbelow, 
                                test.effs= "Treatment:Horizon" ) # Correct number of rows - but all factors signifcant - not true
leastsquare.inter 


################################################################
# Ecosystem C storage
################################################################
# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.aboveground.csv", sep=",",header=TRUE)
names(GhanaCabove)
# Import data -Belowground C storage
GhanaCbelow<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Ghana.belowground.csv", sep=",",header=TRUE)
names(GhanaCbelow)
head(GhanaCbelow)

#Remove columns from GhanaCabove and GhanaCbelow that do not match: pool, horizon, depth and SOC 
GhanaCabove2<-GhanaCabove[,-c(3)] # Removes pool
head(GhanaCabove2)
GhanaCbelow2<-GhanaCbelow[,-c(3:5)] # Removes horizon, depth and SOC
head(GhanaCbelow2)
colnames(GhanaCbelow2)<-c("Treatment","Location","C.stock.kg.m2")

#Combine above and belowground datasets
GhanaEcosystemC<-rbind(GhanaCabove2,GhanaCbelow2)

# Summaries Ecosystem C
aggregate(GhanaEcosystemC$C.stock.kg.m2, by=list(Treatment=GhanaEcosystemC$Treatment,Location=GhanaEcosystemC$Location),na.rm=T,sum)
GhanaEcosystemCTot<-aggregate(GhanaEcosystemC$C.stock.kg.m2, by=list(Treatment=GhanaEcosystemC$Treatment,Location=GhanaEcosystemC$Location),na.rm=T,sum)
colnames(GhanaEcosystemCTot)<-c("Treatment","Location","C.stock.kg.m2")
dim(GhanaEcosystemCTot) #28  3 - only 28 datapoints - sufficient for mixed model? 


# Mixed effect model # Ecosystem C
library(lme4)
names(GhanaEcosystemCTot)
GhanaCmixedEco<-lmer(C.stock.kg.m2~Treatment+(1|Location),na.action=na.omit, data = GhanaEcosystemCTot, REML=T)
#Error: number of levels of each grouping factor must be < number of observations
# Use lm 

GhanaClmEco<-lm(C.stock.kg.m2~Treatment,na.action=na.omit, data = GhanaEcosystemCTot)
summary(GhanaClmEco)
anova(GhanaClmEco) # Treatment marginal for Ecosystem C 

#Diagnostic of lm 
library(car)
par(mfrow=c(2,4))
plot(GhanaClmEco) # Resid vs fit almost conical - unburnt has sliglty higher resid variance 
plot(GhanaClmEco, which=4) # Cooks distance outliers
qqPlot(GhanaClmEco,main="QQ Plot + CI")
sresid <- studres(GhanaClmEco) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
spreadLevelPlot(GhanaClmEco) # Evaluate homoscedasticity
ncvTest(GhanaClmEco) # Issues with spread of residuals

# Transform C stocks - log better fit of assumptions
GhanaClmEco<-lm(log(C.stock.kg.m2)~Treatment,na.action=na.omit, data = GhanaEcosystemCTot)
summary(GhanaClmEco)
anova(GhanaClmEco) # Treatment marginal for Ecosystem C 

#Diagnostic of lm 
library(car)
par(mfrow=c(2,4))
plot(GhanaClmEco) # Model ok - unburnt has sliglty higher resid variance 
plot(GhanaClmEco, which=4) # Cooks distance outliers
qqPlot(GhanaClmEco,main="QQ Plot + CI")
sresid <- studres(GhanaClmEco) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
spreadLevelPlot(GhanaClmEco) # Evaluate homoscedasticity
ncvTest(GhanaClmEco) # NS - no issues

# Transform C stocks - log better fit of assumptions
GhanaClmEco<-lm(log(C.stock.kg.m2)~Treatment,na.action=na.omit, data = GhanaEcosystemCTot)
summary(GhanaClmEco)
anova(GhanaClmEco)
str(GhanaClmEco)

# Ecosystem C - no significant difference
#Df Sum Sq Mean Sq F value  Pr(>F)  
#Residuals 24 3.7459 0.15608 
#Treatment  3 1.2034 0.40113    2.57 0.07787 .

# Post hoc treatments
library(multcomp)
summary(glht(GhanaClmEco, mcp(Treatment="Tukey")))
#Linear Hypotheses:
#Estimate Std. Error t value Pr(>|t|)  
#Recent early - Old late == 0      0.1395     0.2112   0.661   0.9107  
#Recent late - Old late == 0      -0.2207     0.2112  -1.045   0.7250  
#Unburnt - Old late == 0           0.3487     0.2112   1.651   0.3704  
#Recent late - Recent early == 0  -0.3603     0.2112  -1.706   0.3425  
#Unburnt - Recent early == 0       0.2092     0.2112   0.991   0.7561  
#Unburnt - Recent late == 0        0.5695     0.2112   2.697   0.0568 .

################################################################
# Tree and shrub densities and allometerics in rela
################################################################
# Import data - Tree shrub data
GhanaTreeShrub<-read.csv("/Users/anotherswsmith/Documents/AfricanBioServices/Master projects/Joana Awuah Adofo/Trees.shrubs.allomet.csv", sep=",",header=TRUE)

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
TreeShrubCount<-count(GhanaTreeShrub, c("Type","TREATMENT","PLOT","Species"))
TreeShrubCount
dim(TreeShrubCount)  # 56 data points

# Biplot of count data for shrubs and trees
bwplot(freq~TREATMENT|Type, data=TreeShrubCount) 

# Summary of tree densities 
TreeShrubdense<-aggregate((freq/625)~TREATMENT+Type, data=TreeShrubCount, mean)

# Mixed effect model # account for plot
library(lme4)
# Convert to density = divide by 25*25=625

# Analyse count data = use poisson?
names(TreeShrubCount)
GhanaCountGLM<-glmer(freq~Type/TREATMENT+(1|PLOT),na.action=na.omit, family =poisson(link="log") ,data =TreeShrubCount)
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
GhanaCountGLM<-glmer(freq~TREATMENT/Type+(1|PLOT)+(1|Species),na.action=na.omit, family =poisson(link="log") ,data =TreeShrubCount)
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


