################################################################
# Ecosystem C storage
################################################################
# Import data - above C storage data for Ghana
GhanaCabove<-read.csv("./Ghana.aboveground.csv", sep=",",header=TRUE)
names(GhanaCabove)
# Import data -Belowground C storage
GhanaCbelow<-read.csv("./Ghana.belowground.csv", sep=",",header=TRUE)
names(GhanaCbelow)
head(GhanaCbelow)

# Summaries above and below C stocks
aboveC<-aggregate(GhanaCabove$C.stock.kg.m2, by=list(Treatment=GhanaCabove$Treatment,Location=GhanaCbelow$Location),na.rm=T,sum)
aboveC
belowC<-aggregate(GhanaCbelow$SOC, by=list(Treatment=GhanaCbelow$Treatment,Location=GhanaCbelow$Location),na.rm=T,sum)
belowC
GhanaEcosystemC<-merge(aboveC,belowC, by=c("Location","Treatment"))
dim(GhanaEcosystemC)
GhanaEcosystemC$EcosystemC<-GhanaEcosystemC$x.x+GhanaEcosystemC$x.y
colnames(GhanaEcosystemC)<-c("Location","Treatment","AboveC","BelowC","EcosystemC")
dim(GhanaEcosystemC) #28  
GhanaEcosystemC

# Mean for Aboveground C, Belowground C and Ecosystem C
names(GhanaEcosystemC)
BelowCx<-aggregate(GhanaEcosystemC$BelowC, by=list(Treatment=GhanaEcosystemC$Treatment),mean)
AboveCx<-aggregate(GhanaEcosystemC$AboveC, by=list(Treatment=GhanaEcosystemC$Treatment),mean)
EcosystemCx<-aggregate(GhanaEcosystemC$EcosystemC, by=list(Treatment=GhanaEcosystemC$Treatment),mean)
BelowCx
AboveCx
EcosystemCx

# Standard errors for Aboveground C, Belowground C and Ecosystem C
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
names(GhanaEcosystemC)
BelowCse<-aggregate(GhanaEcosystemC$BelowC, by=list(Treatment=GhanaEcosystemC$Treatment),se)
AboveCse<-aggregate(GhanaEcosystemC$AboveC, by=list(Treatment=GhanaEcosystemC$Treatment),se)
EcosystemCse<-aggregate(GhanaEcosystemC$EcosystemC, by=list(Treatment=GhanaEcosystemC$Treatment),se)
BelowCse
AboveCse
EcosystemCse

#Combine all in a dataframe
EcosystemC.summary<-cbind(BelowCx,AboveCx[,2],EcosystemCx[,2],BelowCse[,2],AboveCse[,2],EcosystemCse[,2])
colnames(EcosystemC.summary)<-c("Treatment","BelowC","AboveC","EcosystemC","BelowC se", "AboveC se","EcosystemC se")
EcosystemC.summary
