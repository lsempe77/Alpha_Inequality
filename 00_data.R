library (dplyr)
library (foreign)
library (car)
library (ggplot2)
library (mirt)
library(psych)
library(haven)

setwd("C:/Users/LUCAS/Desktop/PISA INEQUALITY/PISA 2018")

pisa <- read_sav("CY07_MSU_STU_QQQ.sav")
CY07_MSU_SCH_QQQ <- read_sav("CY07_MSU_SCH_QQQ.sav")

pisaf<-merge(pisa,CY07_MSU_SCH_QQQ, by=c("CNT","CNTSCHID"),all.x=T)

pisaf$ST011Q10TA = car::recode (pisaf$ST011Q10TA,"2=0")
pisaf$ST011Q11TA = car::recode (pisaf$ST011Q11TA,"2=0")
pisaf$ST011Q12TA = car::recode (pisaf$ST011Q12TA,"2=0")
###
variable.names(pisaf[18])
variable.names(pisaf[63])
variable.names(pisaf[1128])
variable.names(pisaf[1129])
colnames(pisaf)[18]<- "Sex"
colnames(pisaf)[63]<- "Language"
colnames(pisaf)[1128]<- "Area"
colnames(pisaf)[1129]<- "Tipo.escola"
####
variable.names(pisaf[1:55])
####

pisaf$ST011D17TA<-as.character(pisaf$ST011D17TA)
table(pisaf$ST011D17TA)
pisaf$ST011D17TA<-ifelse(endsWith(pisaf$ST011D17TA,"1"),1,0)
pisaf$ST011D18TA<-as.character(pisaf$ST011D18TA)
str(pisaf$ST011D18TA)
pisaf$ST011D18TA<-ifelse(endsWith(pisaf$ST011D18TA,"1"),1,0)
pisaf$ST011D19TA<-as.character(pisaf$ST011D19TA)
str(pisaf$ST011D19TA)
pisaf$ST011D19TA<-ifelse(endsWith(pisaf$ST011D19TA,"1"),1,0)
pisaf$ST011D17TA<-as.character(pisaf$ST011D17TA)
table(pisaf$ST011D17TA)
pisaf$ST011D17TA<-ifelse(endsWith(pisaf$ST011D17TA,"1"),1,0)
pisaf$ST011D18TA<-as.character(pisaf$ST011D18TA)
str(pisaf$ST011D18TA)
pisaf$ST011D18TA<-ifelse(endsWith(pisaf$ST011D18TA,"1"),1,0)
pisaf$ST011D19TA<-as.character(pisaf$ST011D19TA)
str(pisaf$ST011D19TA)
pisaf$ST011D19TA<-ifelse(endsWith(pisaf$ST011D19TA,"1"),1,0)
pisaf$ST011Q01TA = car::recode (pisaf$ST011Q01TA,"2=0")
pisaf$ST011Q02TA = car::recode (pisaf$ST011Q02TA,"2=0")
pisaf$ST011Q03TA = car::recode (pisaf$ST011Q03TA,"2=0")
pisaf$ST011Q04TA = car::recode (pisaf$ST011Q04TA,"2=0")
pisaf$ST011Q05TA = car::recode (pisaf$ST011Q05TA,"2=0")
pisaf$ST011Q06TA = car::recode (pisaf$ST011Q06TA,"2=0")
pisaf$ST011Q07TA = car::recode (pisaf$ST011Q07TA,"2=0")
pisaf$ST011Q08TA = car::recode (pisaf$ST011Q08TA,"2=0")
pisaf$ST011Q09TA = car::recode (pisaf$ST011Q09TA,"2=0")
pisaf$ST011Q10TA = car::recode (pisaf$ST011Q10TA,"2=0")
pisaf$ST011Q11TA = car::recode (pisaf$ST011Q11TA,"2=0")
pisaf$ST011Q12TA = car::recode (pisaf$ST011Q12TA,"2=0")
#
####
variable.names(pisaf[1:55])
####
pisaf<-pisaf[rowSums(is.na(pisaf[29:53])) != ncol(pisaf[29:53]), ]
pisaf[29:53] <- as.data.frame(lapply(pisaf[29:53], as.numeric))

####


###

s<-'F=1-25
CONSTRAINB = (1-13,17-25,a1),(1-13,17-25,d1),(17-25,d2),(17-25,d3),(25,d4),(25,d5)'

group <- as.character(pisaf$CNT)

hom2 = multipleGroup(data = pisaf[,29:53], model=s,group= group,
                       itemtype="gpcm",technical = list(removeEmptyRows=TRUE,NCYCLES = 2000))


scor.hom2 <- fscores (hom2,method = "WLE",full.scores = T,full.scores.SE = T)

hom2<-readRDS("hom2b.RDS")
print (hom2)
summary (hom2)
GPCM<-readRDS("GPCMb.RDS")
variable.names(GPCM[1313:1314])
colnames(GPCM)[1313]<- "Replication"
colnames(GPCM)[1314]<- "S.E.Replication"
pisaf <- cbind (pisaf,GPCM[1313:1314])
variable.names(pisaf)
##

cor.test (pisaf$HOMEPOS,pisaf$Replication,use = "pairwise.complete.obs")

table(pisaf$CNT)

ggplot (pisaf, aes(x=HOMEPOS,y=Replication)) + geom_point() +
  xlab("PISA Home possessions index") +
  ylab("Replicated Home possessions index")

homepos.d <- as.data.frame(pisaf[,c("HOMEPOS")])
gpcm.d <- as.data.frame(pisaf[,c("Replication")])

homepos.d$id <-"Pisa"
gpcm.d$id <-"Replicated"

colnames(homepos.d)[1]<- "Index"
colnames(gpcm.d)[1]<- "Index"

ind.x <- rbind(homepos.d, gpcm.d)

ggplot(ind.x, aes(Index, fill = id)) + geom_density(alpha = 0.2)

####


###

##### homepos ALFA

install.packages("labelled")
library(labelled)
pisaf<-remove_val_labels(pisaf)

str(pisaf)
unique(pisaf$CNT)
pisaf$CNT<-as.factor(pisaf$CNT)
pisaf$CNT<-droplevels(pisaf$CNT)
cnt.list<-levels(pisaf$CNT)

coef25<-coef(hom2,IRTpars=T,as.data.frame=F)
coef25 <- data.frame(matrix(unlist(coef25),
      nrow=80, byrow=T),
      stringsAsFactors=T)
coef25 <- coef25[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,
                   33,37,41,45,49,53,57,61,65)]

coef25<-cbind(cnt.list,coef25)

variable.names(pisaf)

quest.nam<-variable.names(pisaf[c(1,29:53)])

old.nam<-variable.names(coef25)

coef25<-coef25 %>% rename_at(vars(old.nam), ~ quest.nam)

variable.names(coef25)

pisaf2<-merge(pisaf,coef25,by="CNT",all.x = T)

#####

variable.names(pisaf2)
pisaf2[45:52] <- lapply(pisaf2[45:52], function(x) x/4)
pisaf2[53] <- lapply(pisaf2[53], function(x) x/6)

variable.names(pisaf2)

######

pisaf2$bf1<-pisaf2$ST011Q01TA.x* pisaf2$ST011Q01TA.y
pisaf2$bf2<-pisaf2$ST011Q02TA.x* pisaf2$ST011Q02TA.y
pisaf2$bf3<-pisaf2$ST011Q03TA.x* pisaf2$ST011Q03TA.y
pisaf2$bf4<-pisaf2$ST011Q04TA.x* pisaf2$ST011Q04TA.y
pisaf2$bf5<-pisaf2$ST011Q05TA.x* pisaf2$ST011Q05TA.y
pisaf2$bf6<-pisaf2$ST011Q06TA.x* pisaf2$ST011Q06TA.y
pisaf2$bf7<-pisaf2$ST011Q07TA.x* pisaf2$ST011Q07TA.y
pisaf2$bf8<-pisaf2$ST011Q08TA.x* pisaf2$ST011Q08TA.y
pisaf2$bf9<-pisaf2$ST011Q09TA.x* pisaf2$ST011Q09TA.y
pisaf2$bf10<-pisaf2$ST011Q10TA.x* pisaf2$ST011Q10TA.y
pisaf2$bf11<-pisaf2$ST011Q11TA.x* pisaf2$ST011Q11TA.y
pisaf2$bf12<-pisaf2$ST011Q12TA.x* pisaf2$ST011Q12TA.y
pisaf2$bf13<-pisaf2$ST011Q16NA.x* pisaf2$ST011Q16NA.y
pisaf2$bf14<-pisaf2$ST011D17TA.x* pisaf2$ST011D17TA.y
pisaf2$bf15<-pisaf2$ST011D18TA.x* pisaf2$ST011D18TA.y
pisaf2$bf16<-pisaf2$ST011D19TA.x* pisaf2$ST011D19TA.y
pisaf2$bf17<-pisaf2$ST012Q01TA.x* pisaf2$ST012Q01TA.y
pisaf2$bf18<-pisaf2$ST012Q02TA.x* pisaf2$ST012Q02TA.y
pisaf2$bf19<-pisaf2$ST012Q03TA.x* pisaf2$ST012Q03TA.y
pisaf2$bf20<-pisaf2$ST012Q05NA.x* pisaf2$ST012Q05NA.y
pisaf2$bf21<-pisaf2$ST012Q06NA.x* pisaf2$ST012Q06NA.y
pisaf2$bf22<-pisaf2$ST012Q07NA.x* pisaf2$ST012Q07NA.y
pisaf2$bf23<-pisaf2$ST012Q08NA.x* pisaf2$ST012Q08NA.y
pisaf2$bf24<-pisaf2$ST012Q09NA.x* pisaf2$ST012Q09NA.y
pisaf2$bf25<-pisaf2$ST013Q01TA.x* pisaf2$ST013Q01TA.y

variable.names(pisaf2[1001:1364])

pisaf2$Alpha.Wealth <- apply(pisaf2[,c(1340:1364)], 1, sum,na.rm=T) ####  REVISAR NUMERACION

hist(pisaf2$Alpha.Wealth)

###

variable.names(pisaf2)

GDP <- read.csv("C:/Users/LUCAS/Desktop/PISA INEQUALITY/PISA 2018/GDP2018.csv")

variable.names(GDP)
names(GDP)[3]<-"Gini.Country"

pisaf3<-merge(pisaf2,GDP, by="CNT")


####
library(psych)

pca<-principal(pisaf3[,29:53],
               nfactors = 1,
               rotate ="none",
               scores = T,
               missing = T,
               cor="poly")

scores<-pca$scores
variable.names(pisaf3)
pisaf3 <- cbind (pisaf3,scores)

####

saveRDS(pisaf3,"pisaf3.RDS")
#pisaf3<-readRDS("pisaf3.RDS")

variable.names(pisaf3[1001:1369])

colnames(pisaf3)[1369]<-"PCA.poly"

variable.names(pisaf3)

cor.val<-cor(pisaf3[,c(878,1313,1365,1369)],use = "pairwise")

stargazer::stargazer(cor.val,type = "text")



cor.test(pisaf3$Replication,pisaf3$Alpha.Wealth,
         use = "pairwise.complete.obs")
cor.test (pisaf3$Replication,pisaf3$HOMEPOS,
          use = "pairwise.complete.obs")
cor.test (pisaf3$Replication,pisaf3$PCA.poly,
          use = "pairwise.complete.obs")
cor.test (pisaf3$Alpha.Wealth,pisaf3$HOMEPOS,
          use = "pairwise.complete.obs")
cor.test (pisaf3$Alpha.Wealth,pisaf3$PCA.poly,
          use = "pairwise.complete.obs")
cor.test (pisaf3$HOMEPOS,pisaf3$PCA.poly,
          use = "pairwise.complete.obs")
library(gridExtra)

library(reshape2)

normalize <- function(x,na.rm=T) {
  return (((x) - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
}
rm(data)
variable.names(pisaf3)
data<- pisaf3[c(1,878,1313,1365,1369)]

data$HOMEPOS<-normalize(data$HOMEPOS)
data$Replication<-normalize(data$Replication)
data$Alpha.Wealth<-normalize(data$Alpha.Wealth)
data$PCA.poly<-normalize(data$PCA.poly)
variable.names(data)
head(data)
####
rm(p6)
p1<-ggplot(data, aes(x=Replication, y=Alpha.Wealth)) + geom_point()
p2<-ggplot(data, aes(x=Replication, y=HOMEPOS)) + geom_point()
p3<-ggplot(data, aes(x=Replication, y=PCA.poly)) + geom_point()
p4<-ggplot(data, aes(y=Alpha.Wealth, x=HOMEPOS)) + geom_point()
p5<-ggplot(data, aes(y=Alpha.Wealth, x=PCA.poly)) + geom_point()
p6<-ggplot(data, aes(x=HOMEPOS, y=PCA.poly)) + geom_point()

jpeg(
  filename="corplots.jpeg",
  width=8,
  height=8,
  units="in",
  res=100)

grid.arrange(p2,p5,p1,p4,p3,p6,nrow=3)

dev.off()

#####

p7<-ggplot(data, aes(x=HOMEPOS)) + geom_histogram(bins = 100)
p8<-ggplot(data, aes(x=Replication)) + geom_histogram(bins = 100)
p9<-ggplot(data, aes(x=Alpha.Wealth)) + geom_histogram(bins = 100)
p10<-ggplot(data, aes(x=PCA.poly)) + geom_histogram(bins = 100)

jpeg(
  filename="histogr.jpeg",
  width=8,
  height=8,
  units="in",
  res=100)

grid.arrange(p7,p8,p9,p10,nrow=2)

dev.off()

###



desc<-describe(data[,-1])

stargazer::stargazer(desc,type = "text",summary = F)

desc.c<-describeBy(data[,-1],group = data$CNT,mat = F)
desc.c<-do.call(rbind,desc.c)
stargazer::stargazer(desc.c,type = "text",summary = F)
write.csv(desc.c,"desc.csv")


######

pisaf3$na.count<- rowSums(!is.na((pisaf3[,c(29:53)])))

table(pisaf3$na.count)

summary(pisaf3$Alpha.Wealth)

pisaf3$Alpha.Wealth<-pisaf3$Alpha.Wealth*(pisaf3$na.count/25)

pisaf3$Alpha.Wealth[pisaf3$Alpha.Wealth==0]<-NA


####

Summary.ineq.school <- pisaf3 %>%
  group_by(CNTSCHID) %>%
  dplyr::summarise(
    Observ.Schools = n(),
    school.HOMEPOS=mean(HOMEPOS,na.rm = T),
    school.Alpha.Wealth=mean(Alpha.Wealth,na.rm=T),
    school.PCA.poly=mean(PCA.poly,na.rm=T),
    sd.HOMEPOS=sd(HOMEPOS,na.rm = T),
    sd.alfa.school=sd(Alpha.Wealth,na.rm = T),
    sd.PCA=sd(PCA.poly,na.rm = T))



var(pisaf3$Alpha.Wealth,na.rm=T) #### 18.78754
var(pisaf3$PCA.poly,na.rm=T)  #### 0.7155758
var(pisaf3$HOMEPOS,na.rm=T) #### 1.384732


Summary.ineq.school$sd.Total.HOMEPOS <- 1.384732 
Summary.ineq.school$sd.Total.PC1 <- 0.7155758   
Summary.ineq.school$sd.Total.Wealth.Alpha.w <- 18.78754   


summary (Summary.ineq.school)

variable.names(Summary.ineq.school)

Summary.ineq.school$INEQ.HOMEPOS <- Summary.ineq.school$sd.HOMEPOS/Summary.ineq.school$sd.Total.HOMEPOS

Summary.ineq.school$INEQ.ALFA <- Summary.ineq.school$sd.alfa.school/Summary.ineq.school$sd.Total.Wealth.Alpha.w

Summary.ineq.school$INEQ.PCA<-Summary.ineq.school$sd.PCA/Summary.ineq.school$sd.Total.PC1

summary (Summary.ineq.school)


sapply(Summary.ineq.school, function(x) sum(is.na(x)))

variable.names(Summary.ineq.school)

pisaf3<-merge(pisaf3,Summary.ineq.school[c(1,3:5,12:14)], by="CNTSCHID",all.x=T)


##### exclude 134 observarions with INEQ missing (only 1 observations)

which( colnames(pisaf3)=="INEQ.ALFA")
which( colnames(pisaf3)=="INEQ.PCA")

pisaf3 <- pisaf3[complete.cases(pisaf3[1375]), ] #612004 to 610616
pisaf3 <- pisaf3[complete.cases(pisaf3[878]), ] #610616 to 600316
pisaf3 <- pisaf3[complete.cases(pisaf3[1376]), ] #600316 to 593257

#######
range(pisaf3$HOMEPOS,na.rm = T)
range(pisaf3$Alpha.Wealth,na.rm = T)
range(pisaf3$PCA.poly,na.rm = T)

pisaf3$HOMEPOS<-pisaf3$HOMEPOS+10.2033
pisaf3$PCA.poly<-pisaf3$PCA.poly+3.422748

######
library(ineq)
summary(pisaf3$HOMEPOS)
str(pisaf3$HOMEPOS)

library(labelled)
pisaf3<-remove_val_labels(pisaf3)

Ineq.Atkinson.05<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
                     ineq::Atkinson,parameter = 0.5,simplify = T)

Ineq.Atkinson.1<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
                    ineq::Atkinson,parameter = 1,simplify = T)

Ineq.Atkinson.2<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
                    ineq::Atkinson,parameter = 2,simplify = T)

Ineq.Gini<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
              ineq::Gini,simplify = T)

Ineq.Theil.m1<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
                  ineq::Theil,parameter = -1,simplify = T)

Ineq.Theil.0<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
                 ineq::Theil,parameter = 0,simplify = T)

Ineq.Theil.1<-by(pisaf3$HOMEPOS,as.factor(pisaf3$CNTSCHID),
                 ineq::Theil,parameter = 1,simplify = T)

#### ineq based on Alpha.Wealth


Ineq.Atkinson.05.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                       ineq::Atkinson,parameter = 0.5,simplify = T)

Ineq.Atkinson.1.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                      ineq::Atkinson,parameter = 1,simplify = T)

Ineq.Atkinson.2.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                      ineq::Atkinson,parameter = 2,simplify = T)

Ineq.Gini.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                ineq::Gini,simplify = T)

Ineq.Theil.m1.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                    ineq::Theil,parameter = -1,simplify = T)

Ineq.Theil.0.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                   ineq::Theil,parameter = 0,simplify = T)

Ineq.Theil.1.s<-by(pisaf3$Alpha.Wealth,as.factor(pisaf3$CNTSCHID),
                   ineq::Theil,parameter = 1,simplify = T)

Ineq.Atkinson.05.s<-as.data.frame(unlist(as.list(Ineq.Atkinson.05.s)))
Ineq.Atkinson.1.s<-as.data.frame(unlist(as.list(Ineq.Atkinson.1.s)))
Ineq.Atkinson.2.s<-as.data.frame(unlist(as.list(Ineq.Atkinson.2.s)))

Ineq.Gini.s<-as.data.frame(unlist(as.list(Ineq.Gini.s)))

Ineq.Theil.m1.s<-as.data.frame(unlist(as.list(Ineq.Theil.m1.s)))
Ineq.Theil.0.s<-as.data.frame(unlist(as.list(Ineq.Theil.0.s)))
Ineq.Theil.1.s<-as.data.frame(unlist(as.list(Ineq.Theil.1.s)))



####

Ineq.Atkinson.05<-as.data.frame(unlist(as.list(Ineq.Atkinson.05)))
Ineq.Atkinson.1<-as.data.frame(unlist(as.list(Ineq.Atkinson.1)))
Ineq.Atkinson.2<-as.data.frame(unlist(as.list(Ineq.Atkinson.2)))

Ineq.Gini<-as.data.frame(unlist(as.list(Ineq.Gini)))

Ineq.Theil.m1<-as.data.frame(unlist(as.list(Ineq.Theil.m1)))
Ineq.Theil.0<-as.data.frame(unlist(as.list(Ineq.Theil.0)))
Ineq.Theil.1<-as.data.frame(unlist(as.list(Ineq.Theil.1)))

#### Ineq.PCA


Ineq.Atkinson.05.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                         ineq::Atkinson,parameter = 0.5,simplify = T)

Ineq.Atkinson.1.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                        ineq::Atkinson,parameter = 1,simplify = T)

Ineq.Atkinson.2.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                        ineq::Atkinson,parameter = 2,simplify = T)

Ineq.Gini.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                  ineq::Gini,simplify = T)

Ineq.Theil.m1.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                      ineq::Theil,parameter = -1,simplify = T)

Ineq.Theil.0.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                     ineq::Theil,parameter = 0,simplify = T)

Ineq.Theil.1.pca<-by(pisaf3$PCA.poly,as.factor(pisaf3$CNTSCHID),
                     ineq::Theil,parameter = 1,simplify = T)

Ineq.Atkinson.05.pca<-as.data.frame(unlist(as.list(Ineq.Atkinson.05.pca)))
Ineq.Atkinson.1.pca<-as.data.frame(unlist(as.list(Ineq.Atkinson.1.pca)))
Ineq.Atkinson.2.pca<-as.data.frame(unlist(as.list(Ineq.Atkinson.2.pca)))

Ineq.Gini.pca<-as.data.frame(unlist(as.list(Ineq.Gini.pca)))

Ineq.Theil.m1.pca<-as.data.frame(unlist(as.list(Ineq.Theil.m1.pca)))
Ineq.Theil.0.pca<-as.data.frame(unlist(as.list(Ineq.Theil.0.pca)))
Ineq.Theil.1.pca<-as.data.frame(unlist(as.list(Ineq.Theil.1.pca)))

variable.names(Summary.ineq.school)

sum(is.na(Summary.ineq.school$sd.HOMEPOS))

sapply(Summary.ineq.school, function(x) sum(is.na(x)))

Summary.ineq.school <- Summary.ineq.school[complete.cases(Summary.ineq.school[6]), ]

sapply(Summary.ineq.school, function(x) sum(is.na(x)))

Summary.ineq.school2<-na.omit(Summary.ineq.school, cols = "INEQ.PCA")

Ineq.tod<-cbind(Ineq.Atkinson.05,
                Ineq.Atkinson.1,
                Ineq.Atkinson.2,
                Ineq.Gini,
                Ineq.Theil.m1,
                Ineq.Theil.0,
                Ineq.Theil.1,
                Ineq.Atkinson.05.s,
                Ineq.Atkinson.1.s,
                Ineq.Atkinson.2.s,
                Ineq.Gini.s,
                Ineq.Theil.m1.s,
                Ineq.Theil.0.s,
                Ineq.Theil.1.s,
                Ineq.Atkinson.05.pca,
                Ineq.Atkinson.1.pca,
                Ineq.Atkinson.2.pca,
                Ineq.Gini.pca,
                Ineq.Theil.m1.pca,
                Ineq.Theil.0.pca,
                Ineq.Theil.1.pca,
                Summary.ineq.school2[,c(12:14,1)])


colnames(Ineq.tod)[1]<- "Atkinson.05"
colnames(Ineq.tod)[2]<- "Atkinson.1"
colnames(Ineq.tod)[3]<- "Atkinson.2"
colnames(Ineq.tod)[4]<- "Gini"
colnames(Ineq.tod)[5]<- "Theil.m1"
colnames(Ineq.tod)[6]<- "Theil.0"
colnames(Ineq.tod)[7]<- "Theil.1"
colnames(Ineq.tod)[8]<- "Atkinson.05.s"
colnames(Ineq.tod)[9]<- "Atkinson.1.s"
colnames(Ineq.tod)[10]<- "Atkinson.2.s"
colnames(Ineq.tod)[11]<- "Gini.s"
colnames(Ineq.tod)[12]<- "Theil.m1.s"
colnames(Ineq.tod)[13]<- "Theil.0.s"
colnames(Ineq.tod)[14]<- "Theil.1.s"
colnames(Ineq.tod)[15]<- "Atkinson.05.pca"
colnames(Ineq.tod)[16]<- "Atkinson.1.pca"
colnames(Ineq.tod)[17]<- "Atkinson.2.pca"
colnames(Ineq.tod)[18]<- "Gini.pca"
colnames(Ineq.tod)[19]<- "Theil.m1.pca"
colnames(Ineq.tod)[20]<- "Theil.0.pca"
colnames(Ineq.tod)[21]<- "Theil.1.pca"
sapply(Ineq.tod, function(x) sum(is.na(x)))

#


summary(Ineq.tod$Theil.m1.pca)




#
variable.names(Ineq.tod)

variable.names(pisaf3[1001:1376])

pisa.c.s<-merge(pisaf3,Ineq.tod[c(1:21,25)], by="CNTSCHID",all.x=T)


####

pisa.c.s$PV1READ[pisa.c.s$PV1READ==0]<-NA
pisa.c.s$PV2READ[pisa.c.s$PV2READ==0]<-NA
pisa.c.s$PV3READ[pisa.c.s$PV3READ==0]<-NA
pisa.c.s$PV4READ[pisa.c.s$PV4READ==0]<-NA
pisa.c.s$PV5READ[pisa.c.s$PV5READ==0]<-NA
pisa.c.s$PV6READ[pisa.c.s$PV6READ==0]<-NA
pisa.c.s$PV7READ[pisa.c.s$PV7READ==0]<-NA
pisa.c.s$PV8READ[pisa.c.s$PV8READ==0]<-NA
pisa.c.s$PV9READ[pisa.c.s$PV9READ==0]<-NA
pisa.c.s$PV10READ[pisa.c.s$PV10READ==0]<-NA

pisa.c.s$PV1MATH[pisa.c.s$PV1MATH==0]<-NA
pisa.c.s$PV2MATH[pisa.c.s$PV2MATH==0]<-NA
pisa.c.s$PV3MATH[pisa.c.s$PV3MATH==0]<-NA
pisa.c.s$PV4MATH[pisa.c.s$PV4MATH==0]<-NA
pisa.c.s$PV5MATH[pisa.c.s$PV5MATH==0]<-NA
pisa.c.s$PV6MATH[pisa.c.s$PV6MATH==0]<-NA
pisa.c.s$PV7MATH[pisa.c.s$PV7MATH==0]<-NA
pisa.c.s$PV8MATH[pisa.c.s$PV8MATH==0]<-NA
pisa.c.s$PV9MATH[pisa.c.s$PV9MATH==0]<-NA
pisa.c.s$PV10MATH[pisa.c.s$PV10MATH==0]<-NA

summary(pisa.c.s[c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ",
                   "PV6READ","PV7READ","PV8READ","PV9READ","PV10READ",
                   "Sex","HISCED","IMMIG","Language","AGE","REPEAT","Tipo.escola","Area","GNI","Gini.Country","HOMEPOS","school.HOMEPOS","INEQ.HOMEPOS")])


variable.names(pisa.c.s[1:1000])
variable.names(pisa.c.s[1001:1397])

#CHECK to take out area, HISCED, IMIG

pisa.c.s[,c(18,63,835,862,1129,1313,1365:1367,
            1369:1397)]<-scale(pisa.c.s[,c(18,63,835,862,1129,1313,1365:1367,1369:1397)],center=T,scale=T)

####

gc()

pisa.c.s$Sex<-as.factor(pisa.c.s$Sex)
table(pisa.c.s$Sex)

pisa.c.s$Sex <- factor(pisa.c.s$Sex,
                       levels = c(-1.0013226785893,0.998677404989455),
                       labels = c("Female", "Male"))

pisa.c.s$Language<-as.factor(pisa.c.s$Language)

table(pisa.c.s$Language)

pisa.c.s$Language <- factor(pisa.c.s$Language,
                            levels = c(-0.457701634447052 , 2.18482576292132),
                            labels = c("Language of test", "Other language"))

pisa.c.s$REPEAT<-as.factor(pisa.c.s$REPEAT)
table(pisa.c.s$REPEAT)
pisa.c.s$REPEAT <- factor(pisa.c.s$REPEAT,
                          levels = c(-0.366066605730879,2.73173855255246),
                          labels = c("No", "Yes"))

pisa.c.s$Tipo.escola <- as.factor(pisa.c.s$Tipo.escola)
table(pisa.c.s$Tipo.escola)
pisa.c.s$Tipo.escola <- factor(pisa.c.s$Tipo.escola,
                               levels = c(-0.493925858312194,2.02459162447923),
                               labels = c("Public", "Private"))
sum(is.na(pisa.c.s$IMMIG))


pisa.c.s$IMMIG <- as.factor(pisa.c.s$IMMIG)
table(pisa.c.s$IMMIG)
pisa.c.s$IMMIG <- factor(pisa.c.s$IMMIG,
                         levels = c(1,
                                    2,
                                    3),
                         labels = c("Native", 
                                    "Second.generation",
                                    "First.generation"))


pisa.c.s$Area <- as.factor(pisa.c.s$Area)
table(pisa.c.s$Area)
pisa.c.s$Area <- factor(pisa.c.s$Area,
                              levels = c(1,
                                         2,
                                         3,
                                         4,
                                         5),
                              labels = c("<3k", "3k>15k",
                                         "15k>100k","100k>1m",">1m"))

which( colnames(pisa.c.s)=="HISCED")

pisa.c.s$HISCED <- as.factor(pisa.c.s$HISCED)
table(pisa.c.s$HISCED)
pisa.c.s$HISCED <- factor(pisa.c.s$HISCED,
                          levels = c(0,
                                     1,
                                     2,
                                     3,
                                     4,
                                     5,
                                     6),
                          labels = c("None", "ISCED 1","ISCED 2",
                                     "ISCED 3B-C","ISCED 3A-4",
                                     "ISCED 5B","ISCED 5A-6"))

#subset countries without school level identification (only 1 school code for whole dataset)
tt<-table(pisa.c.s$CNTSCHID,pisa.c.s$CNT)

write.csv(tt,"cntsch.csv")


#### weights

###The percentage of school variance explained by explicit stratification variables has been computed from a multilevel model where Level 2 (i.e. school) weights correspond to the sum of final student weights (W_FSTUWT) within each stratum. 

oldweight<- pisa %>% 
  group_by(CNT) %>% 
  dplyr::summarise(Frequency = sum(W_FSTUWT),
                   Observation.old = n())

newweight<- pisa.c.s %>% 
  group_by(CNT) %>% 
  dplyr::summarise(Frequency = sum(W_FSTUWT),
                   Observation.new = n())

weights<-merge(oldweight,newweight,by="CNT")

weights$reweight<-weights$Frequency.x/weights$Frequency.y

pisa.c.s<-merge(pisa.c.s,weights[c(1,6)], by="CNT",all.x = T)

pisa.c.s$Student.Weight<-pisa.c.s$W_FSTUWT*pisa.c.s$reweight

cor(pisa.c.s$Student.Weight,pisa.c.s$W_FSTUWT)


saveRDS(pisa.c.s,"pisa.c.s.RDS")
