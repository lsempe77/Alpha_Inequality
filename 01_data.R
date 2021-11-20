memory.limit(size = 32000)


library(gridExtra)
library(intsvy)
library (dplyr)
library (foreign)
library (car)
library (ggplot2)
library (mirt)
library(psych)
library(ineq)
library(Amelia)
library(stargazer)
library(sjstats)
library (lme4)
library(sjPlot)
library (readxl)
library (merTools)
library(haven)

#### PISA 2015

pisaf <- read.spss("PISA.sav",to.data.frame = T,use.value.labels=F)

pisa.school <- read.spss("pisa-school2015.sav",to.data.frame = T,use.value.labels=F)

variable.names(pisa.school)
summary(pisa.school$W_SCHGRNRABWT)

variable.names(pisa.school)
gc()

pisaf<-pisaf %>% left_join(pisa.school[c(2:3,271)],by=c("CNT","CNTSCHID"))

summary(pisaf$W_SCHGRNRABWT)


#

ggplot(pisaf,aes(x=HOMEPOS,colour=as.factor(CNT)))+
  geom_density(alpha=.3)+
  theme(legend.position="none")

#recode to 1 and 0

pisaf$ST011D17TA[pisaf$ST011D17TA=="9999997"]<-NA
pisaf$ST011D18TA[pisaf$ST011D18TA=="9999997"]<-NA
pisaf$ST011D19TA[pisaf$ST011D19TA=="9999997"]<-NA

pisaf$ST011D17TA<-as.character(pisaf$ST011D17TA)
table(pisaf$ST011D17TA)
pisaf$ST011D17TA<-ifelse(endsWith(pisaf$ST011D17TA,"1"),1,0)
pisaf$ST011D18TA<-as.character(pisaf$ST011D18TA)
table(pisaf$ST011D18TA)
pisaf$ST011D18TA<-ifelse(endsWith(pisaf$ST011D18TA,"1"),1,0)
pisaf$ST011D19TA<-as.character(pisaf$ST011D19TA)
table(pisaf$ST011D19TA)
pisaf$ST011D19TA<-ifelse(endsWith(pisaf$ST011D19TA,"1"),1,0)

table(pisaf$ST011Q01TA)

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
pisaf$ST011Q16NA = car::recode (pisaf$ST011Q16NA,"2=0")

summary(pisaf[40:64])
variable.names(pisaf)
#####

### exclude countries without school-level representativeness
gc()
pisaf2 <- pisaf

#pisaf2<-pisaf2[complete.cases(pisaf2[40:64]),]
 sapply(pisaf2[40:64], function(y) sum(is.na(y)))

sum(is.na(pisaf2$HOMEPOS))

pisaf2<-pisaf2[complete.cases(pisaf2$HOMEPOS), ] 

IndexMat <- sapply(pisaf2[40:64], is.na)
pisaf2<-subset(pisaf2, rowSums(!IndexMat) > 2) 

max(rowSums(is.na(pisaf2[40:64])))

pisaf2<-pisaf2[pisaf2$CNT!="QUE" &pisaf2$CNT!="QUD"& pisaf2$CNT !="QUC" & pisaf2$CNT!="ALB",]

pisaf2<-pisaf2[ ave(1:nrow(pisaf2), pisaf2$CNTSCHID, FUN=length) >= 20 , ]

pisaf2[40:64] <- as.data.frame(lapply(pisaf2[40:64], as.numeric))

pisaf2$CNT<-as.factor(pisaf2$CNT)

pisaf2$CNT<-droplevels(pisaf2$CNT)

#
variable.names(pisaf2)

stargazer(pisaf2[,c(40:64,723)],summary = T,type = "text",median = T,iqr = T,omit.summary.stat = c("p25","p75"))

pisaf2 %>% count(CNT) 

pisaf2%>%summarise (a=n())

counts<-data.frame(table(pisaf2$CNT))
stargazer(counts,summary = F,type="text",rownames = F)

# weights

oldweight<- pisaf %>% 
  group_by(CNT) %>% 
  dplyr::summarise(Frequency = sum(W_FSTUWT),
                   Observation.old = n())

newweight<- pisaf2 %>% 
  group_by(CNT) %>% 
  dplyr::summarise(Frequency = sum(W_FSTUWT),
                   Observation.new = n())


weights<-merge(oldweight,newweight,by="CNT")

weights$reweight<-weights$Frequency.x/weights$Frequency.y

pisaf2<-merge(pisaf2,weights[c(1,6)], by="CNT",all.x = T)

pisaf2$Student.Weight<-pisaf2$W_FSTUWT*pisaf2$reweight

cor(pisaf2$W_FSTUWT,pisaf2$Student.Weight)
# replicate HOMEPOS (to extract alpha parameter)

s<-'F=1-25
CONSTRAINB = (1-13,17-25,a1),(1-13,17-25,d1),(17-25,d2),(17-25,d3),(25,d4),(25,d5)'

group <- as.character(pisaf2$CNT)
weights<-(pisaf2$Student.Weight)

gc()

hom2 <- multipleGroup(data = pisaf2[,40:64], model=s,group= group,  
                     itemtype="gpcm",
                     TOL=.0001,
                     technical = list(removeEmptyRows=TRUE))

#survey.weights=weights,  
gc()


mirtCluster(4)

fs_full <- fscores(hom2, method = 'WLE',
                   full.scores = TRUE, full.scores.SE = F)

mirtCluster(remove = TRUE)

fs_full <-as.data.frame(fs_full)

pisaf2coef<-cbind(pisaf2,fs_full)
gc()

pisaf2coef %>% 
  dplyr::select(HOMEPOS,`F`) %>%
  print()

result <- by(pisaf2coef, pisaf2coef$CNT, function(x) {cor(x$HOMEPOS, x$`F`,use="pairwise")})


result1<-as.list(result)

result1<-do.call(rbind,result1)

stargazer(result1,type="text",summary = F,flip = F,digits = 3)

min(result1)
max(result1)

plot(hom2,1)

## extract alpha parameter

coef25<-coef(hom2,IRTpars=T,as.data.frame=F)

#revisar nrow according to number of countries (complete cases and excluding single school countries os 57)
coef25 <- data.frame(matrix(unlist(coef25),
                            nrow=69, byrow=T),
                     stringsAsFactors=T)
head(coef25)
coef25 <- coef25[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,
                   33,37,41,45,49,53,57,61,65)]

cnt.list<-levels(pisaf2$CNT)

coef25<-cbind(cnt.list,coef25)

variable.names(pisaf2)[1:10]

quest.nam<-variable.names(pisaf2[c(1,40:64)])

old.nam<-variable.names(coef25)

coef25<-coef25 %>% rename_at(vars(old.nam), ~ quest.nam)

variable.names(coef25)
coef25[,-1] <-round(coef25[,-1],3) #the "-1" excludes column 1

stargazer(coef25[1,],type="text",summary = F,flip = T,digits = 3)

stargazer(coef25[,c(1,15:17)],type="text",summary = F,rownames = F)

variable.names(pisaf2)
variable.names(coef25)

pisaf2<-merge(pisaf2,coef25,by.x ="CNT",by.y="CNT")

#####
#####
## normalise/standardise items (0 to 1) all of them

variable.names(pisaf2)[56:64]
pisaf2[56:63] <- lapply(pisaf2[56:63], function(x) x/4)
pisaf2[64] <- lapply(pisaf2[64], function(x) x/6)


#INEQUALITY INDEX

##### first step: possesion * alpha 

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

head(pisaf2$bf25)
gc()

variable.names(pisaf2)

# second step: sum per person

pisaf2$Alpha.Wealth <- apply(pisaf2[,c(950:974)], 1, sum,na.rm=T) ####  REVISAR NUMERACION

#weight for missing data in INEQ scale - weight in case of missing answers
pisaf2$na.count<- rowSums(!is.na((pisaf2[,c(40:64)])))

table(pisaf2$na.count)

summary(pisaf2$Alpha.Wealth)

pisaf2$Alpha.Wealth<-pisaf2$Alpha.Wealth*(pisaf2$na.count/25)
summary(pisaf2$Alpha.Wealth)
###
str(pisaf2$CNTSCHID)
pisaf2$CNTSCHID<-as.factor(pisaf2$CNTSCHID)
pisaf2$CNTSCHID<-droplevels(pisaf2$CNTSCHID)

###

variable.names(pisaf2)

###


# extract SD per school

ineq.school <- pisaf2 %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisaf2 %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

variable.names(ineq.school)

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

variable.names(ineq.school)

pisaf2<-merge(pisaf2,ineq.school[,c(2,4:5)],by="CNTSCHID")

variable.names(pisaf2)
#####
str(pisaf2$CNT)
table(pisaf2$CNT)
str(pisaf2$ineq.alpha)

#

ggplot(pisaf2) + 
  geom_boxplot(aes(x=reorder(CNT,ineq.alpha,FUN = mean),
                   y=ineq.alpha)) +
  theme(axis.text.x = element_text(size=12,angle=90))+
  scale_y_continuous(name=
                       "Inequality School") +
  scale_x_discrete(name="Countries")+
    theme(legend.position="bottom")+
  theme(legend.title=element_blank())

####

summary(pisaf2$HOMEPOS)
pisaf2$HOMEPOS.m<-pisaf2$HOMEPOS+109.4813
min(pisaf2$HOMEPOS.m)

Ineq.HOMEPOS.Atkinson.05<-by(pisaf2$HOMEPOS.m,as.factor(pisaf2$CNTSCHID),
                           ineq::Atkinson,parameter = 0.5,simplify = T)

Ineq.HOMEPOS.Atkinson.1<-by(pisaf2$HOMEPOS.m,as.factor(pisaf2$CNTSCHID),
                          ineq::Atkinson,parameter = 1,simplify = T)

Ineq.HOMEPOS.Atkinson.2<-by(pisaf2$HOMEPOS.m,as.factor(pisaf2$CNTSCHID),
                          ineq::Atkinson,parameter = 2,simplify = T)

Ineq.HOMEPOS.Gini<-by(pisaf2$HOMEPOS.m,as.factor(pisaf2$CNTSCHID),
                    ineq::Gini,corr=T,simplify = T)

Ineq.HOMEPOS.Theil.0<-by(pisaf2$HOMEPOS.m,as.factor(pisaf2$CNTSCHID),
                       ineq::Theil,parameter = 0,simplify = T)

###

Ineq.HOMEPOS.Atkinson.05<-as.data.frame(unlist(as.list(Ineq.HOMEPOS.Atkinson.05),use.names = T))
Ineq.HOMEPOS.Atkinson.1<-as.data.frame(unlist(as.list(Ineq.HOMEPOS.Atkinson.1)))
Ineq.HOMEPOS.Atkinson.2<-as.data.frame(unlist(as.list(Ineq.HOMEPOS.Atkinson.2)))

Ineq.HOMEPOS.Gini<-as.data.frame(unlist(as.list(Ineq.HOMEPOS.Gini)))


Ineq.HOMEPOS.Theil.0<-as.data.frame(unlist(as.list(Ineq.HOMEPOS.Theil.0)))

Ineq.school.tod<-cbind(Ineq.HOMEPOS.Atkinson.05,
                      Ineq.HOMEPOS.Atkinson.1,
                      Ineq.HOMEPOS.Atkinson.2,
                      Ineq.HOMEPOS.Gini,
                      Ineq.HOMEPOS.Theil.0)
                      
Ineq.school.tod$CNTSCHID<-rownames(Ineq.school.tod)
variable.names(Ineq.school.tod) 
variable.names(ineq.school)

Ineq.school.tod<-merge(Ineq.school.tod, ineq.school)

variable.names(Ineq.school.tod)

colnames(Ineq.school.tod)[2]<- "Atkinson.05.HOMEPOS"
colnames(Ineq.school.tod)[3]<- "Atkinson.1.HOMEPOS"
colnames(Ineq.school.tod)[4]<- "Atkinson.2.HOMEPOS"
colnames(Ineq.school.tod)[5]<- "Gini.HOMEPOS"
colnames(Ineq.school.tod)[6]<- "Theil.0.HOMEPOS"

sapply(Ineq.school.tod,function(x)sum(!is.finite(x)))

sapply(Ineq.school.tod, function(x) sum(is.na(x)))

Ineq.school.tod<-Ineq.school.tod[complete.cases(Ineq.school.tod),]

variable.names(Ineq.school.tod)
variable.names(pisaf2)[1:10]
summary(pisaf2$CNTSCHID)
summary(Ineq.school.tod$CNTSCHID)
variable.names(pisaf2)
variable.names(Ineq.school.tod)

pisaf3<-merge(pisaf2,Ineq.school.tod[1:6],by="CNTSCHID")

gc()

#

library(gglorenz)

summary(pisaf3$ESCS)
pisaf4<-pisaf3
pisaf4$ESCS.m<-pisaf4$ESCS+7.2603

pisaf3%>% filter(CNT=="FIN")%>%
ggplot(aes(x=HOMEPOS.m,colour=CNTSCHID)) + 
  stat_lorenz() + 
  geom_abline(color = "black")+
  theme(legend.position="none")




##

avem<-pisaf3%>%group_by(CNT,CNTSCHID)%>%
  summarise(ave=mean(ineq.alpha),
             avem1=sum(ave>1))%>%group_by(CNT)%>%
      summarise(per.c = round(100*sum(avem1==1)/n(),3))%>%arrange(-per.c)
avem<-as.data.frame(avem)
stargazer(avem,type = "text",summary = F,rownames = F)

#
s1<-pisaf3[which(pisaf3$CNTSCHID=="37600120"),]
s1<-s1[40:64]

apply(s1, 2, table)

variable.names(pisaf3)

results.cor.cnt <- by(pisaf3[,c(977,982)], pisaf3$CNT, function(x) cor(x,use ="pairwise"))
results.cor.cnt

results.cor.cnt[] <- lapply(results.cor.cnt,round,3)
results.cor.cnt

results.cor.cnt2<-do.call(rbind, results.cor.cnt)
results.cor.cnt[]


variable.names(results.cor.cnt)

results.cor.cnt.df <- data.frame(matrix(unlist(results.cor.cnt), nrow=69, byrow=T),stringsAsFactors=FALSE)

summary(results.cor.cnt.df$X2)
sd(results.cor.cnt.df$X2)


pisaf3%>%group_by(CNTSCHID)%>%
ggplot(aes(x=ineq.alpha))+
  geom_density(color="darkblue", fill="lightblue")+
  facet_wrap(~CNT,scales = "free")

pisaf3%>%group_by(CNTSCHID)%>%
  ggplot(aes(x=Gini.HOMEPOS))+
  geom_density()+
  geom_density(color="darkgreen", fill="lightgreen")+
    facet_wrap(~CNT,scales = "free")

gc()
##

sum.ineq.cnts<-describeBy(pisaf3$ineq.alpha,pisaf3$CNT,mat = T)
sum.gini.cnts<-describeBy(pisaf3$Gini.HOMEPOS,pisaf3$CNT,mat = T)

variable.names(sum.gini.cnts)

sum.ineq.gini<-merge(sum.ineq.cnts[,c(2,4:6)],
                     sum.gini.cnts[,c(2,5:6)],
                     by="group1")

colnames(sum.ineq.gini)[1]<-"Country"
colnames(sum.ineq.gini)[3]<-"mean.Ineq.Alpha"
colnames(sum.ineq.gini)[4]<-"sd.Ineq.Alpha"
colnames(sum.ineq.gini)[5]<-"mean.Gini.Homepos"
colnames(sum.ineq.gini)[6]<-"sd.Gini.Homepos"

variable.names(sum.ineq.gini)
variable.names(sum.theil.cnts)

cor.test(sum.ineq.gini$mean.Ineq.Alpha,
         sum.ineq.gini$mean.Gini.Homepos)


ineq.country3 <- Ineq.school.tod %>%  group_by(CNT)%>%
  dplyr::summarise(mean.A=mean(ineq.alpha,na.rm = T),
                   cv.A=sd(ineq.alpha)/mean(ineq.alpha),
                   mean.G=mean(Gini.HOMEPOS,na.rm=T),
                   cv.G=sd(Gini.HOMEPOS)/mean(Gini.HOMEPOS))

ggplot(ineq.country3,(aes(mean.A,
                        cv.A,label=CNT)))+
         geom_point()+
          geom_text(check_overlap = F,size=3,vjust = 2)


cor.test(ineq.country3$cv.A,
         ineq.country3$mean.A,alternative = "l",
         )

cor.test(ineq.country3$cv.G,
         ineq.country3$mean.G,alternative = "g")


variable.names(ineq.country3)
colnames(ineq.country3)[1]<-"Country"
colnames(ineq.country3)[2]<-"mean.Ineq.Alpha"
colnames(ineq.country3)[3]<-"coef.variation.Ineq.Alpha"
colnames(ineq.country3)[4]<-"mean.Gini.Homepos"
colnames(ineq.country3)[5]<-"coef.variation.Gini.Homepos"

ineq.country3<-as.data.frame(ineq.country3)
stargazer(ineq.country3,type="text",summary = F,rownames = F,digits = 5)
gc()

####

variable.names(Ineq.school.tod)

ggplot(ineq.country3)+geom_point(aes(x=mean.Ineq.Alpha,y=mean.Gini.Homepos))+
  geom_smooth(aes(x=mean.Ineq.Alpha,y=mean.Gini.Homepos),method = "lm",formula = y ~ x,se=F)

cor.test(ineq.country3$mean.Ineq.Alpha,ineq.country3$mean.Gini.Homepos)

ggplot(ineq.country3)+geom_point(aes(x=cv.A,y=mean.A))

cor.test(ineq.country3$cv.A,ineq.country3$mean.A)
ineq.country3<-as.data.frame(ineq.country3)
stargazer(ineq.country3,type="text",summary = F)



ggplot(pisaf3) + 
  geom_histogram(aes(x=Gini.HOMEPOS),bins = 20)+
  facet_wrap(~CNT,scales = "free")

##
gc()

meanpi<-pisa2015.mean.pv(pvlabel = "MATH",
                         by=c("CNTSCHID","CNT"),data=pisaf3)

variable.names(meanpi)
ineq.country4<-merge(Ineq.school.tod,meanpi,by="CNTSCHID")

variable.names(ineq.country4)

ineq.country5<-ineq.country4%>%
   group_by(CNT.x)%>%
      slice_max(order_by=ineq.alpha,prop=.2)   %>%
          dplyr::select(CNTSCHID,CNT.x,Gini.HOMEPOS,
                        ineq.alpha,Mean)

ineq.country5$t20<-1
head(ineq.country5)

ineq.country6<-ineq.country4%>%group_by(CNT.x)%>%
  slice_min(order_by=ineq.alpha,prop=.2)  %>%
  dplyr::select(CNTSCHID,CNT.x,Gini.HOMEPOS,ineq.alpha,Mean)

ineq.country6$t20<-0

ineq.country7<-rbind(ineq.country6,ineq.country5)

ineq.country7$t20<-as.factor(ineq.country7$t20)

ttest<-by(ineq.country7,ineq.country7$CNT.x, 
          function (x) t.test(x$Mean ~x$t20))

ineq.country7%>%filter(CNT.x=="USA")%>%ggplot()+
  geom_point(aes(x=ineq.alpha,y=Mean,colour=t20))

ttest

head(ineq.country7)

###

ineq.country5b<-ineq.country4%>%
  group_by(CNT.x)%>%
  slice_max(order_by=Gini.HOMEPOS,prop=.2)   %>%
  dplyr::select(CNTSCHID,CNT.x,Gini.HOMEPOS,
                ineq.alpha,Mean)

ineq.country5b$t20<-1
head(ineq.country5b)

ineq.country6b<-ineq.country4%>%group_by(CNT.x)%>%
  slice_min(order_by=Gini.HOMEPOS,prop=.2)  %>%
  dplyr::select(CNTSCHID,CNT.x,Gini.HOMEPOS,ineq.alpha,Mean)

ineq.country6b$t20<-0

ineq.country7b<-rbind(ineq.country6b,ineq.country5b)

ineq.country7b$t20<-as.factor(ineq.country7b$t20)

ttest.b<-by(ineq.country7b,ineq.country7b$CNT.x, 
          function (x) t.test(x$Mean ~x$t20))

ttest.b

####

gd <- ineq.country7 %>% filter(CNT.x=="USA")%>%
  group_by(t20) %>% 
  summarise(Mean  = mean(Mean),
            ineq.alpha=mean(ineq.alpha))

aa<-ineq.country7%>%filter(CNT.x=="USA")%>%ggplot()+
  geom_point(aes(x=scale(ineq.alpha),y=Mean,colour=t20),size=2)+
  xlab("Alpha Inequality")+theme_few()+theme(legend.position = "none")+
  geom_point(data = gd, aes(scale(ineq.alpha),Mean),size = 4,shape=17)

gd1 <- ineq.country7b %>% filter(CNT.x=="USA")%>%
  group_by(t20) %>% 
  summarise(Mean  = mean(Mean),
            Gini.HOMEPOS=mean(Gini.HOMEPOS))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


bb<-ineq.country7b%>%filter(CNT.x=="USA")%>%ggplot()+
  geom_point(aes(x=Gini.HOMEPOS,y=Mean,colour=t20),size=2)+xlab("Gini")+theme_few()+ylab("")+
  theme(legend.position = "right")+
  geom_point(data = gd1, aes(Gini.HOMEPOS,Mean),size = 4,shape=17)+
  scale_colour_discrete(name = "20% inequality", labels = c("Lowest","Highest"))

legend <- get_legend(bb)
                     
bb <- bb + theme(legend.position="none")

grid.arrange(aa,bb,legend,ncol=3,widths=c(2.3, 2.3, 0.8))


