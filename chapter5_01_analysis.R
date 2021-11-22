library(lme4)
library(patchwork)
library(tidyverse)
library(BIFIEsurvey)

# Pisa 2018 ---------------------------------------------------------------

pisa2018 <- readRDS("C:/Users/LUCAS/Desktop/PISA INEQUALITY/PISA 2018/pisa.c.s.RDS")

gc()


adjusting.weights <- pisa2018 %>%
  mutate(squared.weight=Student.Weight*Student.Weight) %>%
  group_by(CNTSCHID) %>%
  summarise (
    school.size = n(),
    sum.weights.students=sum(Student.Weight),
    sum.sqr=sum(squared.weight),
    adjust.weight = school.size/sum.weights.students,
    sum.squared.weight=sum.weights.students/sum.sqr)

gc()

pisa2018 <- pisa2018 %>% left_join(adjusting.weights,by="CNTSCHID") %>%
  mutate(std.weight.final = Student.Weight*adjust.weight,
         std.weight.final2 = Student.Weight*sum.squared.weight)

ineq.school <- pisa2018 %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(mean.alpha.school=mean(Alpha.Wealth,na.rm = T),
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2018 %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

pisa2018<-pisa2018 %>% left_join (ineq.school[,c(2,6)])

#plot(ineq.school$ineq.alpha,ineq.school$mean.alpha.school)

# saveRDS(pisa2018,"pisa2018.RDS")

variable.names(pisa2018) [1:1000]

variable.names(pisa2018)[1001:1407]

pisa2018<-pisa2018 %>% dplyr::select(1,2,18,63,835,846,860,862,878,
                                     945:1024,1027:1046,
                                     1128,1129,
                                     1294,1302,
                                     1309,1372,1365,
                                     1371,1377,
                                     1378,
                                     1379,
                                     1380,1382,1399:1407)

# only Gini

# pisa2018<-pisa2018 %>% dplyr::select(1,2,18,63,835,846,860,862,878,
#                                      945:1024,1027:1046,
#                                      1128,1129,
#                                      1294,1302,
#                                      1309,1372,
#                                      1371,1380,1405)



variable.names(ineq.school)

ineq.country3 <- ineq.school %>%  group_by(CNT)%>%
  dplyr::summarise(mean.A=mean(ineq.alpha,na.rm = T),
                   cv.A=sd(ineq.alpha)/mean(ineq.alpha))

pisa2018<-merge(pisa2018,ineq.country3,by="CNT")

summary(pisa2018$school.HOMEPOS)



pisa2018$school.HOMEPOS.m<-pisa2018$school.HOMEPOS+6.6546

summary(pisa2018$school.HOMEPOS.m)


Country.Gini.schools<-by(pisa2018$school.HOMEPOS.m,as.factor(pisa2018$CNT),
                      ineq::Gini,simplify = T,na.rm=T)


Country.Gini.schools<-as.data.frame(unlist(as.list(Country.Gini.schools),use.names = T))

Country.Gini.schools$CNT<-rownames(Country.Gini.schools)

colnames(Country.Gini.schools)[1]<- "Country.Gini.school_HOMEPOS"

pisa2018<-pisa2018 %>% left_join(Country.Gini.schools,by="CNT")


ineq.country3 <- ineq.country3 %>% left_join(Country.Gini.schools)

#

colSums(is.na(pisa2018))


pisa2018<-pisa2018[complete.cases(pisa2018),]

##






#

disim<-pisa2018 %>% group_by(CNT) %>%
  mutate (median.HOMEPOS = median(HOMEPOS,na.rm=T)) %>% group_by(CNT, CNTSCHID)%>%
  summarise(
    count.low = length(HOMEPOS[HOMEPOS<=median.HOMEPOS]),
    count.high = length(HOMEPOS[HOMEPOS>=median.HOMEPOS]))

duncan<-disim %>% group_by(CNT) %>%
  do(a=OasisR::DIDuncan(.[3:4])) %>%
  unnest(a)

duncan <- duncan %>% left_join(ineq.country3)

duncan <- duncan %>% filter (a !=0)

d1<-duncan %>% ggplot () + geom_point(aes(a[,1],(-1*mean.A)+1)) + xlab("Dissimilarity index - median") +
  ylab("Country Segregation based on Alpha Inequality") + theme_minimal()


duncan$duncan.median<-duncan$a[,1]
duncan$mean.A<-(duncan$mean.A)*-1

variable.names(duncan)

library(GGally)

duncan <- duncan %>% rename(Country_School_Segregation_Alpha=mean.A,
                            Country_School_Gini= Country.Gini.school_HOMEPOS,
                            Country_School_Segregation_Duncan=duncan.median )

ggpairs(duncan[,c(5,3,6)]) +
  theme_minimal()

write.csv(duncan,"seg.csv")



#


quantiles<-pisa2018 %>% group_by(CNT) %>%
  summarise(quantile = scales::percent(c(0.2)),
         HOMEPOS.q = quantile(HOMEPOS, c(0.2)))


disim2<-pisa2018 %>% left_join(quantiles,by= "CNT") %>%
group_by(CNT, CNTSCHID) %>%
  summarise(count.low = length(HOMEPOS[HOMEPOS<=HOMEPOS.q]),
            count.high = length(HOMEPOS[HOMEPOS>=HOMEPOS.q]))


duncan2<-disim2 %>% group_by(CNT) %>%
  do(a=OasisR::DIDuncan(.[3:4])) %>%
  unnest(a)

duncan2 <- duncan2 %>% left_join(ineq.country3)

duncan2 <- duncan2 %>% filter (a !=0)

d2<-duncan2 %>% ggplot () + geom_point(aes(a[,1],(-1*mean.A)+1)) + xlab("Dissimilarity index - 20%") +
  ylab("")+ theme_minimal()



quantiles2<-pisa2018 %>% group_by(CNT) %>%
  summarise(quantile = scales::percent(c(0.8)),
            HOMEPOS.q = quantile(HOMEPOS, c(0.8)))


disim3<-pisa2018 %>% left_join(quantiles2,by= "CNT") %>%
  group_by(CNT, CNTSCHID) %>%
  summarise(count.low = length(HOMEPOS[HOMEPOS<=HOMEPOS.q]),
            count.high = length(HOMEPOS[HOMEPOS>=HOMEPOS.q]))


duncan3<-disim3 %>% group_by(CNT) %>%
  do(a=OasisR::DIDuncan(.[3:4])) %>%
  unnest(a)

duncan3 <- duncan3 %>% left_join(ineq.country3)

duncan3 <- duncan3 %>% filter (a !=0)

d3<-duncan3 %>% ggplot () + geom_point(aes(a[,1],(-1*mean.A)+1)) + xlab("Dissimilarity index - 80%") +
  ylab("") + theme_minimal()

cor.test(duncan$a[,1],duncan$Country_School_Segregation_Alpha)
cor.test(duncan2$a[,1],duncan2$mean.A)
cor.test(duncan3$a[,1],duncan3$mean.A)


d1+d2 +d3

duncan3$inv.mean<-duncan3$mean.A*-1
duncan3$inv.mean1<-duncan3$inv.mean+1

range(duncan3$mean.A)
range(duncan3$inv.mean)
range(duncan3$inv.mean1)

plot(duncan3$a[,1],duncan3$inv.mean1)

plot(duncan3$inv.mean,duncan3$inv.mean1)

##



##

library(lme4)

variable.names(pisa2018)


pisa2018$mean.A<- (pisa2018$mean.A*-1)+1


memory.limit(size=30000)

#


fit.lmer.18.cv.00<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+  school.HOMEPOS + HOMEPOS  + ineq.alpha + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.0<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+  school.HOMEPOS + HOMEPOS  + mean.A + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.1<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+  school.HOMEPOS + HOMEPOS  + ineq.alpha + mean.A + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

library(car)
vif(fit.lmer.18.cv.1)

fit.lmer.18.cv.2<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                      AGE + REPEAT  + Tipo.escola +
                      Area+ school.HOMEPOS + HOMEPOS  + mean.A*ineq.alpha + (1|CNT),
                    data= pisa2018, weights = std.weight.final)

vif(fit.lmer.18.cv.2)

performance::check_collinearity(fit.lmer.18.cv.2)


fit.lmer.18.cv.3<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ school.HOMEPOS + HOMEPOS + school.HOMEPOS  *mean.A+ineq.alpha + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.4<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ HOMEPOS+ school.HOMEPOS + school.HOMEPOS  * mean.A*ineq.alpha + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

performance::check_collinearity(fit.lmer.18.cv.4)

vif(fit.lmer.18.cv.4)

performance::check_collinearity()

sjstats::r2(fit.lmer.18.cv.00)

summary(fit.lmer.18.cv.4)

fit.lmer.18.cv.gini<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ HOMEPOS+ school.HOMEPOS +
                           school.HOMEPOS  * Country.Gini.school_HOMEPOS*Gini + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

summary(fit.lmer.18.cv.gini)


duncan$cv.A<-NULL

pisa2018 <- pisa2018 %>% left_join(duncan)

fit.lmer.18.cv.duncan<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  * Country_School_Segregation_Duncan*Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan.alpha<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                              AGE + REPEAT  + Tipo.escola +
                              Area+ HOMEPOS+ school.HOMEPOS +
                              school.HOMEPOS  * Country_School_Segregation_Duncan*ineq.alpha + (1|CNT),
                            data= pisa2018, weights = std.weight.final)

summary(fit.lmer.18.cv.duncan.alpha)


interactions::interact_plot(fit.lmer.18.cv.2,modx = ineq.alpha,
                            pred = mean.A,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country segregation",
                            y.label = "PISA score",
                            legend.main = "School inequality")



interactions::interact_plot(fit.lmer.18.cv.4,modx = ineq.alpha,
                            pred = mean.A, mod2 = school.HOMEPOS,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country segregation",
                            y.label = "PISA score",
                            legend.main = "School inequality")

interactions::interact_plot(fit.lmer.18.cv.duncan.alpha,modx = ineq.alpha,
                            pred = Country_School_Segregation_Duncan,
                            mod2 = school.HOMEPOS,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country Duncan Segregation",
                            y.label = "PISA score",
                            legend.main = "School Alpha Ineq")


interactions::interact_plot(fit.lmer.18.cv.gini,modx = Gini,
                            pred = Country.Gini.school_HOMEPOS,
                            mod2 = school.HOMEPOS,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country Gini",
                            y.label = "PISA score",
                            legend.main = "School Gini")



interactions::interact_plot(fit.lmer.18.cv.duncan,modx = Gini,
                            pred = Country_School_Segregation_Duncan,
                            mod2 = school.HOMEPOS,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country Duncan Segregation",
                            y.label = "PISA score",
                            legend.main = "School Gini")



stargazer::stargazer(fit.lmer.18.cv.00,
                     fit.lmer.18.cv.0,
                     fit.lmer.18.cv.1,
                     fit.lmer.18.cv.2,
                     fit.lmer.18.cv.4,  type = "text")

gc()

plot_cme(fit.lmer.18.cv.2, effect = "ineq.alpha", condition = c("mean.A"))

library(marginaleffects)

plot_cme(fit.lmer.18.cv.4, effect = "ineq.alpha", condition = c("school.HOMEPOS","mean.A"))

me2<-marginaleffects(fit.lmer.18.cv.2)

me4<-marginaleffects(fit.lmer.18.cv.4)

summary(me4$mean.A)

#
pisa2018 <- pisa2018 %>%
  arrange(CNT)

variable.names(pisa2018)

cor(pisa2018$mean.A,pisa2018$Country_School_Segregation_Alpha)

brr <- BIFIE.data.jack(data = pisa2018[,c(1:129,131,135,137:139)],
                       wgt ="std.weight.final",
                       jktype = "RW_PISA",
                       pvpre = paste0("PV",1:10),
                       wgtrep = paste0("W_FSTURWT",1:80),
                       cdata = F)

fit1 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr,
dep="MATH",
formula.fixed=~ Sex + HISCED +IMMIG + Language +
AGE + REPEAT  + Tipo.escola + Area +
HOMEPOS + school.HOMEPOS + ineq.alpha ,
formula.random=~ 1,
idcluster="CNT",
wgtlevel1="std.weight.final",
wgtlevel2 = "one")

gc()

summary (fit1)

fit2 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr,
dep="MATH",
formula.fixed=~ Sex + HISCED +IMMIG + Language +
AGE + REPEAT  + Tipo.escola + Area +
HOMEPOS + school.HOMEPOS + Country_School_Segregation_Alpha ,
formula.random=~ 1,
idcluster="CNT",
wgtlevel1="std.weight.final",
wgtlevel2 = "one")

summary(fit2)

fit3 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr,
dep="MATH",
formula.fixed=~ Sex + HISCED +IMMIG + Language +
AGE + REPEAT  + Tipo.escola + Area +
HOMEPOS + school.HOMEPOS + ineq.alpha + Country_School_Segregation_Alpha ,
formula.random=~ 1,
idcluster="CNT",
wgtlevel1="std.weight.final",
wgtlevel2 = "one")

gc()

fit4 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area +
                                         HOMEPOS + school.HOMEPOS + ineq.alpha * Country_School_Segregation_Alpha ,
                                       formula.random=~ 1,
                                       idcluster="CNT",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

fit5 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area +
                                         HOMEPOS + school.HOMEPOS * ineq.alpha * Country_School_Segregation_Alpha ,
                                       formula.random=~ 1,
                                       idcluster="CNT",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

fi1s<-as.data.frame(summary(fit1))
fi2s<-as.data.frame(summary(fit2))
fi3s<-as.data.frame(summary(fit3))
fi4s<-as.data.frame(summary(fit4))
fi5s<-as.data.frame(summary(fit5))


fi1s<-fi1s[,c(1:3,6)]
fi2s<-fi2s[,c(1:3,6)]
fi3s<-fi3s[,c(1:3,6)]
fi4s<-fi4s[,c(1:3,6)]
fi5s<-fi5s[,c(1:3,6)]

fi1s<-fi1s %>%
  mutate (est=as.numeric(est),
          SE = as.numeric(SE),
          p=as.numeric(p),
          pv=case_when(p<.001 ~ "***",
                                    p>.001 & p<.01 ~ "**",
                                    p<.01 & p<.05 ~ "*",
                                    T ~ "")) %>%
  mutate ("(1)" = paste0(est," (",SE,") ",pv)) %>% select(parameter,"(1)")

fi2s<-fi2s %>%
  mutate (est=as.numeric(est),
          SE = as.numeric(SE),
          p=as.numeric(p),
          pv=case_when(p<.001 ~ "***",
                       p>.001 & p<.01 ~ "**",
                       p<.01 & p<.05 ~ "*",
                       T ~ "")) %>%
  mutate ("(2)" = paste0(est," (",SE,") ",pv)) %>% select(parameter,"(2)")

fi3s<-fi3s %>%
  mutate (est=as.numeric(est),
          SE = as.numeric(SE),
          p=as.numeric(p),
          pv=case_when(p<.001 ~ "***",
                       p>.001 & p<.01 ~ "**",
                       p<.01 & p<.05 ~ "*",
                       T ~ "")) %>%
  mutate ("(3)" = paste0(est," (",SE,") ",pv)) %>% select(parameter,"(3)")

fi4s<-fi4s %>%
  mutate (est=as.numeric(est),
          SE = as.numeric(SE),
          p=as.numeric(p),
          pv=case_when(p<.001 ~ "***",
                       p>.001 & p<.01 ~ "**",
                       p<.01 & p<.05 ~ "*",
                       T ~ "")) %>%
  mutate ("(4)" = paste0(est," (",SE,") ",pv)) %>% select(parameter,"(4)")

fi5s<-fi5s %>%
  mutate (est=as.numeric(est),
          SE = as.numeric(SE),
          p=as.numeric(p),
          pv=case_when(p<.001 ~ "***",
                       p>.001 & p<.01 ~ "**",
                       p<.01 & p<.05 ~ "*",
                       T ~ "")) %>%
  mutate ("(5)" = paste0(est," (",SE,") ",pv)) %>% select(parameter,"(5)")


fitfinal<-fi1s %>% full_join(fi2s) %>% full_join(fi3s) %>% full_join(fi4s) %>% full_join(fi5s)

write.csv(fitfinal,"fitfinal.csv")



###

gc()

fit.lmer.18.cv.gini0<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS+ Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini1<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  + Country.Gini.school_HOMEPOS + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini2<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  + Country.Gini.school_HOMEPOS+Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini3<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  + Country.Gini.school_HOMEPOS*Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini4<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  * Country.Gini.school_HOMEPOS*Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)


##

fit.lmer.18.cv.duncan1<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  + Country_School_Segregation_Duncan + (1|CNT),
                           data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan2<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  + Country_School_Segregation_Duncan+Gini + (1|CNT),
                           data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan3<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  + Country_School_Segregation_Duncan*Gini + (1|CNT),
                           data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan4<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  * Country_School_Segregation_Duncan*Gini + (1|CNT),
                           data= pisa2018, weights = std.weight.final)


#

fit.lmer.18.cv.duncan2.a<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                               AGE + REPEAT  + Tipo.escola +
                               Area+ HOMEPOS+ school.HOMEPOS +
                               school.HOMEPOS  + Country_School_Segregation_Duncan+ineq.alpha + (1|CNT),
                             data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan3.a<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                               AGE + REPEAT  + Tipo.escola +
                               Area+ HOMEPOS+ school.HOMEPOS +
                               school.HOMEPOS  + Country_School_Segregation_Duncan*ineq.alpha + (1|CNT),
                             data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan4.a<-lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                               AGE + REPEAT  + Tipo.escola +
                               Area+ HOMEPOS+ school.HOMEPOS +
                               school.HOMEPOS  * Country_School_Segregation_Duncan*ineq.alpha + (1|CNT),
                             data= pisa2018, weights = std.weight.final)




stargazer::stargazer(fit.lmer.18.cv.gini0,
                     fit.lmer.18.cv.gini1,
                     fit.lmer.18.cv.gini2,
                     fit.lmer.18.cv.gini3,
                     fit.lmer.18.cv.gini4,  type = "text")




stargazer::stargazer(fit.lmer.18.cv.duncan1,
                     fit.lmer.18.cv.duncan2,
                     fit.lmer.18.cv.duncan3,
                     fit.lmer.18.cv.duncan4,type = "text")
                     # fit.lmer.18.cv.duncan2.a,
                     # fit.lmer.18.cv.duncan3.a,
                     # fit.lmer.18.cv.duncan4.a,
##

variable.names(pisa2018)

gc()

pc<-pisa2018 %>% group_by(CNT) %>%
  summarise(school.HOMEPOS=mean(school.HOMEPOS,na.rm=T),
         Country_School_Segregation_Alpha=mean(Country_School_Segregation_Alpha,na.rm=T)) %>%
  select(CNT,school.HOMEPOS,Country_School_Segregation_Alpha)

pc%>%
    ggplot(aes(school.HOMEPOS,Country_School_Segregation_Alpha+1,label=CNT)) + geom_point() +
  theme_minimal () + ggrepel::geom_label_repel(aes(label = CNT),
                                     box.padding   = 0.35,
                                     point.padding = 0.5,
                                     segment.color = 'grey50')

pc1<-pisa2018 %>% group_by(CNT) %>%
  summarise(PV1READ=mean(PV1READ),
            school.HOMEPOS=mean(school.HOMEPOS,na.rm=T),
            Country_School_Segregation_Alpha=mean(Country_School_Segregation_Alpha,na.rm=T)) %>%
  select(CNT,PV1READ,school.HOMEPOS,Country_School_Segregation_Alpha)


pc1%>%
  ggplot(aes(Country_School_Segregation_Alpha+1,PV1READ,label=CNT)) + geom_point() +
  theme_minimal () + ggrepel::geom_label_repel(aes(label = CNT),
                                               box.padding   = 0.35,
                                               point.padding = 0.5,
                                               segment.color = 'grey50')


pc1<-pisa2018 %>% group_by(CNT) %>%
  summarise(PV1READ=mean(PV1READ),
            school.HOMEPOS=mean(school.HOMEPOS,na.rm=T),
            Country_School_Segregation_Alpha=mean(Country_School_Segregation_Alpha,na.rm=T)) %>%
  select(CNT,PV1READ,school.HOMEPOS,Country_School_Segregation_Alpha)

cor(pisa2018$Country_School_Segregation_Alpha,pisa2018$ineq.alpha,use="pairwise")

pisa2018 %>% ggplot () + geom_point(aes(ineq.alpha,Country_School_Segregation_Alpha))


###

variable.names(pisa2018)

flex<-pisa2018 %>% select(CNT,Sex,AGE,REPEAT,HOMEPOS,school.HOMEPOS,PV1READ:PV10READ,Area,
                          Tipo.escola,ineq.alpha,mean.A,
                          Country_School_Segregation_Duncan,Gini,IMMIG,Language,HISCED) %>%
  mutate(Sex=as.factor(Sex),
         REPEAT=as.factor(REPEAT),
         Area=as.factor(Area),
         Tipo.escola=as.factor(Tipo.escola),
         IMMIG=as.factor(IMMIG),
         Language=as.factor(Language),
         HISCED=as.factor(HISCED))%>%
  gtsummary::tbl_summary(#by = CNT,
    missing = "no") %>%
  gtsummary::as_flex_table()

flextable::save_as_docx(flex,
                        path="C:/Users/LUCAS/Desktop/Chapter 3 Mediation/table1segregation.docx")

