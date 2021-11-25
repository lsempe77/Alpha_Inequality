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
  mutate (median.HOMEPOS = median(HOMEPOS,na.rm=T)) %>% 
  group_by(CNT, CNTSCHID)%>%
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


fit.lmer.18.cv.00<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+  school.HOMEPOS + HOMEPOS  + ineq.alpha + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.0<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+  school.HOMEPOS + HOMEPOS  + mean.A + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.1<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+  school.HOMEPOS + HOMEPOS  + ineq.alpha + mean.A + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

library(car)


fit.lmer.18.cv.2<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                      AGE + REPEAT  + Tipo.escola +
                      Area+ school.HOMEPOS + HOMEPOS  + mean.A*ineq.alpha + (1|CNT),
                    data= pisa2018, weights = std.weight.final)

vif(fit.lmer.18.cv.2)

performance::check_collinearity(fit.lmer.18.cv.2)


fit.lmer.18.cv.3<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ school.HOMEPOS + HOMEPOS + school.HOMEPOS  *mean.A+ineq.alpha + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.4<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ HOMEPOS+ school.HOMEPOS + school.HOMEPOS  * mean.A*ineq.alpha + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

performance::check_collinearity(fit.lmer.18.cv.4)


sjstats::r2(fit.lmer.18.cv.00)
vif(fit.lmer.18.cv.4)


sjstats::r2(fit.lmer.18.cv.00)

summary(fit.lmer.18.cv.4)

fit.lmer.18.cv.gini<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ HOMEPOS+ school.HOMEPOS +
                           school.HOMEPOS  * Country.Gini.school_HOMEPOS*Gini + (1|CNT),
                       data= pisa2018, weights = std.weight.final)

summary(fit.lmer.18.cv.gini)


duncan$cv.A<-NULL

pisa2018 <- pisa2018 %>% left_join(duncan)

fit.lmer.18.cv.duncan<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  * Country_School_Segregation_Duncan*Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan.alpha<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
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

v1<-as.data.frame(vif(fit.lmer.18.cv.00)) %>% select (GVIF) %>% 
    rownames_to_column(var = "Parameters") %>%   rename("(1)" =GVIF)

v2<-as.data.frame(vif(fit.lmer.18.cv.0)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(2)" =GVIF)

v3<-as.data.frame(vif(fit.lmer.18.cv.1)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(3)" =GVIF)

v4<-as.data.frame(vif(fit.lmer.18.cv.2)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(4)" =GVIF)

v5<-as.data.frame(vif(fit.lmer.18.cv.4)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(6)" =GVIF)



vif.final<-v1 %>% full_join(v2) %>% full_join(v3) %>% full_join(v4) %>% full_join(v5)
write.csv(vif.final,"vif.final.csv")

sjstats::r2(fit.lmer.18.cv.00)
sjstats::r2(fit.lmer.18.cv.0)
sjstats::r2(fit.lmer.18.cv.1)
sjstats::r2(fit.lmer.18.cv.2)
sjstats::r2(fit.lmer.18.cv.4)

gc()

plot_cme(fit.lmer.18.cv.2, effect = "ineq.alpha", condition = c("mean.A"))

library(marginaleffects)

plot_cme(fit.lmer.18.cv.4, effect = "ineq.alpha", condition = c("school.HOMEPOS","mean.A"))

me2<-marginaleffects(fit.lmer.18.cv.2)

me4<-marginaleffects(fit.lmer.18.cv.4)

summary(me4$mean.A)

#
###

gc()

fit.lmer.18.cv.gini0<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS + Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini1<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  + Country.Gini.school_HOMEPOS + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini2<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  + Country.Gini.school_HOMEPOS+Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini3<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  + Country.Gini.school_HOMEPOS*Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.gini4<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ HOMEPOS+ school.HOMEPOS +
                            school.HOMEPOS  * Country.Gini.school_HOMEPOS*Gini + (1|CNT),
                          data= pisa2018, weights = std.weight.final)


##

fit.lmer.18.cv.duncan1<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  + Country_School_Segregation_Duncan + (1|CNT),
                           data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan2<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  + Country_School_Segregation_Duncan+Gini + (1|CNT),
                           data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan3<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  + Country_School_Segregation_Duncan*Gini + (1|CNT),
                           data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan4<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola +
                             Area+ HOMEPOS+ school.HOMEPOS +
                             school.HOMEPOS  * Country_School_Segregation_Duncan*Gini + (1|CNT),
                           data= pisa2018, weights = std.weight.final)


#

fit.lmer.18.cv.duncan2.a<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                               AGE + REPEAT  + Tipo.escola +
                               Area+ HOMEPOS+ school.HOMEPOS +
                               school.HOMEPOS  + Country_School_Segregation_Duncan+ineq.alpha + (1|CNT),
                             data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan3.a<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                               AGE + REPEAT  + Tipo.escola +
                               Area+ HOMEPOS+ school.HOMEPOS +
                               school.HOMEPOS  + Country_School_Segregation_Duncan*ineq.alpha + (1|CNT),
                             data= pisa2018, weights = std.weight.final)

fit.lmer.18.cv.duncan4.a<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                               AGE + REPEAT  + Tipo.escola +
                               Area+ HOMEPOS+ school.HOMEPOS +
                               school.HOMEPOS  * Country_School_Segregation_Duncan*ineq.alpha + (1|CNT),
                             data= pisa2018, weights = std.weight.final)



interactions::interact_plot(fit.lmer.18.cv.duncan4.a,modx = ineq.alpha,
                            pred = Country_School_Segregation_Duncan,
                            mod2 = school.HOMEPOS,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country Duncan",
                            y.label = "PISA score",
                            legend.main = "School Alpha")

stargazer::stargazer(fit.lmer.18.cv.gini1,
                     fit.lmer.18.cv.gini2,
                     fit.lmer.18.cv.gini3,
                     fit.lmer.18.cv.gini4,  type = "text")


v1.g<-as.data.frame(vif(fit.lmer.18.cv.gini0)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(1)" =GVIF)

v2.g<-as.data.frame(vif(fit.lmer.18.cv.gini1)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(2)" =GVIF)

v3.g<-as.data.frame(vif(fit.lmer.18.cv.gini2)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(3)" =GVIF)

v4.g<-as.data.frame(vif(fit.lmer.18.cv.gini3)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(4)" =GVIF)

v5.g<-as.data.frame(vif(fit.lmer.18.cv.gini4)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(6)" =GVIF)

vif.final.g<-v2.g %>% full_join(v3.g) %>% full_join(v4.g) %>% full_join(v5.g)

write.csv(vif.final.g,"vif.final.g.csv")

cor(pisa2018$PV5READ,pisa2018$PV8MATH)

stargazer::stargazer(fit.lmer.18.cv.duncan1,
                     fit.lmer.18.cv.duncan2,
                     fit.lmer.18.cv.duncan3,
                     fit.lmer.18.cv.duncan4,
                     fit.lmer.18.cv.duncan2.a,
                     fit.lmer.18.cv.duncan3.a,
                     fit.lmer.18.cv.duncan4.a,type = "text")


v1.d<-as.data.frame(vif(fit.lmer.18.cv.duncan1)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(1)" =GVIF)

v2.d<-as.data.frame(vif(fit.lmer.18.cv.duncan2)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(2)" =GVIF)

v3.d<-as.data.frame(vif(fit.lmer.18.cv.duncan3)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(3)" =GVIF)

v4.d<-as.data.frame(vif(fit.lmer.18.cv.duncan4)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(4)" =GVIF)

v5.d<-as.data.frame(vif(fit.lmer.18.cv.duncan2.a)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(5)" =GVIF)

v6.d<-as.data.frame(vif(fit.lmer.18.cv.duncan3.a)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(6)" =GVIF)

v7.d<-as.data.frame(vif(fit.lmer.18.cv.duncan4.a)) %>% select (GVIF) %>% 
  rownames_to_column(var = "Parameters") %>%   rename("(7)" =GVIF)

vif.final.d<-v1.d %>% full_join(v2.d) %>% full_join(v3.d) %>% 
  full_join(v4.d) %>% full_join(v5.d )%>% full_join(v6.d) %>% full_join(v7.d)

write.csv(vif.final.d,"vif.final.d.csv")

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

###

GDP2015 <- read_excel("C:/Users/LUCAS/Desktop/PISA INEQUALITY/R PISA 0/GDP2015.xls")

GDP2015$...5<-NULL

GDP2015$`Year of GINI`<-NULL

pisa2018 <- pisa2018 %>% left_join(GDP2015)

variable.names(pisa2018)

###

gdp.fit.lmer.18.cv.00<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                          AGE + REPEAT  + Tipo.escola +
                          Area+  school.HOMEPOS + HOMEPOS  + ineq.alpha + 
                          `GNI per capita`     +         `Gini index` +
                          (1|CNT),
                        data= pisa2018, weights = std.weight.final)

gdp.fit.lmer.18.cv.0<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+  school.HOMEPOS + HOMEPOS  + mean.A + 
                           `GNI per capita`     +         `Gini index` +
                           (1|CNT),
                       data= pisa2018, weights = std.weight.final)

gdp.fit.lmer.18.cv.1<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+  school.HOMEPOS + HOMEPOS  + ineq.alpha + mean.A +
                           `GNI per capita`     +         `Gini index` +
                           (1|CNT),
                       data= pisa2018, weights = std.weight.final)

vif(gdp.fit.lmer.18.cv.1)

gdp.fit.lmer.18.cv.2<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ school.HOMEPOS + HOMEPOS  + mean.A*ineq.alpha + 
                           `GNI per capita`     +         `Gini index` +
                           (1|CNT),
                       data= pisa2018, weights = std.weight.final)

vif(gdp.fit.lmer.18.cv.2)

performance::check_collinearity(gdp.fit.lmer.18.cv.2)


gdp.fit.lmer.18.cv.3<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ school.HOMEPOS + HOMEPOS + school.HOMEPOS  *mean.A+ineq.alpha + 
                           `GNI per capita`     +         `Gini index` +
                           (1|CNT),
                       data= pisa2018, weights = std.weight.final)

gdp.fit.lmer.18.cv.4<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                         AGE + REPEAT  + Tipo.escola +
                         Area+ HOMEPOS+ school.HOMEPOS + school.HOMEPOS  * mean.A*ineq.alpha + 
                           `GNI per capita`     +         `Gini index` +
                           (1|CNT),
                       data= pisa2018, weights = std.weight.final)



stargazer::stargazer(gdp.fit.lmer.18.cv.00,
                     gdp.fit.lmer.18.cv.0,
                     gdp.fit.lmer.18.cv.1,
                     gdp.fit.lmer.18.cv.2,
                     gdp.fit.lmer.18.cv.4,  type = "text")


interactions::interact_plot(gdp.fit.lmer.18.cv.4,modx = ineq.alpha,
                            pred = mean.A,
                            mod2 = school.HOMEPOS,
                            alpha = .05, interval = F,
                            robust = T,
                            colors = "Qual3",
                            x.label = "Country Segregation (Alpha)",
                            y.label = "PISA score",
                            legend.main = "School Alpha")
