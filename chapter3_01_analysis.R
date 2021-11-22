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
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2018 %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

pisa2018<-pisa2018 %>% left_join (ineq.school[,c(2,5)])

# saveRDS(pisa2018,"pisa2018.RDS")

variable.names(pisa2018)[1:1000]

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

pisa2018<-pisa2018 %>% dplyr::select(1,2,18,63,835,846,860,862,878,
                                     945:1024,1027:1046,
                                     1128,1129,
                                     1294,1302,
                                     1309,1372,
                                     1371,1380,1405)

pisa2018<-pisa2018[complete.cases(pisa2018),]

sd(pisa2018$Gini)

pisa2018 <- pisa2018 %>%
  arrange(CNTSCHID)

brr.2018 <- BIFIE.data.jack(data=pisa2018, wgt="std.weight.final",
                       jktype="RW_PISA",
                       pvpre =paste0("PV",1:10),
                       fayfac=.05,
                       wgtrep=paste0("W_FSTURWT",1:80),
                       cdata=F)
gc()

rm(pisa2018)
rm(ineq.school)
rm(ineq.country)
rm(adjusting.weights)

#

gc()

fit1.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                          dep="MATH",
                                          formula.fixed=~ Sex + HISCED +
                                            IMMIG + Language +
                                            AGE + REPEAT  +
                                            Tipo.escola +
                                            Area + CNT +
                                            HOMEPOS + school.HOMEPOS,
                                          formula.random=~ 1,
                                          idcluster="CNTSCHID",
                                          wgtlevel1="std.weight.final",
                                          wgtlevel2 = "one")

fit2.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                          dep="MATH",
                                          formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                            AGE + REPEAT  + Tipo.escola + Area + CNT +
                                            HOMEPOS + school.HOMEPOS + Gini,
                                          formula.random=~ 1,
                                          idcluster="CNTSCHID",
                                          wgtlevel1="std.weight.final",
                                          wgtlevel2 = "one")
gc()

fit3.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                          dep="MATH",
                                          formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                            AGE + REPEAT  + Tipo.escola + Area + CNT +
                                            HOMEPOS + school.HOMEPOS* Gini,
                                          formula.random=~ 1,
                                          idcluster="CNTSCHID",
                                          wgtlevel1="std.weight.final",
                                          wgtlevel2 = "one")

gc()

fit4.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                          dep="READ",
                                          formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                            AGE + REPEAT  + Tipo.escola + Area + CNT +
                                            HOMEPOS+ school.HOMEPOS,
                                          formula.random=~ 1,
                                          idcluster="CNTSCHID",
                                          wgtlevel1="std.weight.final",
                                          wgtlevel2 = "one")

gc()

fit5.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                          dep="READ",
                                          formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                            AGE + REPEAT  + Tipo.escola + Area + CNT +
                                            HOMEPOS + school.HOMEPOS+ Gini,
                                          formula.random=~ 1,
                                          idcluster="CNTSCHID",
                                          wgtlevel1="std.weight.final",
                                          wgtlevel2 = "one")

gc()

fit6.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                          dep="READ",
                                          formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                            AGE + REPEAT  + Tipo.escola + Area + CNT +
                                            HOMEPOS + school.HOMEPOS* Gini,
                                          formula.random=~ 1,
                                          idcluster="CNTSCHID",
                                          wgtlevel1="std.weight.final",
                                          wgtlevel2 = "one")

gc()

fit1.18$Npers
fit2.18$Npers
fit3.18$Npers
fit4.18$Npers
fit5.18$Npers
fit6.18$Npers

write.csv(as.data.frame(summary(fit1.18)),"fit1.18.csv")
write.csv(as.data.frame(summary(fit2.18)),"fit2.18.csv")
write.csv(as.data.frame(summary(fit3.18)),"fit3.18.csv")
write.csv(as.data.frame(summary(fit4.18)),"fit4.18.csv")
write.csv(as.data.frame(summary(fit5.18)),"fit5.18.csv")
write.csv(as.data.frame(summary(fit6.18)),"fit6.18.csv")

rm(fit1.18)
rm(fit2.18)
rm(fit3.18)
rm(fit4.18)
rm(fit5.18)
rm(fit6.18)

##
fit.2.lmer.18<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area+ CNT +
                            Gini+ school.HOMEPOS + HOMEPOS  + (1|CNTSCHID),
                          data= pisa2018, weights = std.weight.final)


fit.3.lmer.18<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola +
                            Area + CNT +
                            Gini*school.HOMEPOS + HOMEPOS  + (1|CNTSCHID),
                          data= pisa2018, weights = std.weight.final)

summary(fit.3.lmer.18)

library(marginaleffects)


me<-marginaleffects(fit.3.lmer.18)





sjPlot::plot_model(fit.3.lmer.18,type = "int",
           mdrt.values = "meansd",colors = "bw")

library(effects)

ef1 <- effect(term="Gini",fit.2.lmer.18)

efdata1 <- as.data.frame(ef1) #convert the effects list to a data frame

efdata1


p0<-ggplot(data=efdata1, aes(x=Gini, y=fit)) +
  geom_line()+
  #geom_ribbon(aes(ymin=fit-se,ymax=fit+se),alpha=0.1) +
  theme_bw () + theme(panel.border = element_blank()) +
  ylim(180, 550) + xlab("School Gini")+ ylab("Math Score")+
  scale_x_continuous(breaks = c(-3, 19.5),
                     labels = c("sample min", "sample max"))

library(interactions)

quantile(pisa2018$school.HOMEPOS,
         prob = seq(0, 1, length = 11), type = 5)

p1<-interact_plot(fit.3.lmer.18,
              pred = Gini,  se=F,
              modx = school.HOMEPOS,
              modx.values = c(-1.39909235,0.13692554,
                              1.14333655),
              y.label = "Math score",
              x.label = "School Gini",
              modx.labels = c("2nd decile","Median",
                              "9th decile"),
              legend.main = "School wealth",
              colors = "Rainbow",
              robust = T)+ylim(180, 550)+
  theme_bw()+
  theme(panel.border = element_blank()) +
  scale_x_continuous(breaks = c(-3, 17),
                     labels = c("sample min", "sample max"))


?interact_plot

library(ggpubr)

ggarrange(p0,p1,
          legend="bottom",
          common.legend = T)

ggarra


?interact_plot

summary(pisa2018$school.HOMEPOS)


summary(fit.3.lmer.18)
install.packages("margins")
library(margins)

memory.limit(size=30000)
gc()
m<-margins(fit.2.lmer.18,
        at = list(Gini = c(-3.40,17.58)))

summary(pisa2018$Gini)


margins(fit.2.lmer.18, at = list(Gini = 0:1))


# 2018 Using Ineq.Alpha, Atkinson and Theil --------------------------------------------------------


gc()

fit2.ineq.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                               dep="MATH",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS + ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

gc()

write.csv(as.data.frame(summary(fit2.ineq.18)),"fit2.ineq.18.csv")

gc()

fit3.ineq.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                               dep="MATH",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS * ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

memory.limit()

memory.limit(size=30000)

write.csv(as.data.frame(summary(fit3.ineq.18)),"fit3.ineq.18.csv")



#

fit5.ineq.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                               dep="READ",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS + ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

gc()

fit6.ineq.18<- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                               dep="READ",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT+
                                                 Alpha.Wealth  + school.HOMEPOS * ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

fit2.ineq.18$Npers
fit3.ineq.18$Npers
fit5.ineq.18$Npers
fit6.ineq.18$Npers


write.csv(as.data.frame(summary(fit5.ineq.18)),"fit5.ineq.18.csv")

write.csv(as.data.frame(summary(fit6.ineq.18)),"fit6.ineq.18.csv")

#
gc()

fit.2.lmer.18<-lme4::lmer(PV1MATH ~Sex + as.numeric(HISCED) +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola + as.numeric(Area) + CNT +
                            ineq.alpha + school.HOMEPOS + HOMEPOS  + (1|CNTSCHID),
                          data= pisa2018, weights = std.weight.final)

summary(fit.2.lmer.18)

fit.3.lmer.18<-lme4::lmer(PV1MATH ~ Sex + as.numeric(HISCED) +IMMIG + Language +
                            AGE + REPEAT  + Tipo.escola + as.numeric(Area) + CNT +
                            ineq.alpha* school.HOMEPOS + HOMEPOS  + (1|CNTSCHID),
                          data= pisa2018, weights = std.weight.final)

summary(fit.3.lmer.18)

library(stargazer)

stargazer(fit.2.lmer.18,fit.3.lmer.18,
          type="text",
          star.cutoffs=c(.05,.01,.001))

sjPlot::plot_model(fit.3.lmer.18,type = "int",mdrt.values = "meansd")

##

# Theil

fit2.theil.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                                dep="MATH",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS + Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")

gc()

fit3.theil.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                                dep="MATH",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS * Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")



write.csv(as.data.frame(summary(fit2.theil.18)),"fit2.theil.18.csv")

write.csv(as.data.frame(summary(fit3.theil.18)),"fit3.theil.18.csv")

##

fit5.theil.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                                dep="READ",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS + Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")

gc()

fit6.theil.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                                dep="READ",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS * Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")



write.csv(as.data.frame(summary(fit5.theil.18)),"fit5.theil.18.csv")
write.csv(as.data.frame(summary(fit6.theil.18)),"fit6.theil.18.csv")

fit2.theil.18$Npers
fit3.theil.18$Npers
fit5.theil.18$Npers
fit6.theil.18$Npers

# Atkinson

gc()

fit2.atk.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                              dep="MATH",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS + Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")

gc()

fit3.atk.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                              dep="MATH",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS * Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")



write.csv(as.data.frame(summary(fit2.atk.18)),"fit2.atk.18.csv")
write.csv(as.data.frame(summary(fit3.atk.18)),"fit3.atk.18.csv")

#

fit5.atk.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                              dep="READ",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS + Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")

gc()

fit6.atk.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018,
                                              dep="READ",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS * Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")



write.csv(as.data.frame(summary(fit5.atk.18)),"fit5.atk.18.csv")
write.csv(as.data.frame(summary(fit6.atk.18)),"fit6.atk.18.csv")


fit2.atk.18$Npers
fit3.atk.18$Npers
fit5.atk.18$Npers
fit6.atk.18$Npers



gc()

# Pisa 2015 ---------------------------------------------------------------

pisa2015<-readRDS("C:/Users/LUCAS/Desktop/PISA INEQUALITY/From UEA Chapter 0 PISA/Modelos finales PISA/pisa.c.s.RDS")

adjusting.weights <- pisa2015 %>%
  mutate(squared.weight=Student.Weight*Student.Weight) %>%
  group_by(CNTSCHID) %>%
  summarise (
    school.size = n(),
    one=sum(Student.Weight),
    sum.sqr=sum(squared.weight),
    adjust.weight = school.size/one,
    sum.squared.weight=one/sum.sqr)

gc()

pisa2015 <- pisa2015 %>% left_join(adjusting.weights,by="CNTSCHID") %>%
  mutate(std.weight.final = Student.Weight*adjust.weight,
         std.weight.final2 = Student.Weight*sum.squared.weight)


#

ineq.school <- pisa2015 %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2015 %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

pisa2015<-pisa2015 %>% left_join (ineq.school[,c(2,5)])

#saveRDS(pisa2015,"pisa2015.RDS")

variable.names(pisa2015)[1:1000]

variable.names(pisa2015)[1001:1285]

pisa2015<-pisa2015 %>% dplyr::select(1,2,29,69,73,641,660,
                                     663,669,723,
                                     727:807,810:839,
                                     931,
                                     1043,1160,1161,
                                     1190,
                                     1246,1249,1257,
                                     1258,1260,1277,
                                     1278:1285)


pisa2015 <- pisa2015 %>%
  arrange(CNTSCHID)

gc()

####


variable.names(pisa2015)

brr.2015 <- BIFIE.data.jack(data=pisa2015, wgt="std.weight.final",
                                               jktype="RW_PISA",
                                               pvpre =paste0("PV",1:10),
                                               fayfac=.05,
                                               wgtrep=paste0("W_FSTURWT",1:80),
                                               cdata=F)

rm(pisa2015)

gc()

fit1.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                          dep="MATH",
                           formula.fixed=~ Sex + HISCED +IMMIG + Language +
                             AGE + REPEAT  + Tipo.escola + Area + CNT +
                             HOMEPOS + school.HOMEPOS,
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one")

gc()

summary (fit1.15)

fit2.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS + Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit2.15)

fit3.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS* Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")



summary(fit3.15)

###

gc()



fit4.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                       dep="READ",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS+school.HOMEPOS,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit4.15)

fit5.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                       dep="READ",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS + Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit5.15)

fit6.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                       dep="READ",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS * Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")


summary(fit6.15)


# 2015 Using Ineq.Alpha, Atkinson and Theil --------------------------------------------------------

fit2.ineq.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                               dep="MATH",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS + ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")


gc()

summary(fit2.ineq.15)

fit3.ineq.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                               dep="MATH",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS * ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")


summary(fit3.ineq.15)

write.csv(as.data.frame(summary(fit2.ineq.15)),"fit2.ineq.15.csv")
write.csv(as.data.frame(summary(fit3.ineq.15)),"fit3.ineq.15.csv")



#

fit5.ineq.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                               dep="READ",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS + ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

gc()

summary(fit5.ineq.15)

fit6.ineq.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                               dep="READ",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS * ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")



summary(fit6.ineq.15)

write.csv(as.data.frame(summary(fit5.ineq.15)),"fit5.ineq.15.csv")
write.csv(as.data.frame(summary(fit6.ineq.15)),"fit6.ineq.15.csv")




# Theil

fit2.theil.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                                dep="MATH",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS + Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")

gc()

summary(fit2.theil.15)

fit3.theil.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                                dep="MATH",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS * Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")


summary(fit3.theil.15)

write.csv(as.data.frame(summary(fit2.theil.15)),"fit2.theil.15.csv")
write.csv(as.data.frame(summary(fit3.theil.15)),"fit3.theil.15.csv")

##

fit5.theil.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                                dep="READ",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS + Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")

gc()

summary(fit5.theil.15)

fit6.theil.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                                dep="READ",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS * Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")



summary(fit6.theil.15)

write.csv(as.data.frame(summary(fit5.theil.15)),"fit5.theil.15.csv")

write.csv(as.data.frame(summary(fit6.theil.15)),"fit6.theil.15.csv")


# Atkinson

fit2.atk.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                              dep="MATH",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS + Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")

gc()

summary(fit2.atk.15)

fit3.atk.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                              dep="MATH",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS * Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")



summary(fit3.atk.15)

write.csv(as.data.frame(summary(fit2.atk.15)),"fit2.atk.15.csv")
write.csv(as.data.frame(summary(fit3.atk.15)),"fit3.atk.15.csv")


fit5.atk.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                              dep="READ",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS + Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")

gc()

summary(fit5.atk.15)

fit6.atk.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015,
                                              dep="READ",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS * Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")



summary(fit6.atk.15)

write.csv(as.data.frame(summary(fit5.atk.15)),"fit5.atk.15.csv")
write.csv(as.data.frame(summary(fit6.atk.15)),"fit6.atk.15.csv")


fit2.15$Npers
fit3.15$Npers
fit2.ineq.15$Npers
fit3.ineq.15$Npers
fit2.theil.15$Npers
fit3.theil.15$Npers
fit2.atk.15$Npers
fit3.atk.15$Npers

fit5.15$Npers
fit6.15$Npers
fit5.ineq.15$Npers
fit6.ineq.15$Npers
fit5.theil.15$Npers
fit6.theil.15$Npers
fit5.atk.15$Npers
fit6.atk.15$Npers



write.csv(as.data.frame(summary(fit1.15)),"fit1.15.csv")
write.csv(as.data.frame(summary(fit2.15)),"fit2.15.csv")
write.csv(as.data.frame(summary(fit3.15)),"fit3.15.csv")
write.csv(as.data.frame(summary(fit4.15)),"fit4.15.csv")
write.csv(as.data.frame(summary(fit5.15)),"fit5.15.csv")
write.csv(as.data.frame(summary(fit6.15)),"fit6.15.csv")

# Pisa 2012 ---------------------------------------------------------------

pisa2012<-readRDS("C:/Users/LUCAS/Desktop/PISA INEQUALITY/PISA2012/pisa.c.s.RDS")

adjusting.weights <- pisa2012 %>%
  mutate(squared.weight=Student.Weight*Student.Weight) %>%
  group_by(CNTSCHID) %>%
  summarise (
    school.size = n(),
    one=sum(Student.Weight),
    sum.sqr=sum(squared.weight),
    adjust.weight = school.size/one,
    sum.squared.weight=one/sum.sqr)

gc()

pisa2012 <- pisa2012 %>% left_join(adjusting.weights,by="CNTSCHID") %>%
  mutate(std.weight.final = Student.Weight*adjust.weight,
         std.weight.final2 = Student.Weight*sum.squared.weight)


ineq.school <- pisa2012 %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2012 %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

pisa2012<-pisa2012 %>% left_join (ineq.school[,c(2,5)])

#saveRDS(pisa2012,"pisa2012.RDS")


##

variable.names(pisa2012)[1:1000]

variable.names(pisa2012)[1001:1013]

pisa2012<-pisa2012 %>% dplyr::select(1,2,13,44,412,440,442,
                                     449,475,501:505,541:545,
                                     551:631,640,645,
                                     911,925,
                                     972,974,982,983,985,1005:1013)

#HOMEPOS.OLD

pisa2012 <- pisa2012 %>%
  arrange(CNTSCHID)


gc()

brr.2012 <- BIFIE.data.jack(data=pisa2012, wgt="std.weight.final",
                       jktype="RW_PISA",
                       pvpre =paste0("PV",1:5),
                       fayfac=.05,
                       wgtrep=paste0("W_FSTR",1:80),
                       cdata=F)


rm(pisa2012)

gc()

fit1.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +
                                         IMMIG + Language +
                                         AGE + REPEAT  +
                                         Tipo.escola +
                                         Area + CNT +
                                         HOMEPOS + school.HOMEPOS,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit1.12)

fit2.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS + Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit2.12)

fit3.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS * Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")



summary(fit3.12)

###

gc()



fit4.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                       dep="READ",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS+ school.HOMEPOS ,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit4.12)

fit5.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                       dep="READ",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS + Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit5.12)

fit6.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                       dep="READ",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS+ school.HOMEPOS  * Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")



summary(fit6.12)

##
gc()

fit7.12<-lme4::lmer(PV1MATH ~ Sex + HISCED +IMMIG + Language +
                   AGE + REPEAT  + Tipo.escola + Area + CNT +
                   Gini* school.HOMEPOS + HOMEPOS  + (1|CNTSCHID), data= pisa2012,
                   weights = std.weight.final)

summary(fit7.12)

#sjPlot::plot_model(fit7.12, type = "int",   mdrt.values = "meansd")


gc()




gc()


write.csv(as.data.frame(summary(fit1.12)),"fit1.12.csv")
write.csv(as.data.frame(summary(fit2.12)),"fit2.12.csv")
write.csv(as.data.frame(summary(fit3.12)),"fit3.12.csv")
write.csv(as.data.frame(summary(fit4.12)),"fit4.12.csv")
write.csv(as.data.frame(summary(fit5.12)),"fit5.12.csv")
write.csv(as.data.frame(summary(fit6.12)),"fit6.12.csv")


stargazer::stargazer(fit7.18,fit7.15,fit7.12,type = "text")







# 2012 Using Ineq.Alpha, Atkinson and Theil --------------------------------------------------------


fit2.ineq.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                               dep="MATH",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS + ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

gc()

summary(fit2.ineq.12)

fit3.ineq.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                               dep="MATH",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS * ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")



summary(fit3.ineq.12)

write.csv(as.data.frame(summary(fit2.ineq.12)),"fit2.ineq.12.csv")
write.csv(as.data.frame(summary(fit3.ineq.12)),"fit3.ineq.12.csv")



#

fit5.ineq.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                               dep="READ",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS + ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")

gc()

summary(fit5.ineq.12)

fit6.ineq.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                               dep="READ",
                                               formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                 AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                 HOMEPOS + school.HOMEPOS * ineq.alpha,
                                               formula.random=~ 1,
                                               idcluster="CNTSCHID",
                                               wgtlevel1="std.weight.final",
                                               wgtlevel2 = "one")



summary(fit6.ineq.12)

write.csv(as.data.frame(summary(fit5.ineq.12)),"fit5.ineq.12.csv")
write.csv(as.data.frame(summary(fit6.ineq.12)),"fit6.ineq.12.csv")




# Theil

fit2.theil.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                                dep="MATH",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS + Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")

gc()

summary(fit2.theil.12)

fit3.theil.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                                dep="MATH",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS * Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")



summary(fit3.theil.12)

write.csv(as.data.frame(summary(fit2.theil.12)),"fit2.theil.12.csv")
write.csv(as.data.frame(summary(fit3.theil.12)),"fit3.theil.12.csv")

##

fit5.theil.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                                dep="READ",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS + Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")

gc()

summary(fit5.theil.12)

fit6.theil.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                                dep="READ",
                                                formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                  AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                  HOMEPOS + school.HOMEPOS * Theil.0,
                                                formula.random=~ 1,
                                                idcluster="CNTSCHID",
                                                wgtlevel1="std.weight.final",
                                                wgtlevel2 = "one")



summary(fit6.theil.12)

write.csv(as.data.frame(summary(fit5.theil.12)),"fit5.theil.12.csv")

write.csv(as.data.frame(summary(fit6.theil.12)),"fit6.theil.12.csv")


# Atkinson

fit2.atk.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                              dep="MATH",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS + Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")

gc()

summary(fit2.atk.12)

fit3.atk.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                              dep="MATH",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS * Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")



summary(fit3.atk.12)

write.csv(as.data.frame(summary(fit2.atk.12)),"fit2.atk.12.csv")
write.csv(as.data.frame(summary(fit3.atk.12)),"fit3.atk.12.csv")

#

fit5.atk.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                              dep="READ",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS + Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")

gc()

summary(fit5.atk.12)

fit6.atk.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012,
                                              dep="READ",
                                              formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                HOMEPOS + school.HOMEPOS * Atkinson.2,
                                              formula.random=~ 1,
                                              idcluster="CNTSCHID",
                                              wgtlevel1="std.weight.final",
                                              wgtlevel2 = "one")



summary(fit6.atk.12)

write.csv(as.data.frame(summary(fit5.atk.12)),"fit5.atk.12.csv")
write.csv(as.data.frame(summary(fit6.atk.12)),"fit6.atk.12.csv")


gc()

fit2.12$Npers
fit3.12$Npers
fit2.ineq.12$Npers
fit3.ineq.12$Npers
fit2.theil.12$Npers
fit3.theil.12$Npers
fit2.atk.12$Npers
fit3.atk.12$Npers

fit5.12$Npers
fit6.12$Npers
fit5.ineq.12$Npers
fit6.ineq.12$Npers
fit5.theil.12$Npers
fit6.theil.12$Npers
fit5.atk.12$Npers
fit6.atk.12$Npers



# PISA 2018 school largen than median  ----------------------------


pisa2018<-readRDS("C:/Users/LUCAS/Desktop/PISA INEQUALITY/PISA 2018/pisa.c.s.RDS")

summary(pisa2018$SCHSIZE) # median
summary(pisa2018$CLSIZE)

pisa2018.size <- pisa2018 %>%
  filter (SCHSIZE>median(SCHSIZE,na.rm=T)) #  # CHECK values obs and median

rm(pisa2018)

adjusting.weights <- pisa2018.size %>%
  mutate(squared.weight=Student.Weight*Student.Weight) %>%
  group_by(CNTSCHID) %>%
  summarise (
    school.size = n(),
    one=sum(Student.Weight),
    sum.sqr=sum(squared.weight),
    adjust.weight = school.size/one,
    sum.squared.weight=one/sum.sqr)

gc()

pisa2018.size <- pisa2018.size %>% left_join(adjusting.weights,by="CNTSCHID") %>%
  mutate(std.weight.final = Student.Weight*adjust.weight,
         std.weight.final2 = Student.Weight*sum.squared.weight)

#
ineq.school <- pisa2018.size %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2018.size %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT


pisa2018.size<-pisa2018.size %>% left_join (ineq.school[,c(2,5)])

#saveRDS(pisa2018,"pisa2018.RDS")

pisa2018.size<-pisa2018.size %>% dplyr::select(1,2,18,63,835,846,860,862,878,
                                     945:1024,1027:1046,
                                     1128,1129,
                                     1294,1302,
                                     1309,1372,1365,
                                     1371,1377,
                                     1378,
                                     1379,
                                     1380,1382,1399:1407)


pisa2018.size <- pisa2018.size %>%
  arrange(CNTSCHID)

gc()

#

brr.2018.med <- BIFIE.data.jack(data=pisa2018.size, wgt="std.weight.final2",
                       jktype="RW_PISA",
                       pvpre =paste0("PV",1:10),
                       fayfac=.05,
                       wgtrep=paste0("W_FSTURWT",1:80),
                       cdata=F)

rm(pisa2018.size)

gc()

fit1.median.school.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018.med,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary (fit1.median.school.18)

fit2.median.school.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018.med,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS + Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit2.median.school.18)

fit3.median.school.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018.med,
                                       dep="MATH",
                                       formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                         AGE + REPEAT  + Tipo.escola + Area + CNT +
                                         HOMEPOS + school.HOMEPOS* Gini,
                                       formula.random=~ 1,
                                       idcluster="CNTSCHID",
                                       wgtlevel1="std.weight.final",
                                       wgtlevel2 = "one")

gc()

summary(fit3.median.school.18)

write.csv(as.data.frame(summary(fit1.median.school.18)),"fit1.median.school.18.csv")
write.csv(as.data.frame(summary(fit2.median.school.18)),"fit2.median.school.18.csv")
write.csv(as.data.frame(summary(fit3.median.school.18)),"fit3.median.school.18.csv")

###

fit4.median.school.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018.med,
                                                     dep="READ",
                                                     formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                       AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                       HOMEPOS + school.HOMEPOS,
                                                     formula.random=~ 1,
                                                     idcluster="CNTSCHID",
                                                     wgtlevel1="std.weight.final",
                                                     wgtlevel2 = "one")

gc()

summary (fit4.median.school.18)

fit5.median.school.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018.med,
                                                     dep="READ",
                                                     formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                       AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                       HOMEPOS + school.HOMEPOS + Gini,
                                                     formula.random=~ 1,
                                                     idcluster="CNTSCHID",
                                                     wgtlevel1="std.weight.final",
                                                     wgtlevel2 = "one")

gc()

summary(fit5.median.school.18)

fit6.median.school.18 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2018.med,
                                                     dep="READ",
                                                     formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                       AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                       HOMEPOS + school.HOMEPOS* Gini,
                                                     formula.random=~ 1,
                                                     idcluster="CNTSCHID",
                                                     wgtlevel1="std.weight.final",
                                                     wgtlevel2 = "one")


gc()

summary(fit6.median.school.18)

write.csv(as.data.frame(summary(fit4.median.school.18)),"fit4.median.school.18.csv")
write.csv(as.data.frame(summary(fit5.median.school.18)),"fit5.median.school.18.csv")
write.csv(as.data.frame(summary(fit6.median.school.18)),"fit6.median.school.18.csv")


fit2.median.school.18$Npers
fit3.median.school.18$Npers
fit5.median.school.18$Npers
fit6.median.school.18$Npers


# PISA 2015 school larger than median  ----------------------------


pisa2015<-readRDS("C:/Users/LUCAS/Desktop/PISA INEQUALITY/From UEA Chapter 0 PISA/Modelos finales PISA/pisa.c.s.RDS")


summary(pisa2015$SCHSIZE) # median
summary(pisa2015$CLSIZE)

pisa2015.size <- pisa2015 %>%
  filter (SCHSIZE>median(SCHSIZE,na.rm=T)) #  # CHECK values

adjusting.weights <- pisa2015.size %>%
  mutate(squared.weight=Student.Weight*Student.Weight) %>%
  group_by(CNTSCHID) %>%
  summarise (
    school.size = n(),
    one=sum(Student.Weight),
    sum.sqr=sum(squared.weight),
    adjust.weight = school.size/one,
    sum.squared.weight=one/sum.sqr)

gc()

pisa2015.size <- pisa2015.size %>% left_join(adjusting.weights,by="CNTSCHID") %>%
  mutate(std.weight.final = Student.Weight*adjust.weight,
         std.weight.final2 = Student.Weight*sum.squared.weight)


#

ineq.school <- pisa2015.size %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2015.size %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

pisa2015.size<-pisa2015.size %>% left_join (ineq.school[,c(2,5)])

#saveRDS(pisa2015,"pisa2015.RDS")

variable.names(pisa2015.size)[1:1000]

variable.names(pisa2015.size)[1001:1285]

pisa2015.size<-pisa2015.size %>% dplyr::select(1,2,29,69,73,641,660,
                                     663,669,723,
                                     727:807,810:839,
                                     931,
                                     1043,1160,1161,
                                     1190,
                                     1246,1249,1257,
                                     1258,1260,1277,
                                     1278:1285)


pisa2015.size <- pisa2015.size %>%
  arrange(CNTSCHID)

gc()

rm(pisa2015)
###

gc()

brr.2015.med <- BIFIE.data.jack(data=pisa2015.size, wgt="std.weight.final2",
                       jktype="RW_PISA",
                       pvpre =paste0("PV",1:10),
                       fayfac=.05,
                       wgtrep=paste0("W_FSTURWT",1:80),
                       cdata=F)

rm(pisa2015.size)

gc()

fit1.median.school.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015.med,
                                                        dep="MATH",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary (fit1.median.school.15)

fit2.median.school.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015.med,
                                                        dep="MATH",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS + Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary(fit2.median.school.15)

fit3.median.school.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015.med,
                                                        dep="MATH",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS* Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")


gc()
summary(fit3.median.school.15)

write.csv(as.data.frame(summary(fit1.median.school.15)),"fit1.median.school.15.csv")
write.csv(as.data.frame(summary(fit2.median.school.15)),"fit2.median.school.15.csv")
write.csv(as.data.frame(summary(fit3.median.school.15)),"fit3.median.school.15.csv")

###


fit4.median.school.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015.med,
                                                        dep="READ",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary (fit4.median.school.15)

fit5.median.school.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015.med,
                                                        dep="READ",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS + Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary(fit5.median.school)

fit6.median.school.15 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2015.med,
                                                        dep="READ",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS* Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")


gc()
summary(fit6.median.school.15)

write.csv(as.data.frame(summary(fit4.median.school.15)),"fit4.median.school.15.csv")
write.csv(as.data.frame(summary(fit5.median.school.15)),"fit5.median.school.15.csv")
write.csv(as.data.frame(summary(fit6.median.school.15)),"fit6.median.school.15.csv")


# PISA 2012 school larger than median ----------------------------

pisa2012<-readRDS("C:/Users/LUCAS/Desktop/PISA INEQUALITY/PISA2012/pisa.c.s.RDS")

pisa2012.size <- pisa2012 %>%
  filter (SCHSIZE>median(SCHSIZE,na.rm=T)) # CHECK values

rm(pisa2012)

adjusting.weights <- pisa2012.size %>%
  mutate(squared.weight=Student.Weight*Student.Weight) %>%
  group_by(CNTSCHID) %>%
  summarise (
    school.size = n(),
    one=sum(Student.Weight),
    sum.sqr=sum(squared.weight),
    adjust.weight = school.size/one,
    sum.squared.weight=one/sum.sqr)

gc()

pisa2012.size <- pisa2012.size %>% left_join(adjusting.weights,by="CNTSCHID") %>%
  mutate(std.weight.final = Student.Weight*adjust.weight,
         std.weight.final2 = Student.Weight*sum.squared.weight)


ineq.school <- pisa2012.size %>%
  group_by(CNTSCHID,CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.school=sd(Alpha.Wealth,na.rm = T))

ineq.country <- pisa2012.size %>%
  group_by(CNT) %>%
  dplyr::summarise(
    sd.Alpha.Wealth.CNT=sd(Alpha.Wealth,na.rm = T))

ineq.school<-merge(ineq.school,ineq.country,by="CNT")

ineq.school$ineq.alpha <- ineq.school$sd.Alpha.Wealth.school/
  ineq.school$sd.Alpha.Wealth.CNT

pisa2012.size<-pisa2012.size %>% left_join (ineq.school[,c(2,5)])

#saveRDS(pisa2012,"pisa2012.RDS")


##

variable.names(pisa2012.size)[1:1000]

variable.names(pisa2012.size)[1001:1013]

pisa2012.size<-pisa2012.size %>% dplyr::select(1,2,13,44,412,440,442,
                                     449,475,501:505,541:545,
                                     551:631,640,645,
                                     911,925,
                                     972,974,982,983,985,1005:1013)

#HOMEPOS.OLD

pisa2012.size <- pisa2012.size %>%
  arrange(CNTSCHID)


###

variable.names(pisa2012.size)

brr.2012.med <- BIFIE.data.jack(data=pisa2012.size, wgt="std.weight.final2",
                       jktype="RW_PISA",
                       pvpre =paste0("PV",1:5),
                       fayfac=.05,
                       wgtrep=paste0("W_FSTR",1:80),
                       cdata=F)


rm(pisa2012.size)

gc()

fit1.median.school.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012.med,
                                                        dep="MATH",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()


summary (fit1.median.school.12)


fit2.median.school.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012.med,
                                                        dep="MATH",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS + Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary(fit2.median.school.12)

fit3.median.school.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012.med,
                                                        dep="MATH",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS* Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")


gc()

summary(fit3.median.school.12)

write.csv(as.data.frame(summary(fit1.median.school.12)),"fit1.median.school.12.csv")
write.csv(as.data.frame(summary(fit2.median.school.12)),"fit2.median.school.12.csv")
write.csv(as.data.frame(summary(fit3.median.school.12)),"fit3.median.school.12.csv")

###


fit4.median.school.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012.med,
                                                        dep="READ",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary (fit4.median.school.12)

fit5.median.school.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012.med,
                                                        dep="READ",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS + Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")

gc()

summary(fit5.median.school.12)

fit6.median.school.12 <- BIFIEsurvey::BIFIE.twolevelreg(BIFIEobj=brr.2012.med,
                                                        dep="READ",
                                                        formula.fixed=~ Sex + HISCED +IMMIG + Language +
                                                          AGE + REPEAT  + Tipo.escola + Area + CNT +
                                                          HOMEPOS + school.HOMEPOS* Gini,
                                                        formula.random=~ 1,
                                                        idcluster="CNTSCHID",
                                                        wgtlevel1="std.weight.final",
                                                        wgtlevel2 = "one")


gc()
summary(fit6.median.school.12)

write.csv(as.data.frame(summary(fit4.median.school.12)),"fit4.median.school.12.csv")
write.csv(as.data.frame(summary(fit5.median.school.12)),"fit5.median.school.12.csv")
write.csv(as.data.frame(summary(fit6.median.school.12)),"fit6.median.school.12.csv")


#####

fit.ineq.18<-lme4::lmer(PV5MATH ~ Sex + HISCED +IMMIG + Language +
                      AGE + REPEAT  + Tipo.escola + Area + CNT +
                      ineq.alpha* school.HOMEPOS + HOMEPOS  + (1|CNTSCHID),
                      data= pisa2018, weights = Student.Weight)

summary(fit.ineq.18)

cor(pisa2012$std.weight.final,
    pisa2012$Student.Weight)

####

cor(pisa2018$PV1MATH,pisa2018$CLSIZE,use="pairwise")
cor(pisa2018$Gini,pisa2018$CLSIZE,use="pairwise")
cor(pisa2018$Atkinson.2,pisa2018$CLSIZE,use="pairwise")
cor(pisa2018$ineq.alpha,pisa2018$CLSIZE,use="pairwise")
cor(pisa2018$Theil.0,pisa2018$CLSIZE,use="pairwise")

cor(pisa2012$PV5MATH,pisa2012$SCHSIZE)
cor(pisa2012$Gini,pisa2012$SCHSIZE)
cor(pisa2012$Atkinson.2,pisa2012$SCHSIZE)
cor(pisa2012$ineq.alpha,pisa2012$SCHSIZE)
cor(pisa2012$Theil.0,pisa2012$SCHSIZE,use="pairwise")

####


