memory.limit(size=30000)

library(ggpmisc)
library(matrixStats)
library(forcats)
library(broom)
library(lavaan)
library(tidyverse)

# Setup -------------------------------------------------------------------
gc()

pisa2018 <- readRDS("pisa.c.s.RDS")

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

variable.names (pisa2018)[1:1000]

variable.names (pisa2018)[1001:1407]

pisa2018 <- pisa2018 %>% dplyr::select(1,3,18,
                                     835,862,878,
                                     #895,
                                     896,903,
                                     #908,
                                     909,911,913,
                                     915,#916,
                                     1027:1046,
                                     1129,1130,
                                     1295,1303,
                                     1310,
                                     1371,
                                     1380,1405,
                                     1407)

sapply(pisa2018, function(x) sum(is.na(x)))


# Factors ---------------------------------------------------------------------
table(pisa2018$Tipo.escola)

pisa2018$Tipo.escola <-as.numeric(pisa2018$Tipo.escola)#Public Private

table(pisa2018$Area)

pisa2018$Area <-as.numeric(pisa2018$Area) #<3k   3k>15k 15k>100k  100k>1m      >1m

table(pisa2018$REPEAT)

pisa2018$REPEAT <-as.numeric(pisa2018$REPEAT)# NO YES

table(pisa2018$Sex)


pisa2018$Sex <-as.numeric(pisa2018$Sex) #Female   Male


pisa2018$Tipo.escola[pisa2018$Tipo.escola==1]<-0
pisa2018$Tipo.escola[pisa2018$Tipo.escola==2]<-1
pisa2018$Sex[pisa2018$Sex==1]<-0
pisa2018$Sex[pisa2018$Sex==2]<-1


##

countries<-c("ARG",
"BRA",
"CHL",
"COL",
"CRI",
"DOM",
"MEX",
"PAN",
"PER",
"PRT",
"URY")


#LA.notsubsample <- pisa2018 %>% filter(CNT %in% countries)

variable.names(pisa2018)

LA <- pisa2018 #%>% filter(CNT %in% countries) # test if it works with all dataset

rm(pisa2018)

ineq.country3 <- ineq.school %>%  group_by(CNT) %>%
  dplyr::summarise(mean.A=mean(ineq.alpha,na.rm = T)) # segregation

LA<-merge(LA,ineq.country3,by="CNT")


LA$mean.A<- (LA$mean.A*-1)+1


#

variable.names(LA)


## Ineq.alpha -------------------------------------------------------------------



LA.pv <- LA %>% pivot_longer(cols=PV1READ:PV10READ,
                             names_to = "READ",
                             values_to = "score")

#rm(LA)

##

# list: 1 matrix per group
correlate <- LA %>% select(CNT,BELONG, RESILIENCE, PERCOOP, PERSPECT,
                           RESPECT,GLOBMIND) %>%
  split(.$CNT) %>%
  map(select, -c(CNT)) %>%
  map(cor,use="pairwise")

write.csv(correlate,"cora2.csv")

LA %>% select(CNT,BELONG, RESILIENCE, PERCOOP, PERSPECT,
              RESPECT,GLOBMIND) %>% group_by(CNT) %>%
  do(data.frame(Cor=t(cor(.[,2:7],.[,2],use="pairwise"))))

# Descriptive -------------------------------------------------------------------

LA %>%
  summarise(sd=weightedSd(ineq.alpha,w=std.weight.final,na.rm=T),
            mean=weighted.mean(ineq.alpha,w=std.weight.final,na.rm=T))


LA %>%
  group_by(CNT,Tipo.escola) %>%
  summarise(ineq=weightedMean(ineq.alpha,na.rm = T,w=std.weight.final)) %>%
  distinct() %>%
  filter(!is.na(Tipo.escola)) %>%
  group_by(CNT) %>%
  summarise(i = ineq-lag(ineq)) %>%
  filter (!is.na(i)) %>%
  arrange (-i) %>%
  ggplot(aes(x=fct_reorder(CNT,i),y=i)) +
  geom_point() + theme_minimal() + geom_hline(aes(yintercept=0),colour="darkorange",
                                              linetype=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "Difference in inequality",
       subtitle = "Values < 0 represents wealth inequality is larger in Public than Private schools",
       caption = "Data source: PISA 2018",
       x = "Countries",
       y = "Difference in inequality")

LA %>%
  group_by(CNT,Tipo.escola) %>%
  summarise(ineq=weightedMean(ineq.alpha,na.rm = T,w=std.weight.final),
            Gini=weightedMean(Gini,na.rm = T,w=std.weight.final),) %>%
  distinct() %>%
  filter(!is.na(Tipo.escola)) %>%
  group_by(CNT) %>% rename(`School type` = Tipo.escola) %>%
  mutate(`School type` = as.factor(`School type`)) %>%
  ggplot(aes(x=fct_reorder(CNT,ineq),y=ineq,colour=`School type`)) +
  geom_point() + theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_colour_manual( values =c("0" = "darkgreen", "1" = "darkorange"),
                       labels = c("Public", "Private")) +
  labs(title = "Alpha inequality in public and private schools",
       caption = "Data source: PISA 2018",
       x = "Countries",
       y = "Alpha inequality")

summary(LA$Gini)

LA %>%
  group_by(CNT,Tipo.escola) %>%
  summarise(ineq=weightedMean(ineq.alpha,na.rm = T,w=std.weight.final),
            Gini=weightedMean(Gini,na.rm = T,w=std.weight.final),) %>%
  distinct() %>%
  filter(!is.na(Tipo.escola)) %>%
  group_by(CNT) %>% rename(`School type` = Tipo.escola) %>%
  mutate(`School type` = as.factor(`School type`)) %>%
  ggplot(aes(x=fct_reorder(CNT,ineq),y=Gini+3.1945,colour=`School type`)) +
  geom_point() + theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_colour_manual( values =c("0" = "darkgreen", "1" = "darkorange"),
                       labels = c("Public", "Private")) +
  labs(title = "Gini in public and private schools",
       caption = "Data source: PISA 2018",
       x = "Countries",
       y = "Gini")




LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT,CNTSCHID,Tipo.escola) %>%
  summarise(ineq=weightedMean(ineq.alpha,na.rm = T,w=std.weight.final)) %>%
  distinct() %>%
  filter(!is.na(Tipo.escola)) %>% mutate(Tipo.escola = as.factor(as.character(Tipo.escola))) %>%
  ggplot(aes(x=forcats::fct_reorder(Tipo.escola,ineq, .fun=median, .desc=T),
             y=ineq,
             colour=CNT)) +
  theme_minimal() + geom_hline(aes(yintercept=0.804),linetype=2,colour="darkorange")+
  geom_hline(aes(yintercept=0.804+0.211),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=0.804-0.211),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=0.804+0.211+0.211),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=0.804-0.211-0.211),linetype=2,colour="darkgreen")+
  geom_boxplot()

LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT,CNTSCHID) %>%
  summarise(ineq=weightedMean(ineq.alpha,na.rm = T,w=std.weight.final)) %>%
  distinct() %>%
  #filter(!is.na(Area)) %>%
  ggplot() +
  theme_minimal() + geom_hline(aes(yintercept=0.804),linetype=2,colour="darkorange")+
  geom_hline(aes(yintercept=0.804+0.211),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=0.804-0.211),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=0.804+0.211+0.211),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=0.804-0.211-0.211),linetype=2,colour="darkgreen")+
  geom_boxplot(aes(as.factor(CNT),ineq)) + xlab("Countries") + ylab ("School inequality")

library(ggforce)

LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT,CNTSCHID) %>%
  summarise(ineq=weightedMean(ineq.alpha, na.rm = T, w=std.weight.final),
            READ=weightedMean(PV1READ, na.rm = T, w=std.weight.final)) %>%
  mutate (ineq.l = case_when(ineq >= 0.804+0.211  ~ "mean+sd",
                             ineq <= 0.804-0.211~ "mean-sd",
                             T ~ "mean") ) %>%
  distinct() %>%
  ggplot(aes(ineq,READ)) +
  geom_point(alpha=.7) +
  facet_grid(vars(ineq.l),vars(CNT), scales = "free") +
  theme_minimal() +
  geom_smooth(aes(ineq,READ),
              method = "lm",se=F,
              formula = 'y ~ x') +
  stat_poly_eq(formula = 'y ~ x',
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               parse = TRUE) + theme_light()



LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  mutate (ineq.l = case_when(ineq.alpha >= 0.804+0.211  ~ "mean+sd",
                             ineq.alpha <= 0.804-0.211~ "mean-sd",
                             T ~ "mean") ) %>%
  ggplot(aes(ineq.alpha,PV1READ,weight=std.weight.final)) +
  geom_point(alpha=.7,aes(colour= ineq.l)) +
  facet_wrap(~CNT, scales = "free") +
  geom_smooth(aes(ineq.alpha,PV1READ),
              method = "lm",se=F,
              formula = 'y ~ x') +
  stat_poly_eq(formula = 'y ~ x',
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               parse = TRUE) + theme_light()

#

quantile(LA$HOMEPOS, prob = seq(0, 1, length = 6), type = 5)
quantile(LA$ineq.alpha, prob = seq(0, 1, length = 6), type = 5)


LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT,CNTSCHID) %>%
  summarise(ineq=weightedMean(ineq.alpha, na.rm = T, w=std.weight.final),
            READ=weightedMean(PV1READ, na.rm = T, w=std.weight.final),
            HOMEPOS= case_when(HOMEPOS<8.03603 ~ "1st",
                               HOMEPOS>= 8.03603 & HOMEPOS <8.74761 ~ "2nd",
                               HOMEPOS>=8.74761  & HOMEPOS <9.35230~ "3rd",
                               HOMEPOS>=9.35230 & HOMEPOS < 10.02364~ "4th",
                               T ~ "5th")) %>%
  distinct() %>%
  ggplot(aes(ineq,READ)) +
  geom_point(alpha=.1) +
  facet_wrap (~HOMEPOS) +
  geom_smooth(method = "lm", se=FALSE, formula = 'y ~ poly(x,2)') +
  stat_poly_eq(formula = 'y ~ poly(x,2)',
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE)+ theme_light()

#

quantile(LA$school.HOMEPOS, prob = seq(0, 1, length = 6), type = 5)

LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT,CNTSCHID) %>%
  summarise(ineq=weightedMean(ineq.alpha, na.rm = T, w=std.weight.final),
            READ=weightedMean(PV1READ, na.rm = T, w=std.weight.final),
            school.HOMEPOS= case_when(school.HOMEPOS< -1.70792079 ~ "1st",
                                      school.HOMEPOS>= -1.70792079 & school.HOMEPOS < -1.21149380  ~ "2nd",
                                      school.HOMEPOS>= -1.21149380   & school.HOMEPOS < -0.73735200  ~ "3rd",
                                      school.HOMEPOS>= -0.73735200  & school.HOMEPOS < -0.01770877 ~ "4th",
                                      T ~ "5th")) %>% distinct() %>%
  ggplot(aes(ineq,READ)) +
  geom_point(alpha=.1) +
  facet_wrap (~school.HOMEPOS) +
  geom_smooth(method = "lm", se=FALSE, formula = 'y ~ x') +
  stat_poly_eq(formula = 'y ~ x',
               aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~")),
               parse = TRUE) + theme_light()


rm(adjusting.weights)


##


#
#
# COHESION_fa<-'
# COHESION =~ BELONG + PERCOOP + PERSPECT + GLOBMIND + RESPECT '
#
#
# cfa_COHESION0 <- cfa(model=COHESION_fa, data = LA,
#                      optim.force.converged = T,
#                      group = "CNT", estimator= "MLR",#orthogonal=T,
#                      sampling.weights ="std.weight.final")
#
# summary(cfa_COHESION0)
#
# modindices(cfa_COHESION0, sort = TRUE, maximum.number = 30)
#
# fitmeasures(cfa_COHESION0, c("chisq.scaled",  "df.scaled", "pvalue.scaled",
#                              "cfi.robust","tli.robust","rmsea.robust",
#                              "rmsea.ci.lower.robust","rmsea.ci.upper.robust",
#                              "srmr"))
#
#
#
#
# COHESION_fabi<-'
#
# COHESION =~ BELONG + PERCOOP + PERSPECT + GLOBMIND + RESPECT
# RELATIONS =~PERCOOP + PERSPECT
# COMMON =~ GLOBMIND + RESPECT'
#
#
# cfa_COHESION0bi <- cfa(model=COHESION_fabi, data = LA,
#                      optim.force.converged = T,#orthogonal=T,
#                      group = "CNT", estimator= "MLR",
#                      sampling.weights ="std.weight.final")
#
# summary(cfa_COHESION0bi)
#
# #
#
#
# COHESION_fa<-'
#
# COHESION =~ BELONG + PERCOOP + PERSPECT + GLOBMIND + RESPECT
#
# PERCOOP ~~ PERSPECT
# GLOBMIND ~~ RESPECT
# '
#
# cfa_COHESION1 <- cfa(model=COHESION_fa, data = LA,#orthogonal=T,
#                      group = "CNT", estimator= "MLR",
#                      sampling.weights ="std.weight.final")
#
# cfa_COHESION2 <- cfa(model=COHESION_fa, data = LA,estimator= "MLR",#orthogonal=T,
#                      group = "CNT", group.equal = c("loadings"),
#                      sampling.weights ="std.weight.final")
#
# cfa_COHESION3 <- cfa(model=COHESION_fa, data = LA, estimator= "WLSMVS",#orthogonal=T,
#                      group = "CNT", group.equal = c("loadings","intercepts"),
#                      sampling.weights ="std.weight.final")
#
# fitmeasures(cfa_COHESION1,  c("chisq.scaled",  "df.scaled", "pvalue.scaled",
#                               "cfi.robust","tli.robust","rmsea.robust",
#                               "rmsea.ci.lower.robust","rmsea.ci.upper.robust",
#                               "srmr"))
#
# fitmeasures(cfa_COHESION2,  c("chisq.scaled",  "df.scaled", "pvalue.scaled",
#                               "cfi.robust","tli.robust","rmsea.robust",
#                               "rmsea.ci.lower.robust","rmsea.ci.upper.robust",
#                               "srmr"))
#
# fitmeasures(cfa_COHESION3,  c("chisq.scaled",  "df.scaled", "pvalue.scaled",
#                               "cfi.robust","tli.robust","rmsea.robust",
#                               "rmsea.ci.lower.robust","rmsea.ci.upper.robust",
#                               "srmr"))
#
#
# # model comparison tests
#
# library(semTools)
#
#
# lavTestLRT(cfa_COHESION1, cfa_COHESION2,cfa_COHESION3,nested = T)
#
#
# #
#
# library("semTable")
#
# ?semTable
#
#
# semTable(cfa_COHESION0,type = "csv",file = "cfa.csv",
#          columns = c("estsestars"),fits = "df")
#
# semTable(cfa_COHESION1,type = "csv",file = "cfa1.csv",
#          columns = c("estsestars"),print.results = F,
#          fits = c("chisq", "df", "pvalue", "cfi.robust","agfi", "rmsea.robust","srmr"))
#
#
# semTable(cfa_COHESION2,type = "csv",file = "cfa2.csv",
#          columns = c("estsestars"),print.results = F,
#          fits = c("chisq", "df", "pvalue", "cfi.robust","agfi", "rmsea.robust","srmr"))
#
#
# semTable(cfa_COHESION3,type = "csv",file = "cfa3.csv",
#          columns = c("estsestars"),print.results = F,
#          fits = c("chisq", "df", "pvalue", "cfi.robust","agfi", "rmsea.robust","srmr"))
#
#
# #modindices(cfa_COHESION1, sort = TRUE, maximum.number = 10)
#
# summary(cfa_COHESION2)
#
# effectsize::interpret(cfa_COHESION2)
#
# #semPlot::semPaths(cfa_COHESION1, "std", edge.label.cex = 0.5, curvePivot = TRUE)
#
#
# report::report_performance(cfa_COHESION1)
# report::report_performance(cfa_COHESION2)
# report::report_performance(cfa_COHESION3)
#
#
#
# idx <- lavInspect(cfa_COHESION2, "case.idx") # list: 1 vector per group
# fscores <- lavPredict(cfa_COHESION2)
#
# ## loop over groups and factors
# for (g in seq_along(fscores)) {
#   for (fs in colnames(fscores[[g]])) {
#     LA[ idx[[g]], fs] <- fscores[[g]][ , fs]
#   }
# }
#
# #LA <- LA %>% filter(!is.na(BELONG))


