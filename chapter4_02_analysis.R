library(broom)
library(lavaan)
library(tidyverse)

LA.pv$Area <- as.factor(LA.pv$Area)


mod0<-'

level:1
score ~ 1

level:2

score ~ ineq.alpha + CNT
'



fit0.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = broom::tidy(lavaan::sem(model=mod0, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

n<-count(LA.pv) # check this number

k <- fit0.LA %>%
  distinct(term) %>% tally() %>% rename(k=n)

k<-k$k

gc()

#

fit1<-fit0.LA %>%
  filter (op== '~') %>% group_by(term,level) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(term,level,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (INEQ.ALPHA1 = paste0(round(mean.estimate,2),
                              " (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,INEQ.ALPHA1)

gc()

fit0_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mod0, data = .,  estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gc()

fit.par<-c("ntotal","srmr_within","srmr_between","logl","bic","bic2","r2")

fit1b<-fit0_parameters.LA %>% group_by(names) %>%
  filter (names %in% fit.par) %>%
  summarise(x=mean(x,na.rm=T)) %>%
  rename (INEQ.ALPHA1=x) %>% rename(Parameter=names)


##

# dcols<-fastDummies::dummy_cols(LA.pv$CNT)
# variable.names(dcols)
#
# LA.pv <- LA.pv %>% bind_cols(dcols[2:12])

LA.pv$Area <-as.numeric(LA.pv$Area) #<3k   3k>15k 15k>100k  100k>1m      >1m


mod1<-'

level:1
score ~ Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fit1.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mod1, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

gc()

k <- fit1.LA %>% distinct(term) %>% tally() %>% rename(k=n)

k<-k$k

fit2<-fit1.LA %>%
  filter (op== '~') %>% group_by(term,level) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(term,level,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (INEQ.ALPHA2 = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,INEQ.ALPHA2)


fit1_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mod1, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit2b<-fit1_parameters.LA %>% group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
rename (INEQ.ALPHA2=x) %>% rename(Parameter=names)

gc()

save.image(file='myEnvironment.RData')

gc()

## BELONG  -------------------------------------------------------------------

BELONG.mean <- tapply(LA.pv$BELONG, LA.pv$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA.pv$CNTSCHID)
LA.pv$BELONG.c <- LA.pv$BELONG - BELONG.mean[cluster.idx]
LA.pv$BELONG.mean <- BELONG.mean[cluster.idx]

summary(LA.pv$BELONG)
summary(LA.pv$BELONG.mean)
summary(LA.pv$BELONG.c)



###

compensate.BELONG<-'

level:1


score ~  BELONG.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + BELONG.mean + school.HOMEPOS + Tipo.escola + Area  + CNT

'

fit.compensate.BELONG.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.BELONG, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.BELONG.LA %>% distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit3<-fit.compensate.BELONG.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
    mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(term,level,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
    mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (BELONG.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,BELONG.compensate)


gc()


fit.compensate.BELONG_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.BELONG, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit3b<- fit.compensate.BELONG_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (BELONG.compensate=x)

gc()

#

LA.pv$ineq.BELONG <-LA.pv$ineq.alpha*LA.pv$BELONG.mean

summary(LA.pv$ineq.BELONG)

moderate.BELONG<-'
level:1

score ~  BELONG.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + BELONG.mean + ineq.BELONG  + school.HOMEPOS + Tipo.escola + Area + CNT
'

fit.moderate.BELONG.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.BELONG, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.BELONG.LA %>% distinct(term) %>% tally() %>% rename(k=n)

k<-k$k

fit.moderate.BELONG.LA$term

fit4<-fit.moderate.BELONG.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
         T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[,1])/(df.old+df.obs[,1]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(term,level,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (BELONG.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,BELONG.moderate)

gc()

fit.moderate.BELONG_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.BELONG, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit4b<-fit.moderate.BELONG_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (BELONG.moderate=x)

gc()

FIT.LMER<- lme4::lmer(score ~ BELONG.c  +  Sex + REPEAT + HOMEPOS +
                        AGE + ineq.alpha * BELONG.mean +
              school.HOMEPOS + Tipo.escola + Area + CNT +
                (1|CNTSCHID), data = LA.pv)

interactions::interact_plot(FIT.LMER,pred = ineq.alpha,
                             modx = BELONG.mean,
                             alpha = .05, interval = TRUE,
                            int.width = 0.95,robust = T,
                            colors = "Qual3",
                            x.label = "INEQ ALPHA",
                            y.label = "PISA score",
                            legend.main = "Belong (between)")

gc()

interactions::johnson_neyman(FIT.LMER,pred = ineq.alpha,
                             modx = BELONG.mean,
                             control.fdr = T,
                             sig.color = "black") + xlab("BELONG")

gc()

##

##


mediate.BELONG.2<-'
level:1


score ~ BELONG.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*ineq.alpha+
          b*BELONG.mean + school.HOMEPOS + Tipo.escola + Area + CNT

BELONG.mean ~  a*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


fit.mediate.BELONG.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.BELONG.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.BELONG.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k



fit5<-fit.mediate.BELONG.2.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  group_by(term,level) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
           mutate(Parameter=sub('.*\\~', '', term)) %>%
          mutate (BELONG.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
   mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
    select(Parameter,level,BELONG.mediate)


gc()

fit.mediate.BELONG.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.BELONG.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)


fit5b<-fit.mediate.BELONG.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (BELONG.mediate=x)


gc()


##



save.image(file='myEnvironment.RData')

gc()


## RESILIENCE  -------------------------------------------------------------------

RESILIENCE.mean <- tapply(LA.pv$RESILIENCE, LA.pv$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA.pv$CNTSCHID)
LA.pv$RESILIENCE.c <- LA.pv$RESILIENCE - RESILIENCE.mean[cluster.idx]
LA.pv$RESILIENCE.mean <- RESILIENCE.mean[cluster.idx]

summary(LA.pv$RESILIENCE)
summary(LA.pv$RESILIENCE.mean)
summary(LA.pv$RESILIENCE.c)

summary(LA.pv$REPEAT)

variable.names(LA.pv)

###

compensate.RESILIENCE<-'

level:1


score ~  RESILIENCE.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + RESILIENCE.mean + school.HOMEPOS + Tipo.escola + Area  + CNT

'

fit.compensate.RESILIENCE.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.RESILIENCE, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.RESILIENCE.LA %>% distinct(term) %>% tally() %>% rename(k=n)
k<-k$k


fit3.resilience<-fit.compensate.RESILIENCE.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(term,level,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (RESILIENCE.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,RESILIENCE.compensate)


gc()


fit.compensate.RESILIENCE_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.RESILIENCE, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit3.resilience.b<- fit.compensate.RESILIENCE_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESILIENCE.compensate=x)

gc()

#

LA.pv$ineq.RESILIENCE <-LA.pv$ineq.alpha*LA.pv$RESILIENCE.mean

summary(LA.pv$ineq.RESILIENCE)

moderate.RESILIENCE<-'
level:1

score ~  RESILIENCE.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + RESILIENCE.mean + ineq.RESILIENCE  + school.HOMEPOS + Tipo.escola + Area + CNT
'

fit.moderate.RESILIENCE.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.RESILIENCE, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.RESILIENCE.LA %>% distinct(term) %>% tally() %>% rename(k=n)

k<-k$k

fit.moderate.RESILIENCE.LA$term

fit4.resilience<-fit.moderate.RESILIENCE.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(term,level,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (RESILIENCE.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,RESILIENCE.moderate)

gc()

fit.moderate.RESILIENCE_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.RESILIENCE, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit4.resilience.b<-fit.moderate.RESILIENCE_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESILIENCE.moderate=x)

gc()

#

labels <-c("a","b","c","indirectbw","totalbw")


##


mediate.RESILIENCE.2<-'
level:1


score ~ RESILIENCE.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*ineq.alpha+
          b*RESILIENCE.mean + school.HOMEPOS + Tipo.escola + Area + CNT

RESILIENCE.mean ~  a*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


fit.mediate.RESILIENCE.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.RESILIENCE.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.RESILIENCE.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k



fit5.resilience<-fit.mediate.RESILIENCE.2.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  group_by(term,level) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (RESILIENCE.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
  select(Parameter,level,RESILIENCE.mediate)

gc()


fit.mediate.RESILIENCE.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.RESILIENCE.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)


fit5b.resilience<-fit.mediate.RESILIENCE.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESILIENCE.mediate=x)


gc()


save.image(file='myEnvironment.RData')

gc()

## PERCOOP  -------------------------------------------------------------------

PERCOOP.mean <- tapply(LA.pv$PERCOOP, LA.pv$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA.pv$CNTSCHID)
LA.pv$PERCOOP.c <- LA.pv$PERCOOP - PERCOOP.mean[cluster.idx]
LA.pv$PERCOOP.mean <- PERCOOP.mean[cluster.idx]

summary(LA.pv$PERCOOP)
summary(LA.pv$PERCOOP.mean)
summary(LA.pv$PERCOOP.c)

###

compensate.PERCOOP<-'

level:1


score ~  PERCOOP.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + PERCOOP.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

fit.compensate.PERCOOP.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.PERCOOP, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.PERCOOP.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit6<-fit.compensate.PERCOOP.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (PERCOOP.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,PERCOOP.compensate)

gc()

fit.compensate.PERCOOP_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.PERCOOP, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit6b<-fit.compensate.PERCOOP_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERCOOP.compensate=x)

gc()

#

LA.pv$ineq.PERCOOP <-LA.pv$ineq.alpha*LA.pv$PERCOOP.mean

summary(LA.pv$ineq.PERCOOP)

moderate.PERCOOP<-'
level:1

score ~  PERCOOP.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + PERCOOP.mean + ineq.PERCOOP  + school.HOMEPOS + Tipo.escola + Area + CNT
'

fit.moderate.PERCOOP.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.PERCOOP, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.PERCOOP.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit7<-fit.moderate.PERCOOP.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (PERCOOP.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,PERCOOP.moderate)

gc()



fit.moderate.PERCOOP_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.PERCOOP, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit7b<-fit.moderate.PERCOOP_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERCOOP.moderate=x)

gc()



##


mediate.PERCOOP.2<-'
level:1


score ~ PERCOOP.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*ineq.alpha+
          b*PERCOOP.mean + school.HOMEPOS + Tipo.escola + Area + CNT

PERCOOP.mean ~  a*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


fit.mediate.PERCOOP.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.PERCOOP.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.PERCOOP.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit8<-fit.mediate.PERCOOP.2.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (PERCOOP.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
  select(Parameter,level,PERCOOP.mediate)

gc()



fit.mediate.PERCOOP.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.PERCOOP.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



fit8b<-fit.mediate.PERCOOP.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERCOOP.mediate=x)


variable.names(LA.pv)
gc()


save.image(file='myEnvironment.RData')

gc()

## PERSPECT  -------------------------------------------------------------------

PERSPECT.mean <- tapply(LA.pv$PERSPECT, LA.pv$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA.pv$CNTSCHID)
LA.pv$PERSPECT.c <- LA.pv$PERSPECT - PERSPECT.mean[cluster.idx]
LA.pv$PERSPECT.mean <- PERSPECT.mean[cluster.idx]

summary(LA.pv$PERSPECT)
summary(LA.pv$PERSPECT.mean)
summary(LA.pv$PERSPECT.c)

###

compensate.PERSPECT<-'

level:1


score ~  PERSPECT.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + PERSPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

fit.compensate.PERSPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.PERSPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.PERSPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit9<-fit.compensate.PERSPECT.LA %>%
         filter (op== '~' |
          term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (PERSPECT.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,PERSPECT.compensate)

gc()

fit.compensate.PERSPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.PERSPECT, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit9b<-fit.compensate.PERSPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERSPECT.compensate=x)

gc()

#


#

LA.pv$ineq.PERSPECT <-LA.pv$ineq.alpha*LA.pv$PERSPECT.mean

summary(LA.pv$ineq.PERSPECT)

moderate.PERSPECT<-'
level:1

score ~  PERSPECT.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + PERSPECT.mean + ineq.PERSPECT  + school.HOMEPOS + Tipo.escola + Area + CNT
'

fit.moderate.PERSPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.PERSPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.PERSPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit10<-fit.moderate.PERSPECT.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (PERSPECT.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,PERSPECT.moderate)

gc()



fit.moderate.PERSPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.PERSPECT, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit10b<-fit.moderate.PERSPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERSPECT.moderate=x)

gc()

##


##


mediate.PERSPECT.2<-'
level:1


score ~ PERSPECT.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*ineq.alpha+
          b*PERSPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

PERSPECT.mean ~  a*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


fit.mediate.PERSPECT.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.PERSPECT.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.PERSPECT.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit11<-fit.mediate.PERSPECT.2.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (PERSPECT.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
  select(Parameter,level,PERSPECT.mediate)

gc()


fit.mediate.PERSPECT.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.PERSPECT.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



fit11b<-fit.mediate.PERSPECT.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERSPECT.mediate=x)

gc()



save.image(file='myEnvironment.RData')

gc()

## RESPECT  -------------------------------------------------------------------

RESPECT.mean <- tapply(LA.pv$RESPECT, LA.pv$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA.pv$CNTSCHID)
LA.pv$RESPECT.c <- LA.pv$RESPECT - RESPECT.mean[cluster.idx]
LA.pv$RESPECT.mean <- RESPECT.mean[cluster.idx]

summary(LA.pv$RESPECT)
summary(LA.pv$RESPECT.mean)
summary(LA.pv$RESPECT.c)

###

compensate.RESPECT<-'

level:1


score ~  RESPECT.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + RESPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

fit.compensate.RESPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.RESPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.RESPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit12<-fit.compensate.RESPECT.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (RESPECT.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,RESPECT.compensate)

gc()


fit.compensate.RESPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.RESPECT, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit12b<-fit.compensate.RESPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESPECT.compensate=x)

gc()


#

LA.pv$ineq.RESPECT <-LA.pv$ineq.alpha*LA.pv$RESPECT.mean

summary(LA.pv$ineq.RESPECT)

moderate.RESPECT<-'
level:1

score ~  RESPECT.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + RESPECT.mean + ineq.RESPECT  + school.HOMEPOS + Tipo.escola + Area + CNT
'

fit.moderate.RESPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.RESPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.RESPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit13<-fit.moderate.RESPECT.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (RESPECT.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,RESPECT.moderate)

gc()

fit.moderate.RESPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.RESPECT, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit13b<-fit.moderate.RESPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESPECT.moderate=x)

gc()

#

FIT.LMER1<- lme4::lmer(score ~ RESPECT.c  +  Sex + REPEAT + HOMEPOS +
                        AGE + ineq.alpha * RESPECT.mean +
                        school.HOMEPOS + Tipo.escola + Area +
                        (1|CNTSCHID), data = LA.pv)

interactions::interact_plot(FIT.LMER1,pred = ineq.alpha,
                            modx = RESPECT.mean,
                            alpha = .05, interval = TRUE,
                            int.width = 0.95,robust = T,
                            colors = "Qual3",
                            x.label = "INEQ ALPHA",
                            y.label = "PISA score",
                            legend.main = "Respect (between)")


##


mediate.RESPECT.2<-'
level:1


score ~ RESPECT.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*ineq.alpha+
          b*RESPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

RESPECT.mean ~  a*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


fit.mediate.RESPECT.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.RESPECT.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.RESPECT.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k


fit14<-fit.mediate.RESPECT.2.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  group_by(term,level) %>%
    mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(label,mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (RESPECT.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
  select(Parameter,level,RESPECT.mediate)


gc()





fit.mediate.RESPECT.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.RESPECT.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



fit14b<-fit.mediate.RESPECT.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESPECT.mediate=x)


variable.names(LA.pv)

gc()

##
save.image(file='myEnvironment.RData')

gc()
## GLOBMIND  -------------------------------------------------------------------

GLOBMIND.mean <- tapply(LA.pv$GLOBMIND, LA.pv$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA.pv$CNTSCHID)
LA.pv$GLOBMIND.c <- LA.pv$GLOBMIND - GLOBMIND.mean[cluster.idx]
LA.pv$GLOBMIND.mean <- GLOBMIND.mean[cluster.idx]

summary(LA.pv$GLOBMIND)
summary(LA.pv$GLOBMIND.mean)
summary(LA.pv$GLOBMIND.c)

###

compensate.GLOBMIND<-'

level:1


score ~  GLOBMIND.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

fit.compensate.GLOBMIND.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.GLOBMIND, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.GLOBMIND.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit15<-fit.compensate.GLOBMIND.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (GLOBMIND.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,GLOBMIND.compensate)


gc()


fit.compensate.GLOBMIND_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.GLOBMIND, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit15b<-fit.compensate.GLOBMIND_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (GLOBMIND.compensate=x)

gc()


#

LA.pv$ineq.GLOBMIND <-LA.pv$ineq.alpha*LA.pv$GLOBMIND.mean

summary(LA.pv$ineq.GLOBMIND)

moderate.GLOBMIND<-'
level:1

score ~  GLOBMIND.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ ineq.alpha + GLOBMIND.mean + ineq.GLOBMIND  + school.HOMEPOS + Tipo.escola + Area + CNT
'

fit.moderate.GLOBMIND.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.GLOBMIND, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.GLOBMIND.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit16<-fit.moderate.GLOBMIND.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (GLOBMIND.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,GLOBMIND.moderate)

gc()

fit.moderate.GLOBMIND_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.GLOBMIND, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit16b<-fit.moderate.GLOBMIND_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (GLOBMIND.moderate=x)

gc()

##

##


mediate.GLOBMIND.2<-'
level:1


score ~ GLOBMIND.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*ineq.alpha+
          b*GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT

GLOBMIND.mean ~  a*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


fit.mediate.GLOBMIND.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.GLOBMIND.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.GLOBMIND.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit17<-fit.mediate.GLOBMIND.2.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (GLOBMIND.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
  select(Parameter,level,GLOBMIND.mediate)

gc()


fit.mediate.GLOBMIND.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.GLOBMIND.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



fit17b<-fit.mediate.GLOBMIND.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (GLOBMIND.mediate=x)


variable.names(LA.pv)

save.image(file='myEnvironment06nov.RData')

gc()

# SOCIAL COHESION ---------------------------------------------------------

compensate.COHESION<-'

level:1

score ~   Sex + REPEAT + HOMEPOS + AGE +  BELONG.c + PERCOOP.c + PERSPECT.c + RESPECT.c + GLOBMIND.c

level:2

score ~ ineq.alpha +  school.HOMEPOS + Tipo.escola + Area + CNT + BELONG.mean + PERCOOP.mean + PERSPECT.mean + RESPECT.mean + GLOBMIND.mean


'

fit.compensate.COHESION.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.COHESION, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.compensate.COHESION.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit18<-fit.compensate.COHESION.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (COHESION.compensate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,COHESION.compensate)

gc()

fit.compensate.COHESION_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.COHESION, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit18b<-fit.compensate.COHESION_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (COHESION.compensate=x)

gc()

#

summary(LA.pv$GLOBMIND.c)

moderate.COHESION<-'

level:1

score ~   Sex + REPEAT + HOMEPOS + AGE  + BELONG.c + PERCOOP.c + PERSPECT.c + RESPECT.c + GLOBMIND.c


level:2

score ~ ineq.alpha + ineq.BELONG  + ineq.PERCOOP +  ineq.PERSPECT + ineq.RESPECT + ineq.GLOBMIND +
school.HOMEPOS + Tipo.escola + Area + CNT +  BELONG.mean + PERCOOP.mean +
PERSPECT.mean + RESPECT.mean + GLOBMIND.mean

'
gc()

fit.moderate.COHESION.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.COHESION, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.moderate.COHESION.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit19<-fit.moderate.COHESION.LA %>%
  filter (op== '~' |
            term == 'score ~1 ') %>% group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          #pvalue>=.05 & pvalue <.1 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (COHESION.moderate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,COHESION.moderate)

gc()

fit.moderate.COHESION_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.COHESION, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

fit19b<-fit.moderate.COHESION_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (COHESION.moderate=x)

gc()

##




###

mediate.COHESION<-'
level:1


score ~ Sex + REPEAT + HOMEPOS + AGE  + BELONG.c + PERCOOP.c + PERSPECT.c  + RESPECT.c + GLOBMIND.c

level:2

score ~ c*ineq.alpha+
  b1*BELONG.mean + b2*PERCOOP.mean + b3*PERSPECT.mean +
  b4*RESPECT.mean + b5*GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT


BELONG.mean ~  a1*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT
PERCOOP.mean ~  a2*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT
PERSPECT.mean ~  a3*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT
RESPECT.mean ~  a4*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT
GLOBMIND.mean ~  a5*ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT




#indirect effects between
indirect.belong:=a1*b1
indirect.percoop:=a2*b2
indirect.perspect:=a3*b3
indirect.respect:=a4*b4
indirect.globmind:=a5*b5

total.belong:=indirect.belong+c
total.percoop:=indirect.percoop+c
total.perspect:=indirect.perspect+c
total.respect:=indirect.respect+c
total.globmind:=indirect.globmind+c

'






fit.mediate.mediate.COHESION.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.COHESION, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- fit.mediate.mediate.COHESION.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

fit20<-fit.mediate.mediate.COHESION.LA %>%
  filter (op== '~' | op== ':=' |
            term == 'score ~1 ') %>%
  group_by(term,level) %>%
  mutate(term = case_when(term == 'score ~1 ' ~ "score ~ intercept",
                          T ~ term)) %>%
  mutate (mean.estimate=mean(estimate),
          within.var=mean(std.error^2),
          between.var=sum((estimate-mean.estimate)^2)/9,
          var.total=within.var+between.var+(between.var/10),
          se.pool=sqrt(var.total),
          t=mean.estimate/se.pool,
          lambda=(between.var+(between.var/10))/var.total,
          riv=(between.var+(between.var/10))/within.var,
          df.old=(10-1)/lambda^2,
          df.obs= ((n-k+1)/(n-k+3))*(n-k)*(1-lambda),
          df.adjusted=(df.old*df.obs[1,])/(df.old+df.obs[1,]),
          pvalue=2*pt(-abs(t),df=df.adjusted)) %>%
  filter (READ == "PV1READ") %>%
  select(mean.estimate, se.pool, df.adjusted,  pvalue) %>%
  mutate (sig = case_when(pvalue<.001 ~ "***",
                          pvalue>.001 & pvalue <.01 ~ "**",
                          pvalue>=.01 & pvalue <.05 ~ "*",
                          pvalue>=.05 ~ "",
                          T ~ "error")) %>%
  mutate(Parameter=sub('.*\\~', '', term)) %>%
  mutate (COHESION.mediate = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  mutate(Parameter2 = case_when(str_detect(term,"score",negate=T) ~ term,
                                T ~NA_character_)) %>%
  mutate(Parameter=coalesce(Parameter2,Parameter)) %>%
  select(Parameter,level,COHESION.mediate)

gc()


fit.mediate.COHESION_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.COHESION, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



fit20b<-fit.mediate.COHESION_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (COHESION.mediate=x)


#



### # FALTA RESILIENCE!

##

gc()

fit.models.1<- fit2 %>% full_join (fit3) %>% full_join(fit6) %>%
  full_join(fit9)  %>%
  full_join(fit12)  %>% full_join(fit15)  %>% full_join(fit18)

fit.models.1.fit<- fit2b %>% bind_cols(fit3b[2]) %>% bind_cols(fit6b[2])%>%
  bind_cols(fit9b[2])  %>% bind_cols(fit12b[2])  %>% bind_cols(fit15b[2]) %>%
  bind_cols(fit18b[2])


fit.models.2<-fit4 %>% full_join(fit7)%>%  full_join(fit10)  %>%
  full_join(fit13)  %>% full_join(fit16)  %>% full_join(fit19)

fit.models.2.fit<-fit4b  %>% bind_cols(fit7b[2])%>%
  bind_cols(fit10b[2])  %>% bind_cols(fit13b[2])  %>% bind_cols(fit16b[2]) %>%
  bind_cols(fit19b[2])


fit.models.3<-fit5 %>% full_join(fit8)%>%  full_join(fit11)  %>%
  full_join(fit14)  %>% full_join(fit17)  %>% full_join(fit20)

fit.models.3.fit<-fit5b  %>% bind_cols(fit8b[2])%>%
  bind_cols(fit11b[2])  %>% bind_cols(fit14b[2])  %>% bind_cols(fit17b[2]) %>%
  bind_cols(fit20b[2])

#

fit.models.resilience<-fit5.resilience  %>%
  full_join(fit4.resilience)%>%
  left_join(fit3.resilience)

fit.models.resilience.fit<-fit3.resilience.b  %>%
  bind_cols(fit4.resilience.b[2])%>%
  bind_cols(fit5b.resilience[2])


write.csv(fit.models.1,"fit.models.1.nov.csv")

write.csv(fit.models.1.fit,"fit.models.1.nov.fit.csv")

write.csv(fit.models.2,"fit.models.2.nov.csv")

write.csv(fit.models.2.fit,"fit.models.2.nov.fit.csv")

write.csv(fit.models.3,"fit.models.3.nov.csv")

write.csv(fit.models.3.fit,"fit.models.3.nov.fit.csv")

write.csv(fit.models.resilience,"fit.models.resilience.csv")

write.csv(fit.models.resilience.fit,"fit.resilience.fit.csv")

save.image(file='myEnvironment07_nov.RData')


# FALTA RESILIENCE!


###


##

# Compensation R2 ---------------------------------------------------------

#FALTA RESILIENCE


mod1c<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE

level:2

PV1READ ~ ineq.alpha + school.HOMEPOS + Tipo.escola + Area + CNT'

fi1c<-sem(model=mod1c, data = LA, estimator= "MLR",
          cluster = "CNTSCHID",
          sampling.weights ="std.weight.final")


lavInspect(fi1c,"r2")

#

gc()


BELONG.mean <- tapply(LA$BELONG, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$BELONG.c <- LA$BELONG - BELONG.mean[cluster.idx]
LA$BELONG.mean <- BELONG.mean[cluster.idx]

mod1c.belong<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + BELONG.c

level:2

PV1READ ~ ineq.alpha +BELONG.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.belong<-sem(model=mod1c.belong, data = LA, estimator= "MLR",
          cluster = "CNTSCHID",
          sampling.weights ="std.weight.final")


lavInspect(fi1c,"r2")
lavInspect(fi1c.belong,"r2")

#

PERCOOP.mean <- tapply(LA$PERCOOP, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$PERCOOP.c <- LA$PERCOOP - PERCOOP.mean[cluster.idx]
LA$PERCOOP.mean <- PERCOOP.mean[cluster.idx]

mod1c.PERCOOP<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + PERCOOP.c

level:2

PV1READ ~ ineq.alpha +PERCOOP.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.PERCOOP<-sem(model=mod1c.PERCOOP, data = LA, estimator= "MLR",
                 cluster = "CNTSCHID",
                 sampling.weights ="std.weight.final")


lavInspect(fi1c,"r2")
lavInspect(fi1c.PERCOOP,"r2")

#

PERCOOP.mean <- tapply(LA$PERCOOP, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$PERCOOP.c <- LA$PERCOOP - PERCOOP.mean[cluster.idx]
LA$PERCOOP.mean <- PERCOOP.mean[cluster.idx]

mod1c.PERCOOP<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + PERCOOP.c

level:2

PV1READ ~ ineq.alpha +PERCOOP.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.PERCOOP<-sem(model=mod1c.PERCOOP, data = LA, estimator= "MLR",
                  cluster = "CNTSCHID",
                  sampling.weights ="std.weight.final")



#

PERSPECT.mean <- tapply(LA$PERSPECT, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$PERSPECT.c <- LA$PERSPECT - PERSPECT.mean[cluster.idx]
LA$PERSPECT.mean <- PERSPECT.mean[cluster.idx]

mod1c.PERSPECT<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + PERSPECT.c

level:2

PV1READ ~ ineq.alpha +PERSPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.PERSPECT<-sem(model=mod1c.PERSPECT, data = LA, estimator= "MLR",
                  cluster = "CNTSCHID",
                  sampling.weights ="std.weight.final")



#

#

RESPECT.mean <- tapply(LA$RESPECT, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$RESPECT.c <- LA$RESPECT - RESPECT.mean[cluster.idx]
LA$RESPECT.mean <- RESPECT.mean[cluster.idx]

mod1c.RESPECT<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + RESPECT.c

level:2

PV1READ ~ ineq.alpha +RESPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.RESPECT<-sem(model=mod1c.RESPECT, data = LA, estimator= "MLR",
                   cluster = "CNTSCHID",
                   sampling.weights ="std.weight.final")


lavInspect(fi1c.RESPECT,"r2")

#

#

GLOBMIND.mean <- tapply(LA$GLOBMIND, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$GLOBMIND.c <- LA$GLOBMIND - GLOBMIND.mean[cluster.idx]
LA$GLOBMIND.mean <- GLOBMIND.mean[cluster.idx]

mod1c.GLOBMIND<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + GLOBMIND.c

level:2

PV1READ ~ ineq.alpha +GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.GLOBMIND<-sem(model=mod1c.GLOBMIND, data = LA, estimator= "MLR",
                  cluster = "CNTSCHID",
                  sampling.weights ="std.weight.final")


lavInspect(fi1c.GLOBMIND,"r2")

#

gc()


RESILIENCE.mean <- tapply(LA$RESILIENCE, LA$CNTSCHID, mean,na.rm=T)
cluster.idx <- as.character(LA$CNTSCHID)
LA$RESILIENCE.c <- LA$RESILIENCE - RESILIENCE.mean[cluster.idx]
LA$RESILIENCE.mean <- RESILIENCE.mean[cluster.idx]

mod1c.RESILIENCE<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + RESILIENCE.c

level:2

PV1READ ~ ineq.alpha +RESILIENCE.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.RESILIENCE<-sem(model=mod1c.RESILIENCE, data = LA, estimator= "MLR",
                 cluster = "CNTSCHID",
                 sampling.weights ="std.weight.final")


lavInspect(fi1c,"r2")
lavInspect(fi1c.RESILIENCE,"r2")

#

mod1c.COHESION<-'

level:1
PV1READ ~ Sex + REPEAT + HOMEPOS + AGE + GLOBMIND.c +BELONG.c+
PERCOOP.c+PERSPECT.c+RESPECT.c

level:2

PV1READ ~ ineq.alpha +GLOBMIND.mean + BELONG.mean+
PERCOOP.mean+PERSPECT.mean+RESPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT'

gc()

fi1c.COHESION<-sem(model=mod1c.COHESION, data = LA, estimator= "MLR",
                   cluster = "CNTSCHID",
                   sampling.weights ="std.weight.final")


li1<-as.data.frame(lavInspect(fi1c,"r2"))

li2<-as.data.frame(lavInspect(fi1c.belong,"r2"))

li3<-as.data.frame(lavInspect(fi1c.PERCOOP,"r2"))

li4<-as.data.frame(lavInspect(fi1c.PERSPECT,"r2"))

li5<-as.data.frame(lavInspect(fi1c.RESPECT,"r2"))

li6<-as.data.frame(lavInspect(fi1c.GLOBMIND,"r2"))

li7<-as.data.frame(lavInspect(fi1c.RESILIENCE,"r2"))

li7b<-as.data.frame(lavInspect(fi1c.COHESION,"r2"))

li8<-li1 %>% bind_rows(li2)%>% bind_rows(li3 )%>%
  bind_rows(li4)%>% bind_rows(li5) %>%
  bind_rows(li6)%>% bind_rows(li7) %>% bind_rows(li7b) %>%
  rownames_to_column() %>% rename (Model = rowname,
                                   Within = within ,
                                   Between = CNTSCHID)

names<-c(
    "Baseline",
    "BELONG",
    "PERCOOP",
    "PERSPECT" ,
    "RESPECT" ,
    "GLOBMIND"  ,
    "RESILIENCE",
    "COHESION")

li8[1]<-c(
  "Baseline",
  "BELONG",
  "PERCOOP",
  "PERSPECT" ,
  "RESPECT" ,
  "GLOBMIND"  ,
  "RESILIENCE",
  "COHESION")

# revisar estos valores de baseline

li8 <- li8 %>% mutate (
  difference.within = Within-0.10035471,
  difference.betweeb = Between-0.4937770,

)

write.csv(li8,"r2.nov.csv")

