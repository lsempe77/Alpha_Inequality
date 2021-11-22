gc()

mod0<-'

level:1
score ~ 1

level:2

score ~ Gini + CNT
'

gini.fit0.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mod0, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)


k <- gini.fit0.LA %>%
  distinct(term) %>% tally() %>% rename(k=n)

k<-k$k


#

gini.fit1<-gini.fit0.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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
  mutate (Gini1 = paste0(round(mean.estimate,2),
                               " (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,Gini1)




gini.fit0_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mod0, data = .,  estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit.par<-c("ntotal","srmr_within","srmr_between","logl","bic","bic2")

gini.fit1b<-gini.fit0_parameters.LA %>% group_by(names) %>%
  filter (names %in% gini.fit.par) %>%
  summarise(x=mean(x,na.rm=T)) %>%
  rename (Gini1=x) %>% rename(Parameter=names)

##



mod1<-'

level:1
score ~ Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + school.HOMEPOS + Tipo.escola + Area + CNT
'

gini.fit1.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mod1, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit1.LA %>% distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit2<-gini.fit1.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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
  mutate (Gini2 = paste0(round(mean.estimate,2)," (",round(se.pool,2),")", " ",sig)) %>%
  ungroup() %>%
  select(Parameter,level,Gini2)


gini.fit1_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mod1, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit2b<-gini.fit1_parameters.LA %>% group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (Gini2=x) %>% rename(Parameter=names)




gini.fit1.t %>% left_join(gini.fit1.p)

##

## BELONG  -------------------------------------------------------------------

###

compensate.BELONG<-'

level:1


score ~  BELONG.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + BELONG.mean + school.HOMEPOS + Tipo.escola + Area  + CNT

'

gini.fit.compensate.BELONG.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.BELONG, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.compensate.BELONG.LA %>% distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit3<-gini.fit.compensate.BELONG.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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




gini.fit.compensate.BELONG_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.BELONG, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit3b<- gini.fit.compensate.BELONG_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (BELONG.compensate=x)

#

LA.pv$ineq.BELONG <-LA.pv$Gini*LA.pv$BELONG.mean


moderate.BELONG<-'
level:1

score ~  BELONG.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + BELONG.mean + ineq.BELONG  + school.HOMEPOS + Tipo.escola + Area + CNT
'

gini.fit.moderate.BELONG.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.BELONG, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.moderate.BELONG.LA %>% distinct(term) %>% tally() %>% rename(k=n)

k<-k$k

gini.fit.moderate.BELONG.LA$term

gini.fit4<-gini.fit.moderate.BELONG.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.moderate.BELONG_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.BELONG, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit4b<-gini.fit.moderate.BELONG_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (BELONG.moderate=x)


gini.fit.LMER<- lme4::lmer(score ~  BELONG.c  +  Sex + REPEAT + HOMEPOS + AGE + Gini * BELONG.mean +
                        school.HOMEPOS + Tipo.escola + Area + CNT + (1|CNTSCHID), data = LA.pv)

summary(gini.fit.LMER)

sjPlot:::plot_model(gini.fit.LMER,type="int",mdrt.values = "meansd")

theme_set(theme_minimal())




##


mediate.BELONG.2<-'
level:1


score ~ BELONG.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*Gini+
          b*BELONG.mean + school.HOMEPOS + Tipo.escola + Area + CNT

BELONG.mean ~  a*Gini + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


gini.fit.mediate.BELONG.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.BELONG.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.mediate.BELONG.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k



gini.fit5<-gini.fit.mediate.BELONG.2.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.mediate.BELONG.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.BELONG.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)


gini.fit5b<-gini.fit.mediate.BELONG.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (BELONG.mediate=x)


variable.names(LA.pv)


##




## PERCOOP  -------------------------------------------------------------------


compensate.PERCOOP<-'

level:1


score ~  PERCOOP.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + PERCOOP.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

gini.fit.compensate.PERCOOP.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.PERCOOP, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.compensate.PERCOOP.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit6<-gini.fit.compensate.PERCOOP.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.compensate.PERCOOP_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.PERCOOP, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit6b<-gini.fit.compensate.PERCOOP_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERCOOP.compensate=x)

#

LA.pv$ineq.PERCOOP <-LA.pv$Gini*LA.pv$PERCOOP.mean

summary(LA.pv$ineq.PERCOOP)

moderate.PERCOOP<-'
level:1

score ~  PERCOOP.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + PERCOOP.mean + ineq.PERCOOP  + school.HOMEPOS + Tipo.escola + Area + CNT
'

gini.fit.moderate.PERCOOP.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.PERCOOP, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.moderate.PERCOOP.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit7<-gini.fit.moderate.PERCOOP.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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




gini.fit.moderate.PERCOOP_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.PERCOOP, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit7b<-gini.fit.moderate.PERCOOP_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERCOOP.moderate=x)

##


mediate.PERCOOP.2<-'
level:1


score ~ PERCOOP.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*Gini+
          b*PERCOOP.mean + school.HOMEPOS + Tipo.escola + Area + CNT

PERCOOP.mean ~  a*Gini + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


gini.fit.mediate.PERCOOP.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.PERCOOP.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.mediate.PERCOOP.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit8<-gini.fit.mediate.PERCOOP.2.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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




gini.fit.mediate.PERCOOP.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.PERCOOP.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



gini.fit8b<-gini.fit.mediate.PERCOOP.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERCOOP.mediate=x)


variable.names(LA.pv)


##

## PERSPECT  -------------------------------------------------------------------

###

compensate.PERSPECT<-'

level:1


score ~  PERSPECT.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + PERSPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

gini.fit.compensate.PERSPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.PERSPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.compensate.PERSPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k


gini.fit9<-gini.fit.compensate.PERSPECT.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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



gini.fit.compensate.PERSPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.PERSPECT, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit9b<-gini.fit.compensate.PERSPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERSPECT.compensate=x)

#

LA.pv$ineq.PERSPECT <-LA.pv$Gini*LA.pv$PERSPECT.mean

summary(LA.pv$ineq.PERSPECT)

moderate.PERSPECT<-'
level:1

score ~  PERSPECT.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + PERSPECT.mean + ineq.PERSPECT  + school.HOMEPOS + Tipo.escola + Area + CNT
'

gini.fit.moderate.PERSPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.PERSPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.moderate.PERSPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit10<-gini.fit.moderate.PERSPECT.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.moderate.PERSPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.PERSPECT, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit10b<-gini.fit.moderate.PERSPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERSPECT.moderate=x)


##


mediate.PERSPECT.2<-'
level:1


score ~ PERSPECT.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*Gini+
          b*PERSPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

PERSPECT.mean ~  a*Gini + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


gini.fit.mediate.PERSPECT.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.PERSPECT.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.mediate.PERSPECT.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit11<-gini.fit.mediate.PERSPECT.2.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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



gini.fit.mediate.PERSPECT.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.PERSPECT.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



gini.fit11b<-gini.fit.mediate.PERSPECT.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (PERSPECT.mediate=x)

variable.names(LA.pv)


##

## RESPECT  -------------------------------------------------------------------

###

compensate.RESPECT<-'

level:1


score ~  RESPECT.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + RESPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

gini.fit.compensate.RESPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.RESPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.compensate.RESPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit12<-gini.fit.compensate.RESPECT.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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



gini.fit.compensate.RESPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.RESPECT, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit12b<-gini.fit.compensate.RESPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESPECT.compensate=x)

#

LA.pv$ineq.RESPECT <-LA.pv$Gini*LA.pv$RESPECT.mean

summary(LA.pv$ineq.RESPECT)

moderate.RESPECT<-'
level:1

score ~  RESPECT.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + RESPECT.mean + ineq.RESPECT  + school.HOMEPOS + Tipo.escola + Area + CNT
'

gini.fit.moderate.RESPECT.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.RESPECT, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.moderate.RESPECT.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit13<-gini.fit.moderate.RESPECT.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.moderate.RESPECT_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.RESPECT, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit13b<-gini.fit.moderate.RESPECT_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESPECT.moderate=x)


##


mediate.RESPECT.2<-'
level:1


score ~ RESPECT.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*Gini+
          b*RESPECT.mean + school.HOMEPOS + Tipo.escola + Area + CNT

RESPECT.mean ~  a*Gini + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


gini.fit.mediate.RESPECT.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.RESPECT.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.mediate.RESPECT.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k


gini.fit14<-gini.fit.mediate.RESPECT.2.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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







gini.fit.mediate.RESPECT.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.RESPECT.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



gini.fit14b<-gini.fit.mediate.RESPECT.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (RESPECT.mediate=x)


variable.names(LA.pv)


##

## GLOBMIND  -------------------------------------------------------------------

###

compensate.GLOBMIND<-'

level:1


score ~  GLOBMIND.c +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT

'

gini.fit.compensate.GLOBMIND.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.GLOBMIND, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.compensate.GLOBMIND.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit15<-gini.fit.compensate.GLOBMIND.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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




gini.fit.compensate.GLOBMIND_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.GLOBMIND, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit15b<-gini.fit.compensate.GLOBMIND_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (GLOBMIND.compensate=x)

#

LA.pv$ineq.GLOBMIND <-LA.pv$Gini*LA.pv$GLOBMIND.mean

summary(LA.pv$ineq.GLOBMIND)

moderate.GLOBMIND<-'
level:1

score ~  GLOBMIND.c  +  Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ Gini + GLOBMIND.mean + ineq.GLOBMIND  + school.HOMEPOS + Tipo.escola + Area + CNT
'

gini.fit.moderate.GLOBMIND.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.GLOBMIND, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.moderate.GLOBMIND.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit16<-gini.fit.moderate.GLOBMIND.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.moderate.GLOBMIND_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.GLOBMIND, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit16b<-gini.fit.moderate.GLOBMIND_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (GLOBMIND.moderate=x)


##


mediate.GLOBMIND.2<-'
level:1


score ~ GLOBMIND.c + Sex + REPEAT + HOMEPOS + AGE

level:2

score ~ c*Gini+
          b*GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT

GLOBMIND.mean ~  a*Gini + school.HOMEPOS + Tipo.escola + Area + CNT

#indirect and total effects between
indirectbw:=a*b
totalbw:=indirectbw+c
'


gini.fit.mediate.GLOBMIND.2.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.GLOBMIND.2, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.mediate.GLOBMIND.2.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit17<-gini.fit.mediate.GLOBMIND.2.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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



gini.fit.mediate.GLOBMIND.2_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.GLOBMIND.2, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



gini.fit17b<-gini.fit.mediate.GLOBMIND.2_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (GLOBMIND.mediate=x)



#save.image(file='myEnvironment.RData')

# SOCIAL COHESION ---------------------------------------------------------

compensate.COHESION<-'

level:1

score ~   Sex + REPEAT + HOMEPOS + AGE + BELONG.c + PERCOOP.c + PERSPECT.c  + RESPECT.c + GLOBMIND.c

level:2

score ~ Gini +  school.HOMEPOS + Tipo.escola + Area + CNT + BELONG.mean + PERCOOP.mean + PERSPECT.mean + RESPECT.mean + GLOBMIND.mean


'

gini.fit.compensate.COHESION.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=compensate.COHESION, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.compensate.COHESION.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit18<-gini.fit.compensate.COHESION.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.compensate.COHESION_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=compensate.COHESION, data = .,
                                  cluster = "CNTSCHID", estimator= "MLR",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit18b<-gini.fit.compensate.COHESION_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (COHESION.compensate=x)

#

summary(LA.pv$GLOBMIND.c)

moderate.COHESION<-'

level:1

score ~   Sex + REPEAT + HOMEPOS + AGE  + BELONG.c + PERCOOP.c + PERSPECT.c + RESPECT.c + GLOBMIND.c


level:2

score ~ Gini + ineq.BELONG  + ineq.PERCOOP +  ineq.PERSPECT + ineq.RESPECT + ineq.GLOBMIND +
school.HOMEPOS + Tipo.escola + Area + CNT +  BELONG.mean + PERCOOP.mean +
PERSPECT.mean + RESPECT.mean + GLOBMIND.mean

'
gc()

gini.fit.moderate.COHESION.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=moderate.COHESION, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.moderate.COHESION.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit19<-gini.fit.moderate.COHESION.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.moderate.COHESION_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=moderate.COHESION, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)

gini.fit19b<-gini.fit.moderate.COHESION_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (COHESION.moderate=x)
##




###

mediate.COHESION<-'
level:1


score ~ Sex + REPEAT + HOMEPOS + AGE  + BELONG.c + PERCOOP.c + PERSPECT.c  + RESPECT.c + GLOBMIND.c

level:2

score ~ c*Gini+
  b1*BELONG.mean + b2*PERCOOP.mean + b3*PERSPECT.mean +
  b4*RESPECT.mean + b5*GLOBMIND.mean + school.HOMEPOS + Tipo.escola + Area + CNT


BELONG.mean ~  a1*Gini + school.HOMEPOS + Tipo.escola + Area + CNT
PERCOOP.mean ~  a2*Gini + school.HOMEPOS + Tipo.escola + Area + CNT
PERSPECT.mean ~  a3*Gini + school.HOMEPOS + Tipo.escola + Area + CNT
RESPECT.mean ~  a4*Gini + school.HOMEPOS + Tipo.escola + Area + CNT
GLOBMIND.mean ~  a5*Gini + school.HOMEPOS + Tipo.escola + Area + CNT


#indirect effects between
indirect.belong:=a1*b1
indirect.percoop:=a2*b2
indirect.perspect:=a3*b3
indirect.respect:=a4*b4
indirect.globmind:=a5*b5

'






gini.fit.mediate.mediate.COHESION.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(sem(model=mediate.COHESION, data = ., estimator= "MLR",
                      cluster = "CNTSCHID",
                      sampling.weights ="std.weight.final"))) %>%
  unnest(model)

k <- gini.fit.mediate.mediate.COHESION.LA %>%  distinct(term) %>% tally() %>% rename(k=n)
k<-k$k

gini.fit20<-gini.fit.mediate.mediate.COHESION.LA %>%
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
          df.adjusted=(df.old*df.obs)/(df.old+df.obs),
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


gini.fit.mediate.COHESION_parameters.LA <- LA.pv %>%
  group_by(READ) %>%
  do(model = tidy(fitmeasures(sem(model=mediate.COHESION, data = ., estimator= "MLR",
                                  cluster = "CNTSCHID",
                                  sampling.weights ="std.weight.final")))) %>%
  unnest(model)



gini.fit20b<-gini.fit.mediate.COHESION_parameters.LA %>%
  group_by(names) %>%
  filter (names %in% gini.fit.par) %>% summarise(x=mean(x,na.rm=T)) %>%
  rename (COHESION.mediate=x)


variable.names(LA.pv)

#save.image(file='myEnvironment.RData')

###

# SOCI
##

gini.fit.models.1<- gini.fit2 %>% full_join (gini.fit3) %>% full_join(gini.fit6) %>%
  full_join(gini.fit9)  %>%
  full_join(gini.fit12)  %>% full_join(gini.fit15)  %>% full_join(gini.fit18)

gini.fit.models.1.gini.fit<- gini.fit2b %>% bind_cols(gini.fit3b[2]) %>% bind_cols(gini.fit6b[2])%>%
  bind_cols(gini.fit9b[2])  %>% bind_cols(gini.fit12b[2])  %>% bind_cols(gini.fit15b[2]) %>%
  bind_cols(gini.fit18b[2])


gini.fit.models.2<-gini.fit4 %>% full_join(gini.fit7)%>%  full_join(gini.fit10)  %>%
  full_join(gini.fit13)  %>% full_join(gini.fit16)  %>% full_join(gini.fit19)

gini.fit.models.2.gini.fit<-gini.fit4b  %>% bind_cols(gini.fit7b[2])%>%
  bind_cols(gini.fit10b[2])  %>% bind_cols(gini.fit13b[2])  %>% bind_cols(gini.fit16b[2]) %>%
  bind_cols(gini.fit19b[2])


gini.fit.models.3<-gini.fit5 %>% full_join(gini.fit8)%>%  full_join(gini.fit11)  %>%
  full_join(gini.fit14)  %>% full_join(gini.fit17)  %>% full_join(gini.fit20)

gini.fit.models.3.gini.fit<-gini.fit5b  %>% bind_cols(gini.fit8b[2])%>%
  bind_cols(gini.fit11b[2])  %>% bind_cols(gini.fit14b[2])  %>% bind_cols(gini.fit17b[2]) %>%
  bind_cols(gini.fit20b[2])




write.csv(gini.fit.models.1,"gini.fit.models.1.csv")

write.csv(gini.fit.models.1.gini.fit,"gini.fit.models.1.fit.csv")

write.csv(gini.fit.models.2,"gini.fit.models.2.csv")

write.csv(gini.fit.models.2.gini.fit,"gini.fit.models.2.fit.csv")

write.csv(gini.fit.models.3,"gini.fit.models.3.csv")

write.csv(gini.fit.models.3.gini.fit,"gini.fit.models.3.fit.csv")
