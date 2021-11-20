convertiblesToNumeric <- function(x){
  x2 <- cbind.data.frame(lapply(seq_along(x), function(i) {
    if (!all(is.na(as.numeric(x[, i])))){
      as.numeric(x[, i])
    } else {
      x[, i]
    }
  }), stringsAsFactors=FALSE)
  names(x2) <- names(x)
  return(x2)
}

library(pander) 
library(tidyverse)
library(BIFIEsurvey)

###

pisaf.brr<-pisaf3 %>% dplyr::select(1:3,727:807,810:819,922,924,978,979,983)

variable.names(pisaf.brr)

summary(pisaf.brr$W_SCHGRNRABWT)

variable.names(pisaf.brr)

adjusting.weights <- pisaf.brr %>% group_by(CNTSCHID) %>% #method 2 Rabe 2006
  summarise ( 
    school.size = n(),
    sum.weights.students=sum(Student.Weight),
    adjust.weight = school.size/sum.weights.students)
  
pisaf.brr <- pisaf.brr %>% left_join(adjusting.weights) %>%
  mutate(std.weight.final = Student.Weight*adjust.weight)

variable.names(pisaf.brr)
summary(pisaf.brr$std.weight.final)

##### ALPHA INEQUALITY #####


gc()

brr<- pisaf.brr %>%   filter(str_detect(CNT, "^A|^B")) %>%
  group_by(CNT) %>% group_map(~BIFIE.data.jack(data=., wgt="std.weight.final",
                                               jktype="RW_PISA", 
                           pvpre =paste0("PV",1:10), fayfac=.05,
                           wgtrep=paste0("W_FSTURWT",1:80),
                           cdata=F))



brr.reg <- brr %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                              formula.fixed=~ HOMEPOS.m+
                                scale(ineq.alpha), 
                              formula.random=~ 1,
                              idcluster="CNTSCHID",
                              wgtlevel1="std.weight.final",
                           wgtlevel2 = "one")) 



##

gc()

brr1<- pisaf.brr %>%   filter(str_detect(CNT, "^C")) %>%
  group_by(CNT) %>%
      group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr1

brr.reg1 <- brr1 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()
##

brr2<- pisaf.brr %>%   filter(str_detect(CNT, "^D")) %>%
  group_by(CNT) %>%
      group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))

brr2


brr.reg2 <- brr2 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()


##

brr3<- pisaf.brr %>%   filter(str_detect(CNT,"^E|^F")) %>%
  group_by(CNT) %>%
      group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr3


brr.reg3 <- brr3 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##

brr4<- pisaf.brr %>%   filter(str_detect(CNT, "^G|^H")) %>%
  group_by(CNT) %>%
      group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr4


brr.reg4 <- brr4 %>% 
      map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


gc()

reg<-do.call("cbind", lapply(brr.reg, '[[', 1))
reg1<-do.call("cbind", lapply(brr.reg1, '[[', 1))
reg2<-do.call("cbind", lapply(brr.reg2, '[[', 1))
reg3<-do.call("cbind", lapply(brr.reg3, '[[', 1))
reg4<-do.call("cbind", lapply(brr.reg4, '[[', 1))

rm(brr.reg);rm(brr.reg1);rm(brr.reg2);rm(brr.reg3);rm(brr.reg4)

##

gc()


brr5<- pisaf.brr %>%   filter(str_detect(CNT, "^I|^J")) %>%
  group_by(CNT) %>%
      group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr5


brr.reg5 <- brr5 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##

brr6<- pisaf.brr %>%   filter(str_detect(CNT, "^K|^L")) %>%
  group_by(CNT) %>%
      group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr6


brr.reg6 <- brr6 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

###

brr7<- pisaf.brr %>%   filter(str_detect(CNT, "^M")) %>%
  group_by(CNT) %>%
  group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr7


brr.reg7 <- brr7 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##


brr8<- pisaf.brr %>%   filter(str_detect(CNT, "^N|^O|^P")) %>%
  group_by(CNT) %>%
  group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr8


brr.reg8 <- brr8 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

####

reg5<-do.call("cbind", lapply(brr.reg5, '[[', 1))
reg6<-do.call("cbind", lapply(brr.reg6, '[[', 1))
reg7<-do.call("cbind", lapply(brr.reg7, '[[', 1))
reg8<-do.call("cbind", lapply(brr.reg8, '[[', 1))

rm(brr.reg5);rm(brr.reg6);rm(brr.reg7);rm(brr.reg8)

gc()
##

brr9<- pisaf.brr %>%   filter(str_detect(CNT, "^Q|^R")) %>%
  group_by(CNT) %>%
  group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr9


brr.reg9 <- brr9 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##


brr10<- pisaf.brr %>%   filter(str_detect(CNT, "^S")) %>%
  group_by(CNT) %>%
  group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr10


brr.reg10 <- brr10 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##

##

brr11<- pisaf.brr %>%   filter(str_detect(CNT, "^T|^U|^V|^W|^X|^Y|^Z")) %>%
  group_by(CNT) %>%
  group_map(~BIFIE.data.jack(data=., wgt="std.weight.final", jktype="RW_PISA", 
                             pvpre =paste0("PV",1:10), fayfac=.05,
                             wgtrep=paste0("W_FSTURWT",1:80),
                             cdata=F))
brr11


brr.reg11 <- brr11 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##

reg9<-do.call("cbind", lapply(brr.reg9, '[[', 1))
reg10<-do.call("cbind", lapply(brr.reg10, '[[', 1))
reg11<-do.call("cbind", lapply(brr.reg11, '[[', 1))

rm(brr.reg9);rm(brr.reg10);rm(brr.reg11)


##### ALPHA INEQUALITY - interaction #####
gc()

inter.brr.reg <- brr %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


##

inter.brr.reg1 <- brr1 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


gc()
##



inter.brr.reg2 <- brr2 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()


##

inter.brr.reg3 <- brr3 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##


inter.brr.reg4 <- brr4 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


gc()

inter.reg<-do.call("cbind", lapply(inter.brr.reg, '[[', 1))
inter.reg1<-do.call("cbind", lapply(inter.brr.reg1, '[[', 1))
inter.reg2<-do.call("cbind", lapply(inter.brr.reg2, '[[', 1))
inter.reg3<-do.call("cbind", lapply(inter.brr.reg3, '[[', 1))
inter.reg4<-do.call("cbind", lapply(inter.brr.reg4, '[[', 1))

rm(inter.brr.reg);rm(inter.brr.reg1);rm(inter.brr.reg2);rm(inter.brr.reg3);rm(inter.brr.reg4)

##

gc()


inter.brr.reg5 <- brr5 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##

inter.brr.reg6 <- brr6 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

###

inter.brr.reg7 <- brr7 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##


inter.brr.reg8 <- brr8 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

####

inter.reg5<-do.call("cbind", lapply(inter.brr.reg5, '[[', 1))
inter.reg6<-do.call("cbind", lapply(inter.brr.reg6, '[[', 1))
inter.reg7<-do.call("cbind", lapply(inter.brr.reg7, '[[', 1))
inter.reg8<-do.call("cbind", lapply(inter.brr.reg8, '[[', 1))

rm(inter.brr.reg5);rm(inter.brr.reg6);rm(inter.brr.reg7);rm(inter.brr.reg8)

gc()
##

inter.brr.reg9 <- brr9 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##


inter.brr.reg10 <- brr10 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##

##

inter.brr.reg11 <- brr11 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(ineq.alpha), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##

inter.reg9<-do.call("cbind", lapply(inter.brr.reg9, '[[', 1))
inter.reg10<-do.call("cbind", lapply(inter.brr.reg10, '[[', 1))
inter.reg11<-do.call("cbind", lapply(inter.brr.reg11, '[[', 1))

rm(inter.brr.reg9);rm(inter.brr.reg10);rm(inter.brr.reg11)


##### GINI #####

Gini.brr.reg <- brr %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

##
gc()


Gini.brr.reg1 <- brr1 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()
##


Gini.brr.reg2 <- brr2 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()


Gini.brr.reg3 <- brr3 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()




Gini.brr.reg4 <- brr4 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))



gc()

reg.gini<-do.call("cbind", lapply(Gini.brr.reg, '[[', 1))
reg1.gini<-do.call("cbind", lapply(Gini.brr.reg1, '[[', 1))
reg2.gini<-do.call("cbind", lapply(Gini.brr.reg2, '[[', 1))
reg3.gini<-do.call("cbind", lapply(Gini.brr.reg3, '[[', 1))
reg4.gini<-do.call("cbind", lapply(Gini.brr.reg4, '[[', 1))

rm(Gini.brr.reg);rm(Gini.brr.reg1);rm(Gini.brr.reg2);rm(Gini.brr.reg3);rm(Gini.brr.reg4)

##

gc()

Gini.brr.reg5 <- brr5 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##

Gini.brr.reg6 <- brr6 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

###

Gini.brr.reg7 <- brr7 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##


Gini.brr.reg8 <- brr8 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

####

reg5.gini<-do.call("cbind", lapply(Gini.brr.reg5, '[[', 1))
reg6.gini<-do.call("cbind", lapply(Gini.brr.reg6, '[[', 1))
reg7.gini<-do.call("cbind", lapply(Gini.brr.reg7, '[[', 1))
reg8.gini<-do.call("cbind", lapply(Gini.brr.reg8, '[[', 1))

rm(Gini.brr.reg5);rm(Gini.brr.reg6);rm(Gini.brr.reg7);rm(Gini.brr.reg8)

gc()
##

Gini.brr.reg9 <- brr9 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one")) 

gc()

##

Gini.brr.reg10 <- brr10 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##

##

Gini.brr.reg11 <-brr11 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m+
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


gc()

##

reg9.gini<-do.call("cbind", lapply(Gini.brr.reg9, '[[', 1))
reg10.gini<-do.call("cbind", lapply(Gini.brr.reg10, '[[', 1))
reg11.gini<-do.call("cbind", lapply(Gini.brr.reg11, '[[', 1))

rm(Gini.brr.reg9);rm(Gini.brr.reg10);rm(Gini.brr.reg11)

###


##### GINI - interaction #####

inter.Gini.brr.reg <- brr %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


gc()


inter.Gini.brr.reg1 <- brr1 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


inter.Gini.brr.reg2 <- brr2 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()


inter.Gini.brr.reg3 <- brr3 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))


inter.Gini.brr.reg4 <- brr4 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))



gc()

reg.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg, '[[', 1))
reg1.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg1, '[[', 1))
reg2.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg2, '[[', 1))
reg3.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg3, '[[', 1))
reg4.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg4, '[[', 1))

rm(inter.Gini.brr.reg);rm(inter.Gini.brr.reg1);rm(inter.Gini.brr.reg2);rm(inter.Gini.brr.reg3);rm(inter.Gini.brr.reg4)

##

gc()

inter.Gini.brr.reg5 <- brr5 %>% 
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##

inter.Gini.brr.reg6 <- brr6 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

###

inter.Gini.brr.reg7 <-brr7 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##

inter.Gini.brr.reg8 <- brr8 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

####

reg5.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg5, '[[', 1))
reg6.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg6, '[[', 1))
reg7.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg7, '[[', 1))
reg8.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg8, '[[', 1))

rm(inter.Gini.brr.reg5);rm(inter.Gini.brr.reg6);rm(inter.Gini.brr.reg7);rm(inter.Gini.brr.reg8)

gc()
##


inter.Gini.brr.reg9 <- brr9 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))

gc()

##



inter.Gini.brr.reg10 <- brr10 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one")) 

gc()



inter.Gini.brr.reg11 <-brr11 %>%
  map (~ BIFIE.twolevelreg(BIFIEobj=., dep="MATH",
                           formula.fixed=~ HOMEPOS.m*
                             scale(Gini.HOMEPOS), 
                           formula.random=~ 1,
                           idcluster="CNTSCHID",
                           wgtlevel1="std.weight.final",
                           wgtlevel2 = "one"))
gc()

##

reg9.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg9, '[[', 1))
reg10.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg10, '[[', 1))
reg11.gini.inter<-do.call("cbind", lapply(inter.Gini.brr.reg11, '[[', 1))

rm(inter.Gini.brr.reg9);rm(inter.Gini.brr.reg10);rm(inter.Gini.brr.reg11)

##### DATA WRANGLING #####

variable.names(reg)
variable.names(reg1)
variable.names(reg2)
variable.names(reg3)
variable.names(reg4)
variable.names(reg5)
variable.names(reg6)
variable.names(reg7)
variable.names(reg8)
variable.names(reg9)
variable.names(reg10)
variable.names(reg11)

####

ineq.alpha.reg<-as.data.frame(reg[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg1<-as.data.frame(reg1[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg2<-as.data.frame(reg2[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.alpha.reg3<-as.data.frame(reg3[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.alpha.reg4<-as.data.frame(reg4[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg5<-as.data.frame(reg5[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60)])
ineq.alpha.reg6<-as.data.frame(reg6[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg7<-as.data.frame(reg7[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg8<-as.data.frame(reg8[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg9<-as.data.frame(reg9[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg10<-as.data.frame(reg10[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.alpha.reg11<-as.data.frame(reg11[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60,65,66,69)])

ineq.alpha.parameters<-cbind(ineq.alpha.reg,
                             ineq.alpha.reg1,
                             ineq.alpha.reg2,
                             ineq.alpha.reg3,
                             ineq.alpha.reg4,
                             ineq.alpha.reg5,
                             ineq.alpha.reg6,
                             ineq.alpha.reg7,
                             ineq.alpha.reg8,
                             ineq.alpha.reg9,
                             ineq.alpha.reg10,
                             ineq.alpha.reg11)

ineq.alpha.parameters.homepos<-as.data.frame(matrix(ineq.alpha.parameters[1,], 
                                            ncol=3, byrow=T))

colnames(ineq.alpha.parameters.homepos)[1]<-"Est.HOMEPOS"
colnames(ineq.alpha.parameters.homepos)[2]<-"Se.HOMEPOS"
colnames(ineq.alpha.parameters.homepos)[3]<-"pvalue.HOMEPOS"

name<-levels(pisaf.brr$CNT)

ineq.alpha.parameters.homepos$CNT<-name

ineq.alpha.parameters.alpha<-as.data.frame(matrix(ineq.alpha.parameters[2,], 
                                                    ncol=3, byrow=T))

colnames(ineq.alpha.parameters.alpha)[1]<-"Est.Alpha"
colnames(ineq.alpha.parameters.alpha)[2]<-"Se.Alpha"
colnames(ineq.alpha.parameters.alpha)[3]<-"pvalue.Alpha"


ineq.alpha.parameters.FINAL <- cbind(ineq.alpha.parameters.homepos[4],
                                     ineq.alpha.parameters.homepos[1:3],
                                     ineq.alpha.parameters.alpha)



ineq.alpha.parameters.FINAL[2:7] <- convertiblesToNumeric(ineq.alpha.parameters.FINAL[2:7])

###


gini.reg<-as.data.frame(reg.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg1<-as.data.frame(reg1.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg2<-as.data.frame(reg2.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33)])
gini.reg3<-as.data.frame(reg3.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33)])
gini.reg4<-as.data.frame(reg4.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg5<-as.data.frame(reg5.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60)])
gini.reg6<-as.data.frame(reg6.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg7<-as.data.frame(reg7.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg8<-as.data.frame(reg8.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg9<-as.data.frame(reg9.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
gini.reg10<-as.data.frame(reg10.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33)])
gini.reg11<-as.data.frame(reg11.gini[2:3,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60,65,66,69)])

gini.parameters<-cbind(gini.reg,
                             gini.reg1,
                             gini.reg2,
                             gini.reg3,
                             gini.reg4,
                             gini.reg5,
                             gini.reg6,
                             gini.reg7,
                             gini.reg8,
                             gini.reg9,
                             gini.reg10,
                             gini.reg11)

gini.parameters.homepos<-as.data.frame(matrix(gini.parameters[1,], 
                                                    ncol=3, byrow=T))

colnames(gini.parameters.homepos)[1]<-"Est.HOMEPOS"
colnames(gini.parameters.homepos)[2]<-"Se.HOMEPOS"
colnames(gini.parameters.homepos)[3]<-"pvalue.HOMEPOS"

gini.parameters.homepos$CNT<-name

gini.parameters.gini<-as.data.frame(matrix(gini.parameters[2,], 
                                                  ncol=3, byrow=T))

colnames(gini.parameters.gini)[1]<-"Est.Gini"
colnames(gini.parameters.gini)[2]<-"Se.Gini"
colnames(gini.parameters.gini)[3]<-"pvalue.Gini"


gini.parameters.FINAL <- cbind(gini.parameters.homepos[4],
                               gini.parameters.homepos[1:3],
                                        gini.parameters.gini)

gini.parameters.FINAL[2:7] <- convertiblesToNumeric(gini.parameters.FINAL[2:7])

ggplot(gini.parameters.FINAL) + geom_histogram(aes(pvalue.Gini))

#### interactions ####


ineq.alpha.reg.inter<-as.data.frame(inter.reg[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg1.inter<-as.data.frame(inter.reg1[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg2.inter<-as.data.frame(inter.reg2[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.alpha.reg3.inter<-as.data.frame(inter.reg3[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.alpha.reg4.inter<-as.data.frame(inter.reg4[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg5.inter<-as.data.frame(inter.reg5[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60)])
ineq.alpha.reg6.inter<-as.data.frame(inter.reg6[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg7.inter<-as.data.frame(inter.reg7[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg8.inter<-as.data.frame(inter.reg8[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg9.inter<-as.data.frame(inter.reg9[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.alpha.reg10.inter<-as.data.frame(inter.reg10[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.alpha.reg11.inter<-as.data.frame(inter.reg11[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60,65,66,69)])

ineq.alpha.parameters.inter<-cbind(ineq.alpha.reg.inter,
                             ineq.alpha.reg1.inter,
                             ineq.alpha.reg2.inter,
                             ineq.alpha.reg3.inter,
                             ineq.alpha.reg4.inter,
                             ineq.alpha.reg5.inter,
                             ineq.alpha.reg6.inter,
                             ineq.alpha.reg7.inter,
                             ineq.alpha.reg8.inter,
                             ineq.alpha.reg9.inter,
                             ineq.alpha.reg10.inter,
                             ineq.alpha.reg11.inter)

ineq.alpha.parameters.homepos.inter<-as.data.frame(matrix(ineq.alpha.parameters.inter[1,], 
                                                    ncol=3, byrow=T))

colnames(ineq.alpha.parameters.homepos.inter)[1]<-"Est.HOMEPOS"
colnames(ineq.alpha.parameters.homepos.inter)[2]<-"Se.HOMEPOS"
colnames(ineq.alpha.parameters.homepos.inter)[3]<-"pvalue.HOMEPOS"

ineq.alpha.parameters.homepos.inter$CNT<-name

ineq.alpha.parameters.alpha.inter<-as.data.frame(matrix(ineq.alpha.parameters.inter[2,], 
                                                  ncol=3, byrow=T))

colnames(ineq.alpha.parameters.alpha.inter)[1]<-"Est.Alpha"
colnames(ineq.alpha.parameters.alpha.inter)[2]<-"Se.Alpha"
colnames(ineq.alpha.parameters.alpha.inter)[3]<-"pvalue.Alpha"

ineq.alpha.parameters.inter.inter<-as.data.frame(matrix(ineq.alpha.parameters.inter[3,], 
                                                        ncol=3, byrow=T))

colnames(ineq.alpha.parameters.inter.inter)[1]<-"Est.Interac"
colnames(ineq.alpha.parameters.inter.inter)[2]<-"Se.Interac"
colnames(ineq.alpha.parameters.inter.inter)[3]<-"pvalue.Interac"


ineq.alpha.parameters.inter.FINAL <- cbind(ineq.alpha.parameters.homepos.inter[4],
                                     ineq.alpha.parameters.homepos.inter[1:3],
                                     ineq.alpha.parameters.alpha.inter,
                                     ineq.alpha.parameters.inter.inter)



ineq.alpha.parameters.inter.FINAL[2:10] <- convertiblesToNumeric(ineq.alpha.parameters.inter.FINAL[2:10])


ggplot(ineq.alpha.parameters.inter.FINAL) + geom_histogram(aes(pvalue.Interac))

###


ineq.gini.reg.inter<-as.data.frame(reg.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg1.inter<-as.data.frame(reg1.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg2.inter<-as.data.frame(reg2.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.gini.reg3.inter<-as.data.frame(reg3.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.gini.reg4.inter<-as.data.frame(reg4.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg5.inter<-as.data.frame(reg5.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60)])
ineq.gini.reg6.inter<-as.data.frame(reg6.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg7.inter<-as.data.frame(reg7.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg8.inter<-as.data.frame(reg8.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg9.inter<-as.data.frame(reg9.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51)])
ineq.gini.reg10.inter<-as.data.frame(reg10.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33)])
ineq.gini.reg11.inter<-as.data.frame(reg11.gini.inter[2:4,c(2,3,6,11,12,15,20,21,24,29,30,33,38,39,42,47,48,51,56,57,60,65,66,69)])

ineq.gini.parameters.inter<-cbind(ineq.gini.reg.inter,
                                   ineq.gini.reg1.inter,
                                   ineq.gini.reg2.inter,
                                   ineq.gini.reg3.inter,
                                   ineq.gini.reg4.inter,
                                   ineq.gini.reg5.inter,
                                   ineq.gini.reg6.inter,
                                   ineq.gini.reg7.inter,
                                   ineq.gini.reg8.inter,
                                   ineq.gini.reg9.inter,
                                   ineq.gini.reg10.inter,
                                   ineq.gini.reg11.inter)

ineq.gini.parameters.homepos.inter<-as.data.frame(matrix(ineq.gini.parameters.inter[1,], 
                                                          ncol=3, byrow=T))

colnames(ineq.gini.parameters.homepos.inter)[1]<-"Est.HOMEPOS"
colnames(ineq.gini.parameters.homepos.inter)[2]<-"Se.HOMEPOS"
colnames(ineq.gini.parameters.homepos.inter)[3]<-"pvalue.HOMEPOS"

ineq.gini.parameters.homepos.inter$CNT<-name

ineq.gini.parameters.gini.inter<-as.data.frame(matrix(ineq.gini.parameters.inter[2,], 
                                                        ncol=3, byrow=T))

colnames(ineq.gini.parameters.gini.inter)[1]<-"Est.Gini"
colnames(ineq.gini.parameters.gini.inter)[2]<-"Se.Gini"
colnames(ineq.gini.parameters.gini.inter)[3]<-"pvalue.Gini"

ineq.gini.parameters.inter.inter<-as.data.frame(matrix(ineq.gini.parameters.inter[3,], 
                                                        ncol=3, byrow=T))

colnames(ineq.gini.parameters.inter.inter)[1]<-"Est.Interac"
colnames(ineq.gini.parameters.inter.inter)[2]<-"Se.Interac"
colnames(ineq.gini.parameters.inter.inter)[3]<-"pvalue.Interac"


ineq.gini.parameters.inter.FINAL <- cbind(ineq.gini.parameters.homepos.inter[4],
                                           ineq.gini.parameters.homepos.inter[1:3],
                                           ineq.gini.parameters.gini.inter,
                                           ineq.gini.parameters.inter.inter)



ineq.gini.parameters.inter.FINAL[2:10] <- convertiblesToNumeric(ineq.gini.parameters.inter.FINAL[2:10])

ggplot() + geom_point(data=subset(ineq.alpha.parameters.FINAL,pvalue.Alpha<.05),
                      aes(CNT,Est.Alpha,colour="Alpha"))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  geom_point(data=subset(gini.parameters.FINAL,pvalue.Gini<.05),
             aes(CNT,Est.Gini,colour="Gini"))
  
  
ggplot() + geom_point(data=subset(ineq.alpha.parameters.inter.FINAL,pvalue.Alpha<.05),
                      aes(CNT,Est.Interac,colour="Alpha"))+
  scale_x_discrete(guide = guide_axis(n.dodge=3))+
  geom_point(data=subset(ineq.gini.parameters.inter.FINAL,pvalue.Gini<.05),
             aes(CNT,Est.Interac,colour="Gini"))

panderOptions('round',3)

options(scipen = 3,digits = 3)
library(xtable)

xtable(ineq.alpha.parameters.FINAL)

pander(ineq.alpha.parameters.FINAL,split.table=Inf,digits=3)

pander(gini.parameters.FINAL,split.table=Inf)

pander(ineq.alpha.parameters.inter.FINAL,split.table=Inf)

pander(ineq.gini.parameters.inter.FINAL,split.table=Inf)

i<-ineq.alpha.parameters.FINAL %>% filter(Est.Alpha>0) 
i

g<-gini.parameters.FINAL %>% filter (Est.Gini>0) 

g
###

variable.names(pisaf.brr)

tod<-BIFIE.data.jack(data=pisaf.brr, wgt="std.weight.final",
                     jktype="RW_PISA", 
                     pvpre =paste0("PV",1:10), fayfac=.05,
                     wgtrep=paste0("W_FSTURWT",1:80),
                     cdata=F)

####


descrip.cnt<-BIFIE.univar(tod,vars=c("ineq.alpha","Gini.HOMEPOS"),
                      group="CNT")

descrip2<-descrip.cnt$stat_M
descrip3<-descrip.cnt$stat_SD

desc<-cbind(name,descrip2[,c(1,6)],descrip3[6])

desc$cv<-desc$SD/desc$M

variable.names(desc)

ggplot(data=desc,(aes(M,cv,label=name,colour=var)))+
  geom_point() + facet_wrap(~var,scales = "free") + 
  geom_text(check_overlap = F,size=3,vjust = 2)

write.csv(desc,"descbrr.csv")

desc.i<-desc%>%slice_head(n=69)
desc.g<-desc%>%slice_tail(n=69)
desf<-cbind(desc.i[,c(1,3,5)],desc.g[,c(3,5)])
variable.names(desf)
colnames(desf)[1]<-"Country"
colnames(desf)[2]<-"Mean Ineq Alpha"
colnames(desf)[3]<-"CV Ineq Alpha"
colnames(desf)[4]<-"Mean Gini"
colnames(desf)[5]<-"CV Gini"

stargazer(desf,type="text",summary = F)




gc()
####



descrip<-BIFIE.univar(tod,vars="MATH",
                      group=c("CNT","CNTSCHID"))

summary(descrip)

descrip2.s<-descrip$stat_M
descrip3.s<-descrip$stat_SD

variable.names(descrip2.s)
variable.names(descrip3.s)
head(descrip2.s)

desc.s<-cbind(descrip2.s[,c(1,3,5,8)],descrip3.s[8])

c.p<-pisaf.brr %>% 
  group_by(CNT) %>% 
  summarise(n1=n_distinct(CNTSCHID)) %>%
  print (n=70)

c.d<-desc.s  %>% 
  group_by(cnt) %>% 
  summarise(n2=n_distinct(groupval2)) %>%
  print (n=70)

cc<-cbind(c.p,c.d)
cc <- cc %>% mutate (igual = case_when(n1==n2 ~ "yes",
                                       T ~ "no"))
cc


colnames(desc.s)[2]<-"cnt"

Ineq.school.tod <- Ineq.school.tod %>% arrange(CNT,CNTSCHID)

desc.INEQ.schoools<-desc.s %>%
  arrange(cnt,groupval2) %>% bind_cols(Ineq.school.tod)

desc.INEQ.schoools$cv<-desc.INEQ.schoools$SD/desc.INEQ.schoools$M

variable.names(desc.INEQ.schoools)
head(desc.INEQ.schoools)

###
ineq.country5<-desc.INEQ.schoools%>%
  group_by(CNT)%>%
  slice_max(order_by=ineq.alpha,prop=.2)

ineq.country5$t20<-1
head(ineq.country5)

ineq.country6<-desc.INEQ.schoools%>%group_by(CNT)%>%
  slice_min(order_by=ineq.alpha,prop=.2)  

ineq.country6$t20<-0

ineq.country7<-rbind(ineq.country6,ineq.country5)

ineq.country7$t20<-as.factor(ineq.country7$t20)

ttest<-by(ineq.country7,ineq.country7$CNT, 
          function (x) t.test(x$M ~x$t20))

ineq.country7%>%filter(CNT=="USA")%>%ggplot()+
  geom_point(aes(x=ineq.alpha,y=M,colour=t20))

ineq.country7%>%ggplot()+
  geom_point(aes(x=Gini.HOMEPOS,y=M,colour=t20))+
  facet_wrap(~CNT,scales = "free")


ttest

head(ineq.country7)

###

ineq.country5b<-desc.INEQ.schoools%>%
  group_by(CNT)%>%
  slice_max(order_by=Gini.HOMEPOS,prop=.2)

ineq.country5b$t20<-1
head(ineq.country5b)

ineq.country6b<-desc.INEQ.schoools%>%group_by(CNT)%>%
  slice_min(order_by=Gini.HOMEPOS,prop=.2) 

ineq.country6b$t20<-0

ineq.country7b<-rbind(ineq.country6b,ineq.country5b)

ineq.country7b$t20<-as.factor(ineq.country7b$t20)

ttest.b<-by(ineq.country7b,ineq.country7b$CNT, 
            function (x) t.test(x$M ~x$t20))

ttest.b

####

ineq.country7b%>%filter(CNT=="USA")%>%ggplot()+
  geom_point(aes(x=Gini.HOMEPOS,y=M,colour=t20))

ineq.country7b%>%ggplot()+
  geom_point(aes(x=Gini.HOMEPOS,y=M,colour=t20)) + 
  facet_wrap(~CNT,scales = "free")


#########
variable.names(desc.INEQ.schoools)

avm<-desc.INEQ.schoools %>% group_by(CNT) %>%
  filter (ineq.alpha > 1) %>% tally()

avmen<-desc.INEQ.schoools %>% group_by(CNT) %>%
  filter (ineq.alpha < 1) %>% tally()

aversc<-cbind(avm,avmen[2])
colnames(aversc)[2]<-"mas"
aversc$Proportion<-round(100*(aversc$mas/(aversc$mas+aversc$n)),2)
colnames(aversc)[2]<-"# schools > national average"
colnames(aversc)[3]<-"# schools < national average"
colnames(aversc)[4]<-"Proportion schools > national average"

stargazer(aversc,summary = F,type = "text")

aversc %>% filter (`Proportion schools > national average` >35)
aversc %>% filter (`Proportion schools > national average` <5)
