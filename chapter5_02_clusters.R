library(factoextra)
library(patchwork)
library(tidyverse)

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

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#

str(pisa2018$Area)

LA.cluster <- pisa2018 %>% filter (CNT != "VNM") %>%
  mutate(Area = as.numeric(Area),
         Tipo.escola = as.numeric (Tipo.escola))%>%
  select(CNT,PV1READ,HOMEPOS,school.HOMEPOS,ineq.alpha,mean.A,std.weight.final,Area,Tipo.escola) %>%
  group_by(CNT) %>%
  summarise(
    # PV1READ=matrixStats::weightedMean(PV1READ,na.rm = T,
    #                                   w=std.weight.final),
    HOMEPOS=matrixStats::weightedMean(HOMEPOS,na.rm = T,
                                      w=std.weight.final),
    school.HOMEPOS=matrixStats::weightedMean(school.HOMEPOS,na.rm = T,
                                             w=std.weight.final),
    ineq.alpha=matrixStats::weightedMean(ineq.alpha,na.rm = T,
                                         w=std.weight.final),
    mean.A=mean(mean.A),
    Area=matrixStats::weightedMean(Area,na.rm = T,
                                   w=std.weight.final),
    Tipo.escola=matrixStats::weightedMean(Tipo.escola,na.rm = T,
                                          w=std.weight.final))

summary(LA.cluster$Tipo.escola)

scaled_data <- LA.cluster %>%
  mutate(
    ineq.alpha = scale_this(ineq.alpha),
    mean.A = scale_this(mean.A),
    Area= scale_this(Area),
    Tipo.escola= scale_this(Tipo.escola),
    school.HOMEPOS=scale_this(school.HOMEPOS)
  )

scaled_data <- scaled_data %>%
  remove_rownames %>%
  column_to_rownames(var="CNT") %>% na.omit

fviz_nbclust(scaled_data, kmeans, method = "wss")
fviz_nbclust(scaled_data, kmeans, method = "silhouette")
fviz_nbclust(scaled_data, kmeans, method = "gap_stat")


k2 <- kmeans(scaled_data, centers = 2, nstart = 25)
k3 <- kmeans(scaled_data, centers = 3, nstart = 25)
k4 <- kmeans(scaled_data, centers = 4, nstart = 25)
k5 <- kmeans(scaled_data, centers = 5, nstart = 25)

p2<-fviz_cluster(k2, data = scaled_data) + theme_light()
p3<-fviz_cluster(k3, data = scaled_data)+ theme_light()
p4<-fviz_cluster(k4, data = scaled_data)+ theme_light()
p5<-fviz_cluster(k5, data = scaled_data)+ theme_light()

p2+p3+p4+p5

k <-tibble::enframe(k3$cluster) %>% rename (country=name)

library("learningtower")
data(countrycode)

k <- k %>% left_join(countrycode)


cor(LA.cluster$ineq.alpha,LA.cluster$mean.A)

write.csv(k,"k6.csv")

LA.cluster<-LA.cluster %>%
  left_join(k,by=c("CNT"="country")) %>% rename(cluster=value)

library(forcats)

p6<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(PV1READ))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=PV1READ,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light ()+
  theme( axis.text.x = element_text(size = 12))+
  xlab("")


p7<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(ineq.alpha))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=ineq.alpha,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light () +
  theme( axis.text.x = element_text(size = 12))+
  xlab("") + ylab("Alpha Inequality")


p8<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(mean.A))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=mean.A,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light ()+
  theme( axis.text.x = element_text(size = 12))+
  xlab("")  + ylab("Country Segregation")


p9<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(HOMEPOS))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=HOMEPOS,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light ()+
  theme( axis.text.x = element_text(size = 12))+
  xlab("")


p10<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(school.HOMEPOS))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=school.HOMEPOS,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light ()+
  theme( axis.text.x = element_text(size = 12))+
  xlab("")


p11<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(Tipo.escola))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=Tipo.escola,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light ()+
  theme( axis.text.x = element_text(size = 12))+
  xlab("")


p12<- LA.cluster %>% mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(Area))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=Area,colour=cluster),size=2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light ()+
  theme( axis.text.x = element_text(size = 12))+
  xlab("")

p6 # 1600 x 800
p7 / p8
p9 / p10
p11 / p12

##

shapes  <- c("s1" = 16, "s2" = 17)

LA.cluster %>%
  mutate(cluster=as.factor(cluster)) %>%
  filter (!is.na(cluster)) %>%
  mutate(CNT = fct_reorder(CNT, desc(mean.A))) %>%
  ggplot() +
  geom_point(aes(x=CNT,y=mean.A,colour=cluster,
                 shape="Country Segregation"),size=5)+
  geom_point(aes(x=CNT,y=ineq.alpha/1.5,colour=cluster,
                 shape="Alpha Inequality"),size=5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_light () +
  theme( axis.text.x = element_text(size = 14))+
  xlab("") + ylab("") +
  scale_shape_discrete("Variables")+
  scale_colour_discrete("Clusters")+
  guides(shape = guide_legend(override.aes = list(colour = "darkorange")))+
  theme(axis.text.y = element_blank(),legend.text = element_text(size=14),
        panel.background=element_blank(),panel.grid.major.y = element_blank(),
        panel.grid.major=element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        legend.position = "bottom")




###
