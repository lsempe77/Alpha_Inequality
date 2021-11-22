
library(tidyverse)

flex<-LA %>% select(CNT,Sex,AGE,REPEAT,HOMEPOS,school.HOMEPOS,BELONG,PERCOOP,PERSPECT,
              GLOBMIND,RESPECT,PV1READ:PV10READ,Area,
              Tipo.escola,ineq.alpha,Gini) %>%
  mutate(Sex=as.factor(Sex),
         REPEAT=as.factor(REPEAT),
         Area=as.factor(Area),
         Tipo.escola=as.factor(Tipo.escola)
         )%>%
  gtsummary::tbl_summary(#by = CNT,
                         missing = "no") %>%
  gtsummary::as_flex_table()

flextable::save_as_docx(flex,
                        path="C:/Users/LUCAS/Desktop/Chapter 3 Mediation/table1ftodos.docx")


m.i<-matrixStats::weightedMean(LA$ineq.alpha,w=LA$std.weight.final)
sd.i<-matrixStats::weightedSd(LA$ineq.alpha,w=LA$std.weight.final)

library(forcats)


ineq.av.cnt<-LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT) %>%
  summarise(ineq.country.ave = matrixStats::weightedMedian(ineq.alpha,na.rm = T,w=std.weight.final))

LA %>% mutate(CNTSCHID = as.factor(CNTSCHID)) %>%
  group_by(CNT,CNTSCHID) %>%
  summarise(ineq=matrixStats::weightedMean(ineq.alpha,na.rm = T,w=std.weight.final)) %>%
  distinct() %>% left_join(ineq.av.cnt) %>% ungroup %>%
  mutate(CNT = forcats::fct_reorder(CNT, desc(ineq.country.ave))) %>%
  ggplot() +  geom_boxplot(aes(CNT,ineq)) + xlab("Countries") + ylab ("School inequality") +
  theme_minimal() + geom_hline(aes(yintercept=m.i),linetype=2,colour="darkorange")+
  geom_hline(aes(yintercept=m.i+sd.i),linetype=2,colour="darkgreen")+
  geom_hline(aes(yintercept=m.i-sd.i),linetype=2,colour="darkgreen")+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))


##

library(factoextra)


scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#

LA.cluster<- LA %>% filter (CNT != "VNM") %>%
  select(CNT,PV1READ,HOMEPOS,school.HOMEPOS,ineq.alpha,mean.A,std.weight.final,Area,Tipo.escola) %>%
  group_by(CNT) %>%
  summarise(
    PV1READ=matrixStats::weightedMean(PV1READ,na.rm = T,
                                               w=std.weight.final),
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
    PV1READ = scale_this(PV1READ),
         ineq.alpha = scale_this(ineq.alpha),
         mean.A = scale_this(mean.A),
         Area= scale_this(Area),
         Tipo.escola= scale_this(Tipo.escola),
    school.HOMEPOS=scale_this(school.HOMEPOS)
    )

scaled_data <- scaled_data %>%
  remove_rownames %>%
  column_to_rownames(var="CNT") %>% na.omit

?fviz_nbclust

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

library(patchwork)

p2+p3+p4+p5

p3

k <-tibble::enframe(k3$cluster) %>% rename (country=name)

library("learningtower")
data(countrycode)
View(countrycode)

k <- k %>% left_join(countrycode)


cor(LA.cluster$ineq.alpha,LA.cluster$mean.A)

write.csv(k,"k6.csv")

LA.cluster<-LA.cluster %>% left_join(k,by=c("CNT"="country")) %>% rename(cluster=value)

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


d0<-dist(scaled_data)

# Hierarchical Agglomerative Clustering
h1=hclust(d0,method='average')
h2=hclust(d0,method='complete')
h3=hclust(d0,method='ward.D')
h4=hclust(d0,method='single')

# Cophenetic Distances, for each linkage
c1=cophenetic(h1)
c2=cophenetic(h2)
c3=cophenetic(h3)
c4=cophenetic(h4)

# Correlations
cor(d0,c1) # 0.7339946
cor(d0,c2) # 0.6951253
cor(d0,c3) # 0.5865711
cor(d0,c4) # 0.5784441

# Dendograms
par(mar=c(1,1,1,1))
plot(h1,main='Average Linkage')
plot(h2,main='Complete Linkage')
plot(h3,main='Ward Linkage')
plot(h4,main='Single Linkage')
par(mfrow=c(1,1))

#
GDP2015 <- read_excel("C:/Users/LUCAS/Desktop/PISA INEQUALITY/R PISA 0/GDP2015.xls")

GDP2015$...5<-NULL

summary(h3)

h3c<-cutree(h3, k = 3)

fviz_cluster(list(data = d0, cluster = h3c))  ## from ‘factoextra’ package



h3c<-tibble::enframe(h3c) %>% rename (CNT=name)

gdp.h<-GDP2015 %>% left_join(h3c)

ggplot(gdp.h) + geom_point(aes(y=`GNI per capita`,x=value))

ggplot(gdp.h) + geom_point(aes(y=`Gini index`,x=value))

#


k2 <- kmeans(scaled_data, centers = 2, nstart = 100)
k3 <- kmeans(scaled_data, centers = 3, nstart = 100)
k4 <- kmeans(scaled_data, centers = 4, nstart = 100)
k5 <- kmeans(scaled_data, centers = 5, nstart = 100)


k <-tibble::enframe(k3$cluster) %>% rename (CNT=name)

gdp.k<-GDP2015 %>% left_join(k)

ggplot(gdp.k) + geom_point(aes(y=`GNI per capita`,x=value))

ggplot(gdp.k) + geom_point(aes(y=`Gini index`,x=value))

k %>% group_by(value) %>% arrange (value) %>% print (n=80)

