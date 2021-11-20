library(mirt)
dat <- expand.table(LSAT7)
mod <- mirt(dat, 1, verbose=FALSE)



# Extract all items 
# Compute the probability trace lines
# Put into a list
traceline <- NULL
for(i in 1:length(dat)){
  extr.2 <- extract.item(mod, i)
  Theta <- matrix(seq(-4,4, by = .1))
  traceline[[i]] <- probtrace(extr.2, Theta)
}

# rename list
names(traceline) <- paste('item',1:length(traceline))

# rbind traceline
traceline.df <- do.call(rbind, traceline)

# create item names length based on length of theta provided
item <- rep(names(traceline),each=length(Theta))

# put them all together into a dataframe
l.format <- cbind.data.frame(Theta, item, traceline.df)


l.format$item<-as.factor(l.format$item)

aux<-l.format %>%
  group_by(item) %>%
  slice(which.min(abs(P.1-0.5))) # We are only using the P.1 column (dichotomous)

aux<-aux[order(aux$Theta),]
ord<-as.integer(aux$item)
l.format$item = factor(l.format$item,levels(l.format$item)[ord])

# plot chart
ggplot(subset(l.format,item!="item 4" & item!="item 5"), aes(Theta, P.1, 
                                                             colour = item)) + 
  geom_line() + 
  xlab(expression(theta)) + 
  ylab(expression(P(theta))) + 
  geom_hline(aes(yintercept = 0.5),linetype=2,colour="darkorange") +
  theme_bw() + 
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank())
