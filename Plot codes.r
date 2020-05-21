#### Plot codes #####

#### Start commands ####

attach(Legit_data)
library(ggplot2)


#### #1 Plots ####
#accretion rates
ggplot(data=Legit_data,aes(x=`Median age (cal BP)`,y=`MSL (depth m)`,col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()

#age vs foram
ggplot(data=Legit_data,aes(x=`Median age (cal BP)`,y=`Foram percentage`,col=`Core No.`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()  #by core

ggplot(data=Legit_data,aes(x=`Median age (cal BP)`,y=`Foram percentage`, col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm") + theme_classic() + coord_cartesian(ylim = (c(0,40)))  #by location

#xpd vs foram
ggplot(data=Legit_data,aes(x= Xpd ,y=`Foram percentage`, col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()  #by location

ggplot(data=Legit_data,aes(x= Xpd ,y=`Foram percentage`, col= `Core No.`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()  #by core

ggplot(data=Legit_data,aes(x= Xpd ,y=`Foram percentage`, col= `Core No.`))+
  geom_boxplot() + theme_classic()  #BOXPLOT by core

ggplot(data=Legit_data,aes(x= Xpd ,y=`Foram percentage`, col= `Core No.`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()  #by core



#### #3 plots #####

head(Legit_data, 3)

LD.pca <- prcomp(Legit_data[, (c(4:5,7:9))], center = TRUE, scale. = TRUE)

print(LD.pca)
# note: rotations are loadings

plot(LD.pca, type = "lines")
plot(LD.pca, type = "barplot")
#note: first three explain a lot

summary(LD.pca)
#wow, cumulative proportion of PC3 is 0.9815, good enough; even 2 would work (0.8312)

biplot(LD.pca) #nice plot !!


#lets gg this
install.packages("ggfortify")
library("ggfortify")

autoplot(LD.pca, data = Legit_data)

plot(LD.pca$x[,1:2], pch=20, col=Legit_data$Colours) #addlabels

base <- autoplot(prcomp(Legit_data[, (c(4:5,7:9))], center = TRUE, scale. = TRUE), data = Legit_data)
base
