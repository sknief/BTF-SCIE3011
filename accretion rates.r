### accretion rates ###

#note: sedimentation rates are not determined via R but manually calculated

################# Start Commands ####################

attach(Legit_data)
library(ggplot2)

##################### TOTAL Accretion rates BY LOCATION ######################

model <- lm(`MSL (depth m)`~ `Median age (cal BP)` * `Apron Location`)

summary(model)

anova(model) # check how valid this is

plot(model) # not very

coef(model)  ##this gives me the slopes

### how to use the coeff output for calculate intercept and slopes
## 1. intercept = "intercept output" + categorical output
## 2. slope = covariate + interaction value
## for example see line 35 and output of line 20

#our model is now a variable called plot to allow for better ggplot coding
plot <- ggplot(data=Legit_data,aes(x=`Median age (cal BP)`,y=`MSL (depth m)`,col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm")

plot

#check the equations we derived by plotting lines on top

plot + geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "blue", slope = -0.0011001466, intercept = -1.0846337974) +
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "yellow", slope = -0.0011+0.000717, intercept = -1.0846337974+(-0.2868)) +
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "red", slope = -0.0011+0.000935, intercept = -1.0846337974+0.2935)

##################### SPLIT Accretion rates BY LOCATION ######################

#### subsetting ####

above2000 <- subset(Legit_data, `Median age (cal BP)` > 2000)
below2000 <- subset(Legit_data, `Median age (cal BP)` < 2000)

### plots ####
#total set as a reference graph
ggplot(data=Legit_data,aes(x=`Median age (cal BP)`,y=`MSL (depth m)`,col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()

#4000-2000 BP
ggplot(data=above2000,aes(x=`Median age (cal BP)`,y=`MSL (depth m)`,col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()

#2000 BP -present
ggplot(data=below2000,aes(x=`Median age (cal BP)`,y=`MSL (depth m)`,col= `Apron Location`))+
  geom_point()+geom_smooth(method="lm") + theme_classic()

### accretion rates ###

a2000model <- lm(data = above2000, `MSL (depth m)`~ `Median age (cal BP)` * `Apron Location`)
b2000model <- lm(data = below2000, `MSL (depth m)`~ `Median age (cal BP)` * `Apron Location`)

summary(a2000model)
summary(b2000model)

#rates determined as described in line 22

##################### TOTAL Accretion rates BY CORE NO ######################

#create a new model to obtain coeff values

cores <- lm(`MSL (depth m)`~ `Median age (cal BP)` * `Core No.`)

coef(cores)

#plot
coreplot <- ggplot(data=Legit_data,aes(x=`Median age (cal BP)`,y=`MSL (depth m)`,col= `Core No.`))+
  geom_point()+geom_smooth(method="lm")
coreplot

#check derived eqns
coreplot + geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  -1.809539e-04, intercept = -6.707545e-01) + #c12
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 + -2.378768e-04), intercept = (-6.707545e-01 + -6.549386e-01)) + #c14
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 + -3.559225e-04), intercept = (-6.707545e-01 + -2.114731e-01)) + #c15
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 +  -5.853539e-04), intercept = (-6.707545e-01 +  -1.230625e+00)) + #c18
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 + -2.099924e-03), intercept = (-6.707545e-01 + 1.587930e-03 )) + #c3
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 + 7.486354e-05), intercept = (-6.707545e-01 + -2.115913e-01)) + #c5
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 +  -2.257031e-05), intercept = (-6.707545e-01 + -2.023468e-01)) + #c6
  geom_abline(aes (inherit.aes = TRUE), data = Legit_data, colour = "black", slope =  (-1.809539e-04 + -4.865368e-04), intercept = (-6.707545e-01 + -1.738487e-01)) #c8
