

### Not run


### regression discontinuity


library(haven)

hw10data <- read_stata("AEJfigs.dta")

summary(hw10data)


summary(hw10data$agecell)
table(hw10data$agecell)


library(car)
hw10data$over21 <-  recode(hw10data$agecell, "21:23=1;else=0")


m1 <- lm(all ~ agecell+over21+ agecell*over21,
        data = hw10data)

 



install.packages("broom.mixed")
library(coefplot)

coefplot(m1, intercept=FALSE)

library(jtools)

plot_summs(m1, scale=TRUE, n.sd=2)


summary(m1)


library(texreg)
screenreg(list(m1))


effect_plot(m1, pred=over21, force.cat=TRUE)

effect_plot(m1, pred=agecell, interval=TRUE,
  rug=TRUE)



install.packages("interactions")
library(interactions)

interact_plot(m1, pred=agecell, modx=over21,
    plot.points = TRUE, interval = TRUE,
  linearity.check = TRUE)


johnson_neyman(m1, pred=agecell, modx=over21)
johnson_neyman(m1, pred=over21, modx=agecell)



plot_model(m1, type="std2")

plot_model(m1, type="pred", 
     terms=c("agecell","over21"))+theme_bw()




summary(hw10data$over21)
table(hw10data$over21)



plot(hw10data$agecell,hw10data$all, 
   pch=10, frame.plot = FALSE,
   xaxs="i",yaxs="i",
   xlim=c(19,23), ylim=c(85,110),
   main = "Flexible RD regression : \nFitted line for subsamples",
   xlab = "Age", ylab =" 'All' Deaths ", 
   cex.main=1, font.main=1)

###
