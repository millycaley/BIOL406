# Analysis regeneration across canopy cover and nfocal species cover
# Authors: H. Southam
# Date: 27 March 2020

#load required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(MASS)
library(wesanderson)
library(gridExtra)
library(cowplot)

#first downloaded as excel file, removed notes and metadata and coverted to csv file.
#read data
Stumps <- read.csv(file = "./data/stumpdata_analysis.csv", header = TRUE)
summary(Stumps)
head(Stumps)
dim(Stumps)

#first create three new columns: regen_seed, regen_sap, regen_total
Stumps$regen_seed = Stumps[,11]+Stumps[,13]+Stumps[,15]
Stumps$regen_sap = Stumps[,12]+Stumps[,14]+Stumps[,16]
Stumps$regen_total <- apply(Stumps[,11:15], 1, sum)

#convert to long format
Stumps <- gather(Stumps, regen_type, regen, 17:19)

#Create area column from diameter in (m2)
Stumps$area <- pi*(Stumps$diameter/200)^2
#Create a colum for density: 
Stumps$density <- Stumps$regen/(pi*(Stumps$diameter/200)^2)

# check visually relationships between these three regen variable and continuous variables (diameter, canopy cover, nfocal cover)
pairs(~regen+diameter+canopy_cover_perc+nfocalspecies_cover, data=Stumps)

#the data is left skewd (typical of count data) so linear models based on and underlying normal distribution won't work. Two options: Poisson dist or negative binomial distribution. 

# Make 3 seperate datframes to use in the GLM: regen_total, regen_sap, regen_seed (basically the reverse ow what we just did). Stumps_total is used in model.
Stumps_total <- filter(Stumps, Stumps$regen_type=="regen_total") 
Stumps_sap <- filter(Stumps, Stumps$regen_type=="regen_sap") 
Stumps_seed <- filter(Stumps, Stumps$regen_type=="regen_seed") 

#MODELS
#Compare Poisson and negative bionmial distributions. For comparison, just use the regen_total. 
#Poisson. Quasi poissson is used to test for overdispersion (is the mean=variance). 
# regular poisson
m_stump_total_p <- glm(regen~canopy_cover_perc+diameter+nfocalspecies_cover, Stumps_total, family=poisson)
summary(m_stump_total_p)

#quasi-poisson. Dispersion parameter = 2.923 = overdispersion. So we shoud use the negative bionomial. 
m_stump_total_qp <- glm(regen~canopy_cover_perc+diameter+nfocalspecies_cover, Stumps_total, family=quasipoisson)
summary(m_stump_total_qp)

#Negative bionomial. # Deviance is a measure of badness of fit. Null deviance is the deviance in the null model. Residual deviance is the deviance after you have added variables. The greater the reduction from null to residual the better the fit.
m_stump_total_nb <- glm.nb(regen~canopy_cover_perc+nfocalspecies_cover+diameter, Stumps_total)
summary(m_stump_total_nb)

#But if we follow Orman et al (2016) we have to add an offset variable --> essentially it turns the response into a proportion. For us that is diameter.This is the final model. Make the same model for regen_sap and regen_seed.  
#regen_total:
m_stump_total_nb <- glm.nb(regen~canopy_cover_perc+nfocalspecies_cover+offset(log(area)), Stumps_total, control=glm.control(maxit=50))
summary(m_stump_total_nb)

#PLOTS 
#Plot with legend 
p_regen_cc <- ggplot(Stumps, aes(canopy_cover_perc*100, log(density), color=regen_type))+geom_point()+geom_smooth(method=lm, se=FALSE)+theme_classic()+labs(x="canopy cover (%)", y="log(density(# plants per m^2))")+scale_color_manual(name="Regeneration Category", labels=c("saplings", "seedlings", "total (saplings+seedlings)"), values=c("regen_total"="black", "regen_sap"="blue", "regen_seed"="red"))+theme(legend.position="bottom")

#save legend
legend <- get_legend(p_regen_cc)

#without legend
p_regen_cc <- ggplot(Stumps, aes(canopy_cover_perc*100, log(density), color=regen_type))+geom_point()+geom_smooth(method=lm, se=FALSE)+theme_classic()+labs(x="canopy cover (%)", y="log(density(# plants per m^2))")+scale_color_manual(name="Regeneration Category", labels=c("saplings", "seedlings", "total (saplings+seedlings)"), values=c("regen_total"="black", "regen_sap"="blue", "regen_seed"="red"))+theme(legend.position="None")

p_regen_nfsc <- ggplot(Stumps, aes(nfocalspecies_cover, log(density), color=regen_type))+geom_point()+geom_smooth(method=lm, se=FALSE)+theme_classic()+labs(x="non-focal species cover (%)", y="log(density(# plants per m^2))")+scale_color_manual(name="Regeneration Category", labels=c("saplings", "seedlings", "total (saplings+seedlings)"), values=c("regen_total"="black", "regen_sap"="blue", "regen_seed"="red"))+theme(legend.position="None")

#Put them all together into a single figure: 
jpeg(filename="figures_tables/p_regen_combined.jpeg",width=12,height=8,units="in",res=500)
lay <- rbind(c(1,2))
grid.arrange(p_regen_cc, p_regen_nfsc, layout_matrix=lay, bottom=legend)
dev.off()

