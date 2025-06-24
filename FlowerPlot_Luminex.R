#version FINAL Correct
library(tidyverse)
library(readxl)
library(RColorBrewer)
library("scales")
probes <- 16
mycolors <- colorRampPalette(brewer.pal(12, "Set1"))(probes)

library(readxl)
EmoryTBDAta <- read_excel("/Users/sarahmali/Desktop/Adult TB_noHIV_Flowerplot_MINMAXSCALE_medians_FINAL_v4.xlsx")

#View(EmoryTBDAta)
ggplot(data=EmoryTBDAta,aes(x=Variable,y=Latent, fill=Probe))+
  geom_bar(stat="identity")+ coord_polar(theta='x',start=0 ,direction=1)+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_line(), panel.grid = element_line(), panel.border = element_blank()) +
  scale_fill_manual(values=mycolors)
ggplot(data=EmoryTBDAta,aes(x=Variable,y=Active, fill=Probe))+
  geom_bar(stat="identity")+ coord_polar(theta='x',start=0 ,direction=1)+
  theme(axis.text.x = element_blank(),axis.ticks.x=element_line(), panel.grid = element_line(), panel.border = element_blank()) +
  scale_fill_manual(values=mycolors)

#command-shift-C=comment/uncomment lines 

library(readxl)
EmoryTBDAta <- read_excel("/Users/sarahmali/Downloads/TB _HIV _Flowerplot_Zscore_medians.xlsx")
View(EmoryTBDAta)
ggplot(data=EmoryTBDAta,aes(x=Variable,y=Latent, fill=Probe))+
  geom_bar(stat="identity")+ coord_polar()
ggplot(data=EmoryTBDAta,aes(x=Variable,y=Active, fill=Probe))+
  geom_bar(stat="identity")+ coord_polar()


library(readxl)
EmoryTBDAta <- read_excel("/Users/sarahmali/Downloads/TB _HIV _Flowerplot_MINMAXSCALE_medians.xlsx")
View(EmoryTBDAta)
ggplot(data=EmoryTBDAta,aes(x=Variable,y=Latent, fill=Probe))+
  geom_bar(stat="identity")+ coord_polar()
ggplot(data=EmoryTBDAta,aes(x=Variable,y=Active, fill=Probe))+
  geom_bar(stat="identity")+ coord_polar()

