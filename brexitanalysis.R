#Analyse der demografischen Kennzahlen zum EU-Referendum Großbritanniens

library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library(extrafont)
library(corrplot)

#Unser Style


theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))  


library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))

(my_sheets <- gs_ls())
brexit <- gs_key('1DowYpzqqCp_yUiqLVBzlRNrcYJQLqHV82U3bomM_LUk')

#Alle Worksheets aus dem GSheet importieren

brexitdata <- gs_read(brexit, ws = 'data', col_names = TRUE)

#Tidying

brexitdata$leavepct <- as.numeric(brexitdata$leavepct)
brexitdata$age <- as.numeric(brexitdata$age)
brexitdata$totalpop <- as.numeric(brexitdata$totalpop)
brexitdata$totalpopuk <- as.numeric(brexitdata$totalpopuk)
brexitdata$totalbildund <- as.numeric(brexitdata$totalbildund)

brexitdata$ohnebildung <- as.numeric(brexitdata$ohnebildung)
brexitdata$hohebildung <- as.numeric(brexitdata$hohebildung)
brexitdata$incomeperweek <- as.numeric(brexitdata$incomeperweek)
brexitdata$electorate <- as.numeric(brexitdata$electorate)

brexitdata$validvotes <- as.numeric(brexitdata$validvotes)
brexitdata$remain <- as.numeric(brexitdata$remain)
brexitdata$leave <- as.numeric(brexitdata$leave)
brexitdata$electorate <- as.numeric(brexitdata$electorate)

#New Columns

brexitdata$ohnebildungpct <- brexitdata$ohnebildung/brexitdata$totalbildund
brexitdata$hohebildungpct <- brexitdata$hohebildung/brexitdata$totalbildund
brexitdata$totalpopukpct <- brexitdata$totalpopuk/brexitdata$totalpop
brexitdata$totalpopnotukpct <- (brexitdata$totalpop-brexitdata$totalpopuk)/brexitdata$totalpop

#Charting

# ============================================================================ #
# SCATTERPLOTTS DER BEZIEHUNGEN
# ============================================================================ #


brexitdata$lessthan50 = brexitdata$remainpct<50;

brexithohebildung <- ggplot(brexitdata, aes(x=remainpct, y=hohebildungpct, color=lessthan50)) +
  geom_point(alpha=1/2) + 
#  geom_smooth(method=lm)  +
  scale_y_continuous(labels = percent) +
  labs(x = "Stimmenanteil für Remain", y = "Bildungsgrad") +
  ggtitle("Je höher der Bildungsgrad,\ndesto mehr für EU-Verbleib") +
#  scale_color_manual(name = "remainpct",
#                     values = c("(0,50]" = "red",
#                                "(50,100]" = "blue"),
#                     labels = c("<= 50", "> 50"))+
  guides(color=FALSE)
  theme
plot(brexithohebildung)

#af040a rot
#1f77b4 blau


