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
library(ggrepel)

options(scipen=999)

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
brexitdata$remainpctdez <- brexitdata$remainpct/100
brexitdata$leavepctdez <- brexitdata$leavepct/100

#Charting

# ============================================================================ #
# SCATTERPLOTTS DER BEZIEHUNGEN
# ============================================================================ #

#Bildungsplot hoch

brexitdata$lessthan50 = brexitdata$remainpct<50;

brexithohebildung <- ggplot(brexitdata, aes(x=remainpctdez, y=hohebildungpct, color=lessthan50)) +
  geom_point(alpha=0.7, aes(size=electorate)) + 
  scale_size_continuous(range = c(0.1,5)) +
#  geom_smooth(method=lm)  +
  scale_y_continuous(labels = percent, breaks = pretty(brexitdata$hohebildungpct, n = 3)) +
  scale_x_continuous(labels = percent, breaks = pretty(brexitdata$remainpctdez, n = 5))+
  scale_color_manual(values=c("FALSE" = "#1f77b4","TRUE" = "#af040a"))+
  labs(x = "Stimmenanteil für Remain", y = "Anteil der Bürger mit\n Hochschulabschluss") +
  ggtitle("Je höher der Bildungsgrad,\ndesto eher für EU-Verbleib") +
  guides(color=FALSE) +
  theme(legend.position="none")+
  geom_text_repel(data=filter(brexitdata, hohebildungpct>0.55), aes(label=area),
                  box.padding = unit(1, "lines"),
                  point.padding = unit(1, "lines"))+
  theme
plot(brexithohebildung)

ggsave("brexithohebildung.pdf", dpi = 300)
quartz.save("brexithohebildung.png", type = "png", device = dev.cur(), dpi = 100)

#af040a rot
#1f77b4 blau


#Bildung ohne formalen Abschluss
brexitohnebildung <- ggplot(brexitdata, aes(x=leavepctdez, y=ohnebildungpct, color=lessthan50)) +
  geom_point(alpha=0.7, aes(size=electorate)) + 
  scale_size_continuous(range = c(0.1,5)) +
  #  geom_smooth(method=lm)  +
  scale_y_continuous(limits=c(0, 0.4),labels = percent, breaks = pretty(brexitdata$ohnebildungpct, n = 3)) +
  scale_x_continuous(labels = percent, breaks = pretty(brexitdata$leavepctdez, n = 5))+
  scale_color_manual(values=c("FALSE" = "#1f77b4","TRUE" = "#af040a"))+
  labs(x = "Stimmenanteil für 'Leave'", y = "Anteil der Bürger ohne\n formalen Abschluss") +
  ggtitle("Je niedriger der Bildungsgrad,\ndesto eher für EU-Austritt") +
  guides(color=FALSE) +
  theme(legend.position="none")+
  geom_text_repel(data=filter(brexitdata, ohnebildungpct>0.55), aes(label=area),
                  box.padding = unit(1, "lines"),
                  point.padding = unit(1, "lines"))+
  theme
plot(brexitohnebildung)

#Check der Korrelationen

#OHne Ausbildung
x <- brexitdata$leavepct
y <- brexitdata$ohnebildungpct
cor(x, y)

#Hoher Bildungsabschluss
z <- brexitdata$remainpct
y <- brexitdata$hohebildungpct
cor(z, y)

#Uk Residents
u <- brexitdata$totalpopukpct
cor(x,u)

#non Uk Residents
notuk <- brexitdata$totalpopnotukpct
cor(x,notuk)

#age
age <- brexitdata$age
cor(x,age)

#income per week
inc <- brexitdata$incomeperweek
cor(inc, z, use = "complete")



#Bildung ohne formalen Abschluss
brexitohnebildung <- ggplot(brexitdata, aes(x=leavepctdez, y=ohnebildungpct, color=lessthan50)) +
  geom_point(alpha=0.7, aes(size=electorate)) + 
  scale_size_continuous(range = c(0.1,5)) +
  #  geom_smooth(method=lm)  +
  scale_y_continuous(limits=c(0, 0.4),labels = percent, breaks = pretty(brexitdata$ohnebildungpct, n = 3)) +
  scale_x_continuous(labels = percent, breaks = pretty(brexitdata$leavepctdez, n = 5))+
  scale_color_manual(values=c("FALSE" = "#1f77b4","TRUE" = "#af040a"))+
  labs(x = "Stimmenanteil für 'Leave'", y = "Anteil der Bürger ohne\n formalen Abschluss") +
  ggtitle("Je niedriger der Bildungsgrad,\ndesto eher für EU-Austritt") +
  guides(color=FALSE) +
  theme(legend.position="none")+
  geom_text_repel(data=filter(brexitdata, ohnebildungpct>0.55), aes(label=area),
                  box.padding = unit(1, "lines"),
                  point.padding = unit(1, "lines"))+
  theme
plot(brexitohnebildung)

#Einkommen
brexitincome <- ggplot(brexitdata, aes(x=remainpctdez, y=incomeperweek, color=lessthan50)) +
  geom_point(alpha=0.7, aes(size=electorate)) + 
  scale_size_continuous(range = c(0.1,5)) +
  #  geom_smooth(method=lm)  +
  scale_y_continuous(limits=c(350, 900), breaks = pretty(brexitdata$incomeperweek, n = 3)) +
  scale_x_continuous(labels = percent, breaks = pretty(brexitdata$remainpctdez, n = 5))+
  scale_color_manual(values=c("FALSE" = "#1f77b4","TRUE" = "#af040a"))+
  labs(x = "Stimmenanteil für 'Remain'", y = "Medianeinkommen pro Woche in £") +
  ggtitle("Je höher das Einkommen,\ndesto eher für EU-Verbleib") +
  guides(color=FALSE) +
  theme(legend.position="none")+
  geom_text_repel(data=filter(brexitdata, incomeperweek>1000), aes(label=area),
                  box.padding = unit(1, "lines"),
                  point.padding = unit(1, "lines"))+
  theme
plot(brexitincome)

#Brexit NOT-UK-Resident
brexitnotukresidents <- ggplot(brexitdata, aes(x=remainpctdez, y=totalpopnotukpct, color=lessthan50)) +
  geom_point(alpha=0.7, aes(size=electorate)) + 
  scale_size_continuous(range = c(0.1,5)) +
  #  geom_smooth(method=lm)  +
  scale_y_continuous(limits=c(0, 0.6),labels = percent, breaks = pretty(brexitdata$totalpopnotukpct, n = 5)) +
  scale_x_continuous(labels = percent, breaks = pretty(brexitdata$remainpctdez, n = 5))+
  scale_color_manual(values=c("FALSE" = "#1f77b4","TRUE" = "#af040a"))+
  labs(x = "Stimmenanteil für 'Remain'", y = "Anteil der im Ausland Geborenen") +
  ggtitle("Je internationaler die Bevölkerung,\ndesto eher für EU-Verbleib") +
  guides(color=FALSE) +
  theme(legend.position="none")+
  geom_text_repel(data=filter(brexitdata, totalpopnotukpct>0.52), aes(label=area),
                  box.padding = unit(0.51, "lines"),
                  point.padding = unit(1, "lines"))+
  theme
plot(brexitnotukresidents)

#Brexit age
brexitage <- ggplot(brexitdata, aes(x=leavepctdez, y=age, color=lessthan50)) +
  geom_point(alpha=0.7, aes(size=electorate)) + 
  scale_size_continuous(range = c(0.1,5)) +
  #  geom_smooth(method=lm)  +
  scale_y_continuous(limits=c(25, 55), breaks = pretty(brexitdata$age, n = 5)) +
  scale_x_continuous(labels = percent, breaks = pretty(brexitdata$leavepctdez, n = 5))+
  scale_color_manual(values=c("FALSE" = "#1f77b4","TRUE" = "#af040a"))+
  labs(x = "Stimmenanteil für 'Leave'", y = "Median-Alter des Bezirkes") +
  ggtitle("Je älter die Bevölkerung,\ndesto eher für EU-Austritt") +
  guides(color=FALSE) +
  theme(legend.position="none")+
  geom_text_repel(data=filter(brexitdata, age>50), aes(label=area),
                  box.padding = unit(0.51, "lines"),
                  point.padding = unit(1, "lines"))+
  theme
plot(brexitage)

