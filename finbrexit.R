library(ggplot2)
library(ggthemes)
require(scales)
library(plyr)
options(scipen=999)


data = read.csv('brexitelectiondata.csv')
data$total = data$leave+data$remain
data$remainpercent = data$remain/data$total*100
data$country = substring(data$key,0,1)
data$country = revalue(data$country, c("B"="Gibraltar", "E"="England","W"="Wales","S"="Schottland","N"="Nordirland"))
data$fct = data$country
data$fct[data$fct!="England"]="Nicht England"

colors  <- as.character(c("asdf"="#666666", "Gibraltar"="#333333", "Nordirland"="#00ff00", "Schottland"="#0000ff","Wales"="#ff0000"))
colorscale <- scale_colour_manual(name="Farben", values=colors)


p = ggplot(data, aes(group=factor(country),color=country,y=remainpercent,x=factor(country))) +
  geom_point(aes(size=total)) +
  colorscale +
  scale_y_continuous(limits = c(0, 100),expand = c(0,0)) +
  scale_size(name="Größe der Council Area") +
  scale_x_discrete() +
  xlab("") +
  ylab("Prozent gegen Brexit") +
  theme_light()

p
