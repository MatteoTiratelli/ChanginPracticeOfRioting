library(tidyverse)
library(reshape2)
library(extrafont)

Figure1 <- read.csv('Figure 1: Riots per Year.csv')
Figure2 <- read.csv('Figures 2 and 3_ Proportion of riots per 20 years.csv')

# Figure 1: The number of riots per year

Years.m <- Figure1
Years.m$X <- NULL
Years.m$Total <- NULL
Years.m <- melt(Years.m, id.vars='Years')
names(Years.m)[names(Years.m)=="variable"] <- "City"
names(Years.m)[names(Years.m)=="value"] <- "Riots"

Years.m$City <- factor(Years.m$City,levels=c("Liverpool","Manchester","Glasgow"))
Years.m$Years <- as.numeric(Years.m$Years)

Figure_1 <- ggplot(Years.m, aes(`Years`, `Riots`)) +   
  geom_bar(stat="identity") +
  scale_y_continuous(name="Number of riots", breaks=c(0,2,4,6,8,10,12), labels=c(0,2,4,6,8,10,12), 
                     limits=c(0,12)) + 
  scale_x_continuous(breaks=c(1800,1820,1840,1860,1880,1900,1920,1940),
                     labels=c("1800","1820","1840","1860","1880", "1900","1920","1940")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  facet_grid(City ~ .)

ggsave(filename = "Figure_1.png", device='png',
       plot = Figure_1, dpi=300,
       bg = "transparent",
       width=8.5, height=5)



# Figure 2: The proportion of riots involving ‘begging’ or ‘factory visiting’ 

Prop.m <- melt(Figure2, id.vars='Year')
names(Prop.m)[names(Prop.m)=="variable"] <- "Behaviour"
names(Prop.m)[names(Prop.m)=="value"] <- "# Riots"

labels <- c('BegProp'='Begging',
            'FactProp'='Factory visiting')

Figure_2 <- ggplot(Prop.m[which(Prop.m$Behaviour=="BegProp"|Prop.m$Behaviour=="FactProp"),], aes(`Year`, `# Riots`)) +   
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_y_continuous(name="Proportion of riots involving...", limits=c(0.00,0.2)) +
  scale_x_continuous(breaks=c(1800,1820,1840,1860,1880,1900,1920,1940),
                     labels=c("1800","1820","1840","1860","1880", "1900","1920","1940")) +
  facet_grid(Behaviour ~ ., labeller=as_labeller(labels))

ggsave(filename = "Figure_2.png", device='png',
       plot = Figure_2, dpi=300,
       bg = "transparent",
       width=8.5, height=5)




# Figure 3: The proportion of riots targeting specific individuals

Figure_3 <- ggplot(Prop.m[which(Prop.m$Behaviour =="TargetedProp"),], aes(`Year`, `# Riots`)) +   
  geom_bar(stat="identity") +
  scale_y_continuous(name="Proportion of riots targeting specific individuals") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(breaks=c(1800,1820,1840,1860,1880,1900,1920,1940),
                     labels=c("1800","1820","1840","1860","1880", "1900","1920","1940"))

ggsave(filename = "Figure_3.png", device='png',
       plot = Figure_3, dpi=300,
       bg = "transparent",
       width=8.5, height=5)


# Simple regressions

Regression <- read.csv("/Users/matteo/Downloads/Regression.csv")
Regression$Year <- as.numeric(substr(Regression$Date, 1, 4))
Regression$Time <- Regression$Year - 1799

ModelTarg <- glm(Targeted ~ Time, family=binomial, data=Regression)
summary(ModelTarg)
ModelBeg <- glm(Begging ~ Time, family=binomial, data=Regression)
summary(ModelBeg)
ModelFac <- glm(Factory ~ Time, family=binomial, data=Regression)
summary(ModelFac)
