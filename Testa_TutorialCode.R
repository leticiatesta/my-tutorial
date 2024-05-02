#Remove all objects from global environment
rm(list = ls())

#Install needed packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("ggtext")

#Load the installed packages
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggtext)


# Set the working directory to a specific folder
setwd("/Users/leticiatesta/Desktop/Research methods II/Tutorial stuffs")

#Upload the excel file into the environment
vitd_data<-read_xlsx("Leticia_fuso_data.xlsx",
                     sheet = 2)%>%
group_by(Treatment, Concentration)
vitd_data

#Separate all the treatments by groups and take the means (for stats analysis) 
iron <-vitd_data%>%
  filter(Treatment == "Iron")%>%
  mutate(mean = mean(ABS))

glucose <-vitd_data%>%
  filter(Treatment == "Glucose")%>%
  mutate(mean = mean(ABS))

res<-vitd_data%>%
  filter(Treatment == "Resveratrol")%>%
  mutate(mean = mean(ABS))

vitaminD <- vitd_data%>%
  filter(Treatment == "Vitamin D")%>%
  mutate(mean = mean(ABS),
         n = n(),
         sd = sd(ABS),
         se = sd/sqrt(n))

vitaminD$Concentration <- as.numeric(vitaminD$Concentration)

#Setup the plot with statistical analysis
#Geom_abline will be used to setup the slope and intercept of some of my treatments
#geom_smooth will add a linear trend to my vitamin D data
#geom_errorbar will add error bar to the vitamin D data set
#Warning: even though there are comments in the lines below, next lines need to be run together until the end, since they are all part of the ggplot package!
leticia_plot<-ggplot(vitaminD, aes(x=Concentration, y=mean, fill=Treatment))+
  geom_abline(slope=0,
              intercept=1,
              linetype = "dashed",
              linewidth=2.5)+
  geom_abline(slope=0,
              intercept=1.385653,
              linetype = "dashed",
              color = "#994d2d",
              linewidth=2.5)+
  geom_abline(slope=0,
              intercept=0.1182763	,
              linetype = "dashed",
              color = "#f0c4b4",
              linewidth=2.5)+
  geom_abline(slope=0,
              intercept=1.284797	,
              linetype = "dashed",
              color = "#a0bca4",
              linewidth=2.5)+
  geom_smooth(method='lm',
              fill = "#a8bccc",
              color = "black",
              linewidth=2.5)+
  geom_errorbar(aes(ymin = mean-se, 
                    ymax = mean+se),
                width=0.09,
                size=1)+
  #Use annotate to add titles to lines in the graph. This helps to differentiate the lines you're looking at.Run together with the previous code
  annotate("text", 
           x=30, 
           y=.17, 
           label= "Resveratrol Average Biofilm",
           color = "#f0c4b4",
           size=8)+
  annotate("text", 
           x=30, 
           y=1.05, 
           label= "Vehicle Average Biofilm",
           color = "black",
           size=8)+
  annotate("text", 
           x=30, 
           y=1.33, 
           label= "Glucose Average Biofilm",
           color = "#a0bca4",
           size=8)+
  annotate("text", 
           x=30, 
           y=1.43, 
           label= "Iron Average Biofilm",
           color = "#994d2d",
           size=8)+
  annotate("text", 
           x=6, 
           y=.55, 
           label= "Linear Regression Analysis:",
           color = "black",
           size=11)+
  scale_fill_manual(values = "#a8bccc")+
  #Add statistical correlation to the graph
  stat_cor(aes(label = after_stat(rr.label)),
           label.y=0.45,
           size=12)+
  stat_cor(aes(label = after_stat(p.label)),
           label.y=0.35,
           size=12)+
  geom_point(shape=21,
             size=8,
             stroke=1.5)+
  ylim(0, 1.5)+
  scale_x_log10()+
  theme_minimal()+
  theme(text=element_text(size = 35),
        axis.title = element_text(face="bold"),
        axis.line = element_line(linewidth = 1.35,
                                 color="black"),
        strip.text = element_text(size=30,
                                  face = "bold"),
        axis.text = element_text(face="bold",
                                 color="black"),
        strip.background = element_blank(),
#Add title to the plot
        plot.title = element_textbox_simple(margin=margin(20,0,20,0)),
        plot.margin = margin(l=20, 
                             r=10,
                             b=10),
        title = element_text(face = "bold"))+
  labs(title="Exposure to VitaminD enhances the ability of a CRC-associated *F. nucleatum* strain to form biofilm.",
       y="Relative Biofilm Formation",
       x= "VitaminD Concentration (ng/uL)")
leticia_plot

#Save the plot in the "tutorial stuffs" file (or whatever file you choose)
ggsave(file="leticia_plot.jpeg", path ="~/Desktop/Research methods II/Tutorial stuffs", height = 13, width = 20, leticia_plot)