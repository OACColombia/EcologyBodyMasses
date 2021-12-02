#Masses of Birds and Mammals
#Packages
library(reshape2)#melt
library(ggplot2)#Graph
library(ggridges)#density graph
library(cowplot)# ggdraw
library(tidyr)#use %>%
library(dplyr)#data manipulation
library(plyr)#to summarise()
library(rstatix)#Pipe-Friendly Framework for Basic Statistical Tests
library(Rmisc)#summarySE

#Call main data####
Colombian<-read.csv("ColombianMasses11.csv")
Colombian$Weight..gr.<-as.numeric(as.character(Colombian$Weight..gr.))#used as numeric value
Colombian$ReferenceW<-as.numeric(as.character(Colombian$ReferenceW))#used as numeric value
str(Colombian$Weight..gr.)

#highlight the outliers
#out<-Colombian %>%
#  group_by(Species) %>%
#  identify_outliers("Weight..gr.")
#write.csv(out,"Colombian-outliers.csv")

Birds<-subset(Colombian,Class=="Aves")
Mammals<-subset(Colombian,Class=="Mammalia")

#Density figures by Order####
theme_set(theme_ridges())

#Select decreasing by body mass
Birds$Order <- factor(Birds$Order,levels=c('Apodiformes','Procellariiformes','Passeriformes',
                                           'Galbuliformes','Caprimulgiformes','Trogoniformes',
                                           'Cuculiformes','Coraciiformes','Piciformes',
                                           'Columbiformes','Podicipediformes','Charadriiformes',
                                           'Eurypygiformes','Strigiformes','Psittaciformes',
                                           'Nyctibiiformes','Falconiformes','Steatornithiformes',
                                           'Gruiformes','Accipitriformes','Tinamiformes',
                                           'Pelecaniformes','Opisthocomiformes','Anseriformes',
                                           'Suliformes','Galliformes','Cathartiformes',
                                           'Sphenisciformes','Ciconiiformes'))
for_limits=summarySE(data=Birds,measurevar="ReferenceW",groupvars=c("Order"),na.rm = TRUE)
for_limits

FBirds<-ggplot(Birds,aes(x=Weight..gr., y=Order))+
  geom_hline(yintercept = Birds$Order, color="gray")+
  geom_point(color="black", size=0.7)+
  geom_point(color="#00AFBB", size=0.5)+
  geom_density_ridges(fill="#00AFBB", alpha=0.6)+
  annotate("point",x= for_limits$ReferenceW,y=for_limits$Order, shape=17)+
  scale_x_log10()+
  theme_bw()+
  labs(x="log 10(Body mass (gr))")+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

Mammals$Order <- factor(Mammals$Order, levels=c('Eulipotyphla','Chiroptera',
                                                'Paucituberculata','Didelphimorphia','Pilosa',
                                                'Primates','Rodentia','Lagomorpha',
                                                'Cingulata','Carnivora','Artiodactyla'))
for_limits2=summarySE(data=Mammals,measurevar="ReferenceW",groupvars=c("Order"),na.rm = TRUE)
for_limits2

FMammals<-ggplot(Mammals,aes(x=Weight..gr., y=Order))+
  geom_hline(yintercept = Mammals$Order, color="gray")+
  geom_point(color="black", size=0.7)+
  geom_point(color="#FC4E07", size=0.5)+
  geom_density_ridges(fill="#FC4E07", alpha=0.6)+
  annotate("point",x= for_limits2$ReferenceW,y=for_limits2$Order, shape=17)+
  scale_x_log10()+
  theme_bw()+
  labs(x="log 10(Body mass (gr))", y="")+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggdraw()+
  draw_plot(FBirds, x = 0, y = 0, width = 0.48, height = 0.98)+
  draw_plot(FMammals, x = 0.48, y = 0, width = 0.52, height = 0.98)+
  draw_plot_label(label = c("Aves", "Mammalia"), size = 10,
                  x = c(0.05, .5), y = c(1,1))+
  draw_line(x = c(0.5, .5),y = c(0, 1), color = "grey80", size = 0.5)

#Guardar esta figura en buena resolución .tiff a 300 dpi (luego en PowerPoint le pongo los mini Soundscapes copiados de ARBIMON)
ggsave("FDensity.tiff", units="in", width=9, height=7, dpi=300, compression = 'lzw')

# Count species####
BSpp<-Birds %>%
  group_by(Species)%>%
 tally()

MSpp<-Mammals %>%
  group_by(Species)%>%
 tally()

# Year
Colombian$fecha<-as.Date(Colombian$Date,format="%Y-%m-%d")
str(Colombian$fecha)

ggplot(Colombian, aes(x=fecha,y=Weight..gr.))+
  geom_point(aes(color=Class, shape=RecordType), alpha=0.25)+
  scale_x_date(date_breaks="8 year", date_labels="%Y")+
#  scale_y_log10()+
  scale_color_manual(
    values = c("#00AFBB","#FC4E07"))+
  labs(y="Body mass (gr)", x="Date (Year)")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 65, vjust = 1, hjust=1))+
  facet_grid(RecordType~Class, scales = "free_y")+
  guides(colour=FALSE)
ggsave("Fig-year.tiff", units="in", width=8, height=4, dpi=300, compression = 'lzw')#Ganó estos

