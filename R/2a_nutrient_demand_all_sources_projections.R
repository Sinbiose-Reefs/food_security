# script to run projections

source("R/packages.R")

# load functions
source ("R/functions.R")
source ("R/rainplot.R")

# ----------------

load(file = here ("output", "projections_fao.RData"))

# melt to a df
df_prop_FAO <- lapply (prop_FAO, function (i) 
  
  do.call(rbind , lapply (i, function (k)
    
    do.call(rbind,k)
    
  )))

# final melt
df_prop_FAO <- do.call(rbind,df_prop_FAO)

# infinite to zero
# df_prop_FAO$proj_nut_FAO[is.infinite(df_prop_FAO$proj_nut_FAO)] <- 0
# disable this if you want that line do not cross each other
#df_prop_FAO$prop_diet_projected [df_prop_FAO$type == "all_minus_seafood"] <- 100 - df_prop_FAO$prop_diet_projected [df_prop_FAO$type == "all_minus_seafood"]

# average
mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")])


pdf(here ('output', "histogram_percentage.pdf"),onefile = T)
par(mfrow=c(1,1))

# seafood
hist(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")],
     main = "Observed percentage of different\nfood types in the diet",
     xlab = "Percentage",
     ylab = "Frequency (projections * individuals)",
     xlim=c(0,100),
     col="blue")
# other 
hist(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "all_minus_seafood")],
     add=T,col = "green3")

abline(v=mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")]),
       lwd=2,col="blue4")
abline(v=mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "all_minus_seafood")]),
       lwd=2,col="green4")
dev.off()

# distribution of ratio values
pdf(here ('output', "histogram_recommendations.pdf"),onefile = F)

par(mfrow=c(2,2))
hist(df_prop_FAO$proj_nut_FAO [which(df_prop_FAO$type == "seafood")],
          main = "Seafood",
            xlab = "Raw ratio values (FAO recommendations / projections)",
            ylab = "Frequency (projections * individuals)",
     col = "blue")
# other 
hist(df_prop_FAO$proj_nut_FAO [which(df_prop_FAO$type == "all_minus_seafood")],
               main = "Other sources",
               xlab = "Raw ratio values (FAO recommendations / projections)",
               ylab = "",
     col = "green3")

# log
hist(log(df_prop_FAO$proj_nut_FAO [which(df_prop_FAO$type == "seafood")]),
     main = "Seafood",
     xlab = "Natural-log of ratio values (FAO recommendations / projections)",
     ylab = "Frequency (projections * individuals)",
     col = "blue")
# other 
hist(log(df_prop_FAO$proj_nut_FAO [which(df_prop_FAO$type == "all_minus_seafood")]),
     main = "Other sources",
     xlab = "Natural-log of ratio values (FAO recommendations / projections)",
     ylab = "",
     col = "green3")

dev.off()

# average vals
p1<-df_prop_FAO %>% 
  #filter (nutrient == nut) %>%
  #filter (proj_nut_FAO != "Inf") %>%
  group_by (type,nutrient,prop_diet_projected) %>%
  summarise (av_val =  median(proj_nut_FAO,na.rm=T)) %>% 
  
  ggplot (aes (x= prop_diet_projected ,
               y=log(av_val), 
               group = type ,
               fill=type,
               colour=type)) +
  geom_point(alpha=0.7)+
  geom_line (alpha=0.7)+
  scale_colour_manual (values = c("#89E06C", "#525DDB"))+
  geom_hline(yintercept=0,linetype=2,colour="black")+
  geom_vline(xintercept=mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")]),
             linetype=1,colour="gray")+
  geom_vline(xintercept=50,linetype=1,colour="gray")+
  geom_vline(xintercept=25,linetype=1,colour="gray")+
  geom_vline(xintercept=75,linetype=1,colour="gray")+
  facet_wrap(~nutrient,scales = "free",nrow=1)+
  theme_classic() + 
  theme(legend.position = "top")+
  ylab ("Deviations from FAO's recommendations\n(ratio at natural-log scale)")+
  xlab ("Proportion of seafood in the diet") + 
  scale_x_continuous(sec.axis = sec_axis (~ .-1,
                                          name = "Proportion of other sources in the diet"))+
  labs (subtitle = "A) Nutritional projections")


# density
p2<-df_prop_FAO %>%
  
  # select percentage
  filter (type == "seafood" & prop_diet_projected == round(mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")])) | 
            type == "all_minus_seafood" & prop_diet_projected == 100-round(mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")]))) %>%
  
  ggplot (aes (x=log(proj_nut_FAO),
               fill = type,
               col=type)) +
  geom_vline(data = df_prop_FAO %>%
               
               filter (type == "seafood" & prop_diet_projected == round(mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")])) | 
                         type == "all_minus_seafood" & prop_diet_projected == 100-round(mean(df_prop_FAO$observed_proportion [which(df_prop_FAO$type == "seafood")]))) %>%
               group_by(type,nutrient) %>%
               summarize (prop=median(proj_nut_FAO)),
             
             aes(xintercept=(log(prop)),col=type),
             linetype=1)+theme_bw()+
  
  geom_vline(aes(xintercept=0),linetype=2)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  geom_density(alpha=0.7) +
  ylim (c(0,1))+
  facet_wrap(~nutrient,nrow=1) +
  
  labs (subtitle = "B) 6% seafood, 94% other sources")+
  scale_colour_manual (values = c("#89E06C", "#525DDB"))+
  scale_fill_manual (values = c("#89E06C", "#525DDB"))

# density
p3<-df_prop_FAO %>%
  
  # select percentage
  filter (type == "seafood" & prop_diet_projected == 50 | 
            type == "all_minus_seafood" & prop_diet_projected == 50) %>%
  
  ggplot (aes (x=log(proj_nut_FAO),
               fill = type,
               col=type)) +
  geom_vline(data = df_prop_FAO %>%
               
               filter (type == "seafood" & prop_diet_projected == 50 | 
                         type == "all_minus_seafood" & prop_diet_projected == 50) %>%
               group_by(type,nutrient) %>%
               summarize (prop=median(proj_nut_FAO)),
             
             aes(xintercept=(log(prop)),col=type),
             linetype=1)+theme_bw()+
  
  geom_vline(aes(xintercept=0),linetype=2)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  geom_density(alpha=0.7) +
  ylim (c(0,1))+
  
  facet_wrap(~nutrient,nrow=1) +
  
  labs (subtitle = "D) 50% seafood, 50% other sources")+
  scale_colour_manual (values = c("#89E06C", "#525DDB"))+
  scale_fill_manual (values = c("#89E06C", "#525DDB"))


# 25%

# density
p4<-df_prop_FAO %>%
  
  # select percentage
  filter (type == "seafood" & prop_diet_projected == 25 | 
            type == "all_minus_seafood" & prop_diet_projected == 75) %>%
  
  ggplot (aes (x=log(proj_nut_FAO),
               fill = type,
               col=type)) +
  geom_vline(data = df_prop_FAO %>%
               
               filter (type == "seafood" & prop_diet_projected == 25 | 
                         type == "all_minus_seafood" & prop_diet_projected == 75) %>%
               group_by(type,nutrient) %>%
               summarize (prop=median(proj_nut_FAO)),
             
             aes(xintercept=(log(prop)),col=type),
             linetype=1)+theme_bw()+
  
  geom_vline(aes(xintercept=0),linetype=2)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  geom_density(alpha=0.7) +
  ylim (c(0,1))+
  
  facet_wrap(~nutrient,nrow=1) +
  
  labs (subtitle = "C) 25% seafood, 75% other sources")+
  scale_colour_manual (values = c("#89E06C", "#525DDB"))+
  scale_fill_manual (values = c("#89E06C", "#525DDB"))



# 75%
p5<-df_prop_FAO %>%
  
  # select percentage
  filter (type == "seafood" & prop_diet_projected == 50 | 
            type == "all_minus_seafood" & prop_diet_projected == 50) %>%
  
  ggplot (aes (x=log(proj_nut_FAO),
               fill = type,
               col=type)) +
  geom_vline(data = df_prop_FAO %>%
               
               filter (type == "seafood" & prop_diet_projected == 75 | 
                         type == "all_minus_seafood" & prop_diet_projected == 25) %>%
               group_by(type,nutrient) %>%
               summarize (prop=median(proj_nut_FAO)),
             
             aes(xintercept=(log(prop)),col=type),
             linetype=1)+theme_bw()+
  
  geom_vline(aes(xintercept=0),linetype=2)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  geom_density(alpha=0.7) +
  ylim (c(0,1))+
  
  facet_wrap(~nutrient,nrow=1) +
  
  labs (subtitle = "E) 75% seafood, 25% other sources") + 
  
  xlab ("Deviations from FAO's recommendations\n(natural-log scale)")+
  scale_colour_manual (values = c("#89E06C", "#525DDB"))+
  scale_fill_manual (values = c("#89E06C", "#525DDB"))



# arrange
pdf (here ("output", "projections.pdf"),width = 10,height=7.5)

grid.arrange(p1,
             p2,
             p4,
             p3,
             p5,
             ncol=1,nrow=6,
             layout_matrix = rbind (c (1),
                                    c (1),
                                    c (1),
                                    c (2),
                                    c (3),
                                    c (4),
                                    c (5)))

dev.off()


# plot
tot_prop <- df_prop_FAO %>% 
  filter (nutrient == "Calcium") %>%
  ggplot (aes (x=log(proj_nut_FAO),fill=type)) +
  geom_density(alpha=0.5) + 
  theme_bw() +
  geom_vline(aes(xintercept=median(log(proj_nut_FAO)),group=type),linetype=2)

# proportion
# mean
df_prop_FAO %>%
  group_by(type) %>%
  summarise(mean(observed_proportion))

# median
df_prop_FAO %>%
  group_by(type) %>%
  summarise(median(observed_proportion))
# sd
df_prop_FAO %>%
  group_by(type) %>%
  summarise(sd(observed_proportion))

# range
df_prop_FAO %>%
  group_by(type) %>%
  summarise(range(observed_proportion))

# grams
# mean
df_prop_FAO %>%
  group_by(type) %>%
  
  summarise(mean(quantity))

# median
df_prop_FAO %>%
  group_by(type) %>%
  summarise(median(quantity))

# sd
df_prop_FAO %>%
  group_by(type) %>%
  summarise(sd(quantity))

# sd
df_prop_FAO %>%
  group_by(type) %>%
  summarise(range(quantity))



df_prop_FAO %>%
  
  group_by(nutrient,type) %>%
  summarize (median (proj_nut_FAO,na.rm=T)) 

