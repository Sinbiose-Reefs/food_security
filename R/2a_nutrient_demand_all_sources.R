

require("here")
require(dplyr)
require(ggplot2)
require(reshape)
require(reshape2)
require(tidyr)
library(sf)
library(tidyverse)
library(ggrepel)
library(scatterpie)
library (gridExtra)
require(cowplot)
require("ggbreak")

# load functions
source ("R/functions.R")
source ("R/rainplot.R")

# ----------------



# load data for analysis
load (here ("output",
            "fishConsumption_Income.RData"))

# 1 - daily consumption
# filter the food
filter_interesting_food <- CONSUMO_ALIMENTAR %>% 
  filter (single_PTN == 1) %>% # seafood & only raw protein
  mutate (Ndays=n_distinct(DIA_SEMANA)) %>% 
  mutate_at("sea_food", ~replace_na(.,0))
  


# plots without the maps
# with FAO's thresholds
FAO_threshold <- openxlsx::read.xlsx (here ("data_fisheries_nutrients", "Threshold_FAO.xlsx"))


# filter the days
consumption_nutrients_day <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  #filter (position == "sea") %>% # coastal states
  select (region,
          state,
          income_cat,
          COD_INFOR,
          DIA_SEMANA,
          N_pop_class, 
          Ndays,
          sea_food,
          QTD, 
          PTN,
          Calcium, 
          Iron, 
          Zinc, 
          `Vitamin-A`, 
          Omega3,
          Magnesium) %>% # select  variables (nutrients) to test
  
  # edit seafood factor
  mutate_at ("region", as.factor) %>%
  mutate_at ("sea_food", as.factor) %>%
  
  mutate(sea_food = recode(sea_food, "0" = "All sources",
                                    "1" = "Seafood"
  )) %>%

  # mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interviewing days
  
  group_by(region,state,income_cat,sea_food,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>%
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays))  %>% 
  #group_by(region,state,sea_food) %>%  # income_cat # summarize by person
  #summarise(across (QTD:Magnesium, ~mean(.x, na.rm=T))) %>% # sum of personal consumption
            
  # long format
  gather (Nutrient,Quantity,PTN:Magnesium) %>% 
  # add FAO thresholds (the col "Nutrient" automatically matches)
  right_join(FAO_threshold) %>%
  # remove NAs
  filter (is.na (Nutrient) != T )
  

# all nutrients ()
consumption_nutrients_day_all <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  #filter (position == "sea") %>% # coastal states
  select (region,
          state,
          income_cat,
          COD_INFOR,
          DIA_SEMANA,
          N_pop_class, 
          Ndays,
          sea_food,
          QTD, 
          PTN,
          Calcium, 
          Iron, 
          Zinc, 
          `Vitamin-A`, 
          Omega3,
          Magnesium) %>% # select  variables (nutrients) to test
  
  # edit seafood factor
  mutate_at ("region", as.factor) %>%
  mutate_at ("sea_food", as.factor) %>%
  
  mutate(sea_food = recode(sea_food, "0" = "All sources",
                                     "1" = "All sources"
  )) %>%
  
  # mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interviewing days
  
  group_by(region,state,income_cat,sea_food,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>%
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays))  %>% 
  #group_by(region,state,sea_food) %>%  # income_cat # summarize by person
  #summarise(across (QTD:Magnesium, ~mean(.x, na.rm=T))) %>% # sum of personal consumption
  
  # long format
  gather (Nutrient,Quantity,PTN:Magnesium) %>% 
  # add FAO thresholds (the col "Nutrient" automatically matches)
  right_join(FAO_threshold) %>%
  # remove NAs
  filter (is.na (Nutrient) != T ) 
                                        
                                           
# all data to plot                                         
consumption_nutrients_day <- bind_rows(consumption_nutrients_day %>%
                                filter (sea_food == "Seafood"), # only seafood
          consumption_nutrients_day_all) #all sources

                                         
                                         
    

# factor order
consumption_nutrients_day$region <- factor (consumption_nutrients_day$region, 
                                            levels = c("South",
                                                       "Southeast",
                                                       "Northeast", 
                                                       "North"))

# plots with breaks
# protein
ptn_plot <- consumption_nutrients_day %>% 
  
  filter (Nutrient== "PTN") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  coord_flip()+
  facet_wrap(~Nutrient)+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_y_break(c(30, 46), expand=T,scales="fixed")

ptn_plot


# zinc plot
zinc_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Zinc") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  coord_flip()+
  facet_wrap(~Nutrient)+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed")+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_break(c(4.5, 9.5), expand=T,scales="fixed")
  
zinc_plot


# iron
iron_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Iron") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  coord_flip()+
  facet_wrap(~Nutrient)+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed")+
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_y_break(c(1.7, 21.2), expand=T,scales="fixed")

iron_plot

# iron
calcium_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Calcium") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.background =  element_rect(
                  colour ="lightgray",
                    fill="lightgray", 
                     size=2, linetype="solid"),
        
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_break(c(150, 990), expand=T,scales="fixed")

calcium_plot


# omega 3
omega_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Omega3") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity*1000),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (mg, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none")

omega_plot


# vitamin A
vitA_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Vitamin-A") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA) +
  #scale_y_continuous(limits = quantile(~.Quantity, c(0.1, 0.9)))+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) + 
  scale_y_break(c(10, 795), expand=T,scales="fixed")

vitA_plot


# vitamin A
magnesium_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Magnesium") %>%
  
  filter (Quantity < quantile (Quantity, 0.90)) %>%
  
  ggplot(aes (x=region,
              y=(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5,outlier.shape = NA)+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none",
        axis.title.x = element_blank())  + 
  scale_y_break(c(32, 305), expand=T,scales="fixed")

magnesium_plot

# arrange 


 
                
              pdf(here ("output",
                        "patchwork_nutrients.pdf"),width =8,height = 12, onefile=T)
              lapply (list (ptn_plot,
                            magnesium_plot, 
                            calcium_plot, 
                            iron_plot, 
                            zinc_plot, 
                            omega_plot, 
                            vitA_plot), function (i)
                  i)
              
              dev.off()
              
              

# require(patchwork)
(ptn_plot)/ ( magnesium_plot | calcium_plot)/(iron_plot| zinc_plot)/(omega_plot | vitA_plot)
(ptn_plot)/ ( magnesium_plot) / (calcium_plot)/ (iron_plot)/ (zinc_plot)/(omega_plot) / (vitA_plot)

# ------------------------------------------

# statistical analysis


# --------------------------------------------
# analyze


# run model (ancova)
model.ancova <- lapply (unique(consumption_nutrients_day$Nutrient), function (i)
  
  aov ((Quantity) ~ region*sea_food,
      data=consumption_nutrients_day[which(consumption_nutrients_day$Nutrient == i),])
  
)


# summary of results
lapply (model.ancova, summary)
# posthoc analysis
posthoc_test <- lapply (model.ancova, TukeyHSD, "region:sea_food")

# nutrients
nuts <- unique(consumption_nutrients_day$Nutrient)
plots_interaction <- lapply (seq (1,length(nuts)), function (i)

  GGTukey.2(posthoc_test[[i]])+ggtitle (nuts[i]) + theme(legend.position = "none")

  )

# plot vit-A without interaction
posthoc_test_vit.a_region <- TukeyHSD(model.ancova[[5]], "region")
posthoc_test_vit.a_sf <- TukeyHSD(model.ancova[[5]], "sea_food")
plots_vita<-GGTukey.2(posthoc_test_vit.a_region)+ggtitle (nuts[5]) 


# plot and save
pdf(here ("output", "tukeyhsd_plots.pdf"),height=17,width=10)
grid.arrange(plots_interaction[[1]]+theme(legend.position = "none",
                                          plot.title = element_text(size=14)),
             plots_interaction[[2]]+theme(axis.text.y =  element_blank(),
                                          legend.position = "none",
                                          plot.title = element_text(size=14)),
             plots_interaction[[3]]+theme(legend.position = "none",
                                          plot.title = element_text(size=14)),
             plots_interaction[[4]]+theme(axis.text.y =  element_blank(),
                                          legend.position = "none",
                                          plot.title = element_text(size=14)),
             plots_interaction[[6]]+theme(legend.position = "none",
                                          plot.title = element_text(size=14)),
             plots_interaction[[7]]+theme(axis.text.y =  element_blank(),
                                          legend.position = "none",
                                          plot.title = element_text(size=14)),
             plots_vita+theme(plot.title = element_text(size=14)),
             
             ncol=3,nrow=4,
             layout_matrix = rbind (c(1,1,2),
                                    c(3,3,4),
                                    c(5,5,6),
                                    c(7,7,NA))
             
             
)
dev.off()
