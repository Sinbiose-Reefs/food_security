

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
  filter (is.na (Nutrient) != T ) %>%
  mutate_at ("region", as.factor) %>%
  # edit seafood factor
  mutate_at ("sea_food", as.factor) %>%
  
  mutate(sea_food = recode(sea_food, "0" = "Other sources",
                           "1" = "Seafood"
  ))  

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
  ggplot(aes (x=region,
              y=log(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5)+
  coord_flip()+
  facet_wrap(~Nutrient)+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural log scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=log(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none",
        axis.title.x = element_blank())# +
  scale_y_break(c(600, 1000), expand=T,scales="fixed")

ptn_plot


# zinc plot
zinc_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Zinc") %>%
  
  ggplot(aes (x=region,
              y=log(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5)+
  coord_flip()+
  facet_wrap(~Nutrient)+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural log scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=log(threshold)),
              color= "black",
              size=1,
              linetype="dashed")+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank()) #+
  #scale_y_break(c(20, 140), expand=T,scales="fixed")
  
zinc_plot


# iron
iron_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Iron") %>%
  
  ggplot(aes (x=region,
              y=log(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5)+
  coord_flip()+
  facet_wrap(~Nutrient)+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural log scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=log(threshold)),
              color= "black",
              size=1,
              linetype="dashed")+
  theme(legend.position = "none",
        axis.title.x = element_blank())# +
  #scale_y_break(c(115, 180), expand=T,scales="fixed")

iron_plot

# iron
calcium_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Calcium") %>%
  
  ggplot(aes (x=region,
              y=log(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural log scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=log(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = c(0.01,0.2),
        legend.title = element_blank(),
        legend.background =  element_rect(
                  colour ="lightgray",
                    fill="lightgray", 
                     size=2, linetype="solid"),
        
        axis.title = element_blank(),
        axis.text.y = element_blank())# +
  #scale_y_break(c(3000, 5500), expand=T,scales="fixed")

calcium_plot


# omega 3
omega_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Omega3") %>%
  
  ggplot(aes (x=region,
              y=log(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural log scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=log(threshold_g)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none")# +
  #scale_y_break(c(30, 249.7), expand=T,scales="fixed")

omega_plot


# vitamin A
vitA_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Vitamin-A") %>%
  
  ggplot(aes (x=region,
              y=log(Quantity),
              fill = sea_food,
              colour=sea_food))+
  #geom_point(position=position_jitter(width=.15),
  #           size=.25,alpha=0.2)+
  geom_flat_violin(position=position_nudge(x=.2,y=0),
                   adjust=2,
                   alpha=0.3)+
  scale_colour_brewer(palette="Dark2",direction = -1)+
  scale_fill_brewer(palette="Dark2",direction = -1)+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~Nutrient)+
  coord_flip()+
  theme_cowplot()+
  guides(fill=FALSE) +
  ylab ("Daily per capita consumption (g, natural log scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=log(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())# + 
  #scale_y_break(c(1000, 94750), expand=T,scales="fixed")

vitA_plot

# arrange 

require(patchwork)
pdf(here ("output","patchwork_nutrients.pdf"),width =15,height = 12)

(ptn_plot | calcium_plot)/(iron_plot| zinc_plot)/(omega_plot | vitA_plot)

dev.off()


# ------------------------------------------



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
  # mutate (Ndays=n_distinct(DIA_SEMANA)) %>% # find the number of interviewing days
  group_by(region,state,income_cat,sea_food,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_class,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>%
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays))  %>% 
  group_by(region,sea_food) %>%  # income_cat # summarize by person
  summarise(across (QTD:Magnesium, ~mean(.x, na.rm=T))) %>% # sum of personal consumption
  
  # long format
  gather (Nutrient,Quantity,PTN:Magnesium) %>% 
  # add FAO thresholds (the col "Nutrient" automatically matches)
  right_join(FAO_threshold) %>%
  # remove NAs
  filter (is.na (Nutrient) != T ) %>%
  
  # edit seafood factor
  mutate_at ("sea_food", as.factor) %>%
  
  mutate(sea_food = recode(sea_food, "0" = "All sources",
                           "1" = "Seafood"
  ))


consumption_nutrients_day %>% 
  filter (Nutrient== "PTN") %>%
  ggplot (aes (x=Quantity,
               y=region,
               fill = sea_food)) +
  geom_bar(position="stack", stat="identity") +
  geom_vline (aes (xintercept=threshold),
              color= "black",
              size=1.2,
              linetype="dashed")+
  facet_wrap(~Nutrient,scales = "free_x",nrow=1)+
  scale_fill_viridis_d(direction = -1, begin=0.2,end=0.8)


# end

rm(list=ls())
