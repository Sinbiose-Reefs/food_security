

# --------------------------------------------

# projections and comparison with FAOs recommendations

# quantitative analysis (Figs 2 - 5)

# --------------------------------------------


source("R/packages.R")

# load functions
source ("R/functions.R")
source ("R/rainplot.R")

# ----------------

# load data for analysis
load (here ("processed_data",
            "fishConsumption_Income_all_food.RData"))


# check omega3
View (CONSUMO_ALIMENTAR %>%
  group_by (bluefood,food_type) %>%
  summarise (mean(Omega3,na.rm=T)))



# 1 - daily consumption
# filter the food
filter_interesting_food <- CONSUMO_ALIMENTAR %>% 
  #filter (single_PTN == 1) %>% # seafood & only raw protein
  mutate_at("sea_food", ~replace_na(.,0))  %>% 
  filter (position == "sea")


# load data - FAO's recommendations
FAO_threshold <- openxlsx::read.xlsx (here ("data_fisheries_nutrients", "Threshold_FAO.xlsx"))

# filter the days
consumption_nutrients_day <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  #filter (position == "sea") %>% # coastal states
  select (region,
          state,
          income_cat,
          COD_INFOR,
          COD_FAMILY,
          DIA_SEMANA,
          N_pop_state, 
          Ndays,
          sea_food,
          QTD, 
          ENERGIA_KCAL,
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
  
  mutate(sea_food = recode(sea_food, "0" = "All minus seafood",
                                    "1" = "Seafood"
  )) %>%

  
  group_by(region,state,income_cat,sea_food, COD_FAMILY,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_state,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% # 
  
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays))  %>% 
  
  # transform grams into 2000 kcalories
  #mutate_at(vars (PTN:Magnesium), funs((2000*.) / ENERGIA_KCAL)) %>%  
  
  # long format
  gather (Nutrient,Quantity,PTN:Magnesium) %>% 
  # add FAO thresholds (the col "Nutrient" automatically matches)
  right_join(FAO_threshold) %>%
  # remove NAs
  filter (is.na (Nutrient) != T )

# checks  
# unique(filter_interesting_food [which (filter_interesting_food$COD_INFOR == "15_1514_2_150036787_15_1_2"),"DIA_SEMANA"])
# (consumption_nutrients_day[which(consumption_nutrients_day$COD_INFOR == "15_1514_2_150036787_15_1_2"), "Ndays"] )
# View(consumption_nutrients_day[which(consumption_nutrients_day$COD_INFOR == "25_2501_1_250057870_5_1_1"), ] )


# all nutrients ()
consumption_nutrients_day_all <- filter_interesting_food %>%
  arrange(state)%>% # ordering states
  #filter (position == "sea") %>% # coastal states
  select (region,
          state,
          income_cat,
          COD_INFOR,
          COD_FAMILY,
          DIA_SEMANA,
          N_pop_state, 
          Ndays,
          sea_food,
          QTD, 
          ENERGIA_KCAL,
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
  
  group_by(region,state,income_cat,sea_food, COD_FAMILY,COD_INFOR) %>%  # summarize by person
  summarise(across (QTD:Magnesium, ~sum(.x, na.rm=T)), # sum of personal consumption
            mean_N_pop = mean(N_pop_state,na.rm=T), # N per pop class
            Ninterv = n_distinct(COD_INFOR),
            Ndays = mean(Ndays)) %>% # 
  
  mutate_at(vars (QTD:Magnesium), funs(. / Ndays))  %>% 
  
  # transform grams into 2000 kcalories
  #mutate_at(vars (PTN:Magnesium), funs((2000*.) / ENERGIA_KCAL)) %>%  
  
  # long format
  gather (Nutrient,Quantity,PTN:Magnesium) %>% 
  # add FAO thresholds (the col "Nutrient" automatically matches)
  right_join(FAO_threshold) %>%
  # remove NAs
  filter (is.na (Nutrient) != T )

                                           
# all data to plot                                         
consumption_nutrients_day <- bind_rows(consumption_nutrients_day, 
          consumption_nutrients_day_all) #all sources

                                         
                                         

# factor order
consumption_nutrients_day$region <- factor (consumption_nutrients_day$region, 
                                            levels = c("South",
                                                       "Southeast",
                                                       "Northeast", 
                                                       "North"))

# seafood order
consumption_nutrients_day$sea_food <- factor (consumption_nutrients_day$sea_food, 
                                            levels = c("All sources",
                                                       "All minus seafood",
                                                       "Seafood"))

# remove all sources
consumption_nutrients_day <- consumption_nutrients_day %>%
  filter (sea_food != "All sources")

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
  theme(legend.position = "right",
        axis.title.x = element_blank()) +
  scale_y_break(c(300, 500), expand=T,scales="fixed")

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
  scale_y_break(c(30, 50), expand=T,scales="fixed")
  
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
  scale_y_break(c(40, 80), expand=T,scales="fixed")

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
        axis.text.y = element_blank())+
  scale_y_break(c(2000, 4000), expand=T,scales="fixed")

calcium_plot


# omega 3
omega_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Omega3") %>%
  
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
  ylab ("Daily per capita consumption (mg, natural  scale)") +
  xlab ("Region")+
  geom_hline (aes (yintercept=(threshold)),
              color= "black",
              size=1,
              linetype="dashed") +
  theme(legend.position = "none") +
  scale_y_break(c(1.5, 2.2), expand=T,scales="fixed")

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
  scale_y_break(c(800, 46000), expand=T,scales="fixed")

vitA_plot


# vitamin A
magnesium_plot <- consumption_nutrients_day  %>%
  
  filter (Nutrient== "Magnesium") %>%
  
  #filter (Quantity < quantile (Quantity, 0.90)) %>%
  
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
  scale_y_break(c(1000, 3000), expand=T,scales="fixed")

magnesium_plot

# arrange 
pdf(here ("output","patchwork_nutrients_one_per_page.pdf"),width =8,height = 12, onefile=T)
              
lapply (list (ptn_plot,
              magnesium_plot, 
              calcium_plot, 
              iron_plot, 
              zinc_plot, 
              omega_plot, 
              vitA_plot), function (i)
                  
                i)
              
dev.off()
              
              

# patchwork
pdf(here ("output","patchwork_nutrients.pdf"),width =8,height = 12, onefile=T)
(ptn_plot)/ ( magnesium_plot | calcium_plot)/(iron_plot| zinc_plot)/(omega_plot | vitA_plot)
dev.off()
(ptn_plot)/ ( magnesium_plot) / (calcium_plot)/ (iron_plot)/ (zinc_plot)/(omega_plot) / (vitA_plot)


# ------------------------------------------


# statistical analysis


# --------------------------------------------
# analyze

# number of families
length(unique(consumption_nutrients_day$COD_FAMILY))
length(unique(consumption_nutrients_day$COD_INFOR))
              
# run model (ancova)
model.anova <- lapply (unique(consumption_nutrients_day$Nutrient), function (i)
  
  aov ((Quantity)- threshold~ region+Error (COD_FAMILY),#*(sea_food),#
       #offset = (threshold),
       data=consumption_nutrients_day %>% #[which(consumption_nutrients_day$),] %>%
         
         filter (sea_food == "Seafood") %>%
         
         filter (Nutrient == i)# %>%
         
         #filter (Quantity < quantile (Quantity, 0.90)) 
       
       )
  
)

# format output table
# anova table
formated_output1<- lapply (model.anova, function (i) {
    
   # extract error within and between
    error_between <- summary(i)[[1]][[1]]
    error_within <- summary(i)[[2]][[1]]
    
    df_output <- rbind (error_between,
                        error_within)
    rownames (df_output) <- c("Region", "Blocks", "Error (Within)")
    # add total
    df_total <- t(data.frame (Total=colSums(df_output)))
    df_output <- rbind (df_output,
                        df_total)  ;
    df_output
})
# set names
names(formated_output1) <- unique(consumption_nutrients_day$Nutrient)

# contrasts and comparisons to the intercept
rbind (
    # intercepts
    do.call(cbind, 
    lapply (model.anova, function (i) coefficients(i)[1])
    ),
    # coefficients
    do.call(cbind, 
            lapply (model.anova, function (i) unlist(coefficients(i)[2]))
    )
)

# error
rbind (
  # intercepts
  do.call(cbind, 
          lapply (model.anova, function (i) coef(summary.lm(i$`(Intercept)`))[, "Std. Error"])
  ),
  # coefficients
  do.call(cbind, 
          lapply (model.anova, function (i) coef(summary.lm(i$COD_FAMILY))[, "Std. Error"])
  )
)

# folder to host modeling results
dir.create(here("output", "model_results"))

# save
save (model.anova,
      file = here("output", "model_results", "model.anova.RData"))

load(file = here("output", "model_results", "model.anova.RData"))

# all food sources
# run model (ancova)
model.anova.all <- lapply (unique(consumption_nutrients_day$Nutrient), function (i)
  
  aov ((Quantity)- threshold~ region+Error (COD_FAMILY),#*(sea_food),#
       #offset = (threshold),
       data= consumption_nutrients_day %>% #[which(consumption_nutrients_day$),] %>%
         
         filter (sea_food == "All minus seafood") %>%
         
         filter (Nutrient == i) # %>%
         
         #filter (Quantity < quantile (Quantity, 0.90)) 
       
  )
  
)

# format output table
# anova table
formated_output2<- lapply (model.anova.all, function (i) {
  
  # extract error within and between
  error_between <- summary(i)[[1]][[1]]
  error_within <- summary(i)[[2]][[1]]
  
  df_output <- rbind (error_between,
                      error_within)
  rownames (df_output) <- c("Region", "Blocks", "Error (Within)")
  # add total
  df_total <- t(data.frame (Total=colSums(df_output)))
  df_output <- rbind (df_output,
                      df_total)  ;
  df_output
})

# set names
names(formated_output2) <- unique(consumption_nutrients_day$Nutrient)

# constrast and comparisons
rbind (
  # intercepts
  do.call(cbind, 
          lapply (model.anova.all, function (i) coefficients(i)[1])
  ),
  # coefficients
  do.call(cbind, 
          lapply (model.anova.all, function (i) unlist(coefficients(i)[2]))
  )
)

# error
rbind (
  # intercepts
  do.call(cbind, 
          lapply (model.anova.all, function (i) coef(summary.lm(i$`(Intercept)`))[, "Std. Error"])
  ),
  # coefficients
  do.call(cbind, 
          lapply (model.anova.all, function (i) coef(summary.lm(i$COD_FAMILY))[, "Std. Error"])
  )
)




# sample size in each ANOVA

consumption_nutrients_day %>% #[which(consumption_nutrients_day$),] %>%
  
  filter (sea_food == "Seafood") %>%
  
  filter (Nutrient == "PTN") %>% select(COD_INFOR) %>% unique() %>% nrow()



# all min seafood
consumption_nutrients_day %>% #[which(consumption_nutrients_day$),] %>%
  
  filter (sea_food == "All minus seafood") %>%
  
  filter (Nutrient == "PTN") %>% select(COD_INFOR) %>% unique() %>% nrow()



# save model results
save (model.anova.all,
      file = here("output", "model_results", "model.anova.all.RData"))

load(file = here("output", "model_results", "model.anova.all.RData"))

# --------------------------------

# project consumption
informant <- unique(consumption_nutrients_day$COD_INFOR)
d1<- split (consumption_nutrients_day %>% 
            filter ("sea_food" != "All sources") , 
            consumption_nutrients_day$COD_INFOR)

# subset to have mixed diet seafood and other
d2 <- lapply (d1, function (informant) 
  
  informant[which (informant$sea_food != "All sources"),]
  
)

# removing those that did not eat seafood
d3 <- d2 [unlist (lapply (d2,nrow))>7]

# 
proportion <- lapply (d3, function (informant) {

  d4 <- informant [informant$Nutrient == "Omega3",]
  seaf<- d4$QTD[which (d4$sea_food == "Seafood")]/sum(d4$QTD)
  all_food<- d4$QTD[which (d4$sea_food == "All minus seafood")]/sum(d4$QTD)
  seaf_nut <- d4$Quantity[which (d4$sea_food == "Seafood")]/sum(d4$QTD)
  seaf
  #(all_food * seaf_nut)/seaf
  #(seaf*4.39)/all_food
  
})

mean(unlist(proportion))
sd(unlist(proportion))
range(unlist(proportion))*100


# nutrients
nutrients <- unique(consumption_nutrients_day$Nutrient)
informant <- d3[[which(names(d3) == "15_1501_1_150082236_8_1_1")]] # check of infinites ( a case where no nutrient was achieved with the diet -- weird )
informant <- d3[[which(names(d3) == "15_1510_1_150034128_8_1_1")]]

# zeros entries
check_zeros <- do.call (rbind, d3)
table(check_zeros$Quantity == 0)[2]/sum(table(check_zeros$Quantity == 0))

# calculate
prop_FAO <- lapply (seq (1,100,1), function (prop) # proportions of seafood in the diet 
  
    lapply (nutrients, function (nut) 
  
          lapply (d3, function (informant) {
            
                # subset
                d4 <- informant [informant$Nutrient == nut,] # nut == one nutrient
                
                # seafood
                seaf <- (d4$QTD[which (d4$sea_food == "Seafood")]/sum(d4$QTD))*100 # proportion of seafood in the diet
                seaf_nut <- d4$Quantity[which (d4$sea_food == "Seafood")] # seafood nutrient intake 
                seaf_nut <- ifelse (seaf_nut == 0,0.0001,seaf_nut)
                seaf_quantity <- d4$QTD[which (d4$sea_food == "Seafood")] # daily quantity of seafood
                # proportion to achieve FAO's recommendations
                proportion_FAO <- (seaf * FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)])/seaf_nut
                  
                # all other sources
                # all_food<- d4$QTD[which (d4$sea_food == "All minus seafood")]/sum(d4$QTD)# proportion of remaining food
                all_sf<- (d4$QTD[which (d4$sea_food == "All minus seafood")]/sum(d4$QTD))*100 # proportion of all in the diet
                all_nut <- d4$Quantity[which (d4$sea_food == "All minus seafood")] # all nutrient intake 
                all_nut <- ifelse (all_nut ==0,0.0001,all_nut)
                all_quantity <- d4$QTD[which (d4$sea_food == "All minus seafood")] # daily quantity of all
                # proportion to achieve FAO's recommendations
                proportion_FAO_all <- (all_sf * FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)])/all_nut
                
                # projection in a mixed ( fith-fifth diet or any other proportion )
                all_ate <- all_quantity + seaf_quantity # all he/she ate
                
                # project seafood
                #proj_nut <- (seaf_nut*(all_ate*prop))/seaf_quantity
                proj_nut <- (seaf_nut*prop)/seaf
                proj_nut_FAO <- FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)]/proj_nut
                proj_nut_FAO_observed <- FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)]/seaf_nut
                
                # project all minus seafood
                # 1-prop = what's not seafood is the remaining food
                # proj_nut_all <- (all_nut*(all_ate*(1-prop)))/all_quantity
                proj_nut_all <- (all_nut*(100-prop))/all_sf
                proj_nut_all_FAO <- FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)]/proj_nut_all
                proj_nut_all_FAO_observed <- FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)]/all_nut
                
                
                # dataframe
                # seafood
                df_seafood <- data.frame (observed_proportion = as.numeric(seaf),
                                  prop_to_achive = as.numeric(proportion_FAO),
                                  FAO_already_meet = seaf>=proportion_FAO,
                                  #FAO_higher_obs_prop = as.numeric(proportion_FAO/seaf),
                                  threshold = FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)],
                                  nutrient = nut,
                                  quantity= seaf_quantity,
                                  proj_nut_FAO= proj_nut_FAO, # projection of a given percentage of the diet being seafood
                                  FAO_observed = proj_nut_FAO_observed,
                                  prop_diet_projected = prop,
                                  prop_observed = seaf,
                                  type = "seafood"
                                  
                                  )
                # all food
                df_all_minus <- data.frame (observed_proportion = as.numeric(all_sf),
                                            prop_to_achive = as.numeric(proportion_FAO_all),
                                            FAO_already_meet = all_sf>=proportion_FAO_all,
                                            #FAO_higher_obs_prop = as.numeric(proportion_FAO_all/all_sf),
                                            threshold = FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)],
                                            nutrient = nut,
                                            quantity=all_quantity,
                                            proj_nut_FAO=proj_nut_all_FAO, # projection of half diet being seafood
                                            FAO_observed = proj_nut_all_FAO_observed,
                                            prop_diet_projected = (100-prop),
                                            prop_observed = all_sf,
                                            type = "all_minus_seafood")
                
                res <- rbind (df_seafood,
                             df_all_minus)
                ;
                res
        })
        )
)


# save projections FAO
save (prop_FAO, file = here ("output", "projections_fao.RData"))

