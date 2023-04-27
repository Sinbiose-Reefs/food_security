

# --------------------------------------------

# projections and comparison with FAOs recommendations

# --------------------------------------------


source("R/packages.R")

# load functions
source ("R/functions.R")
source ("R/rainplot.R")

# ----------------


# load data for analysis
load (here ("output",
            "fishConsumption_Income_all_food.RData"))


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
  scale_y_break(c(30, 50), expand=T,scales="fixed")

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




dir.create(here("output", "model_results"))
# save
save (model.anova,
      file = here("output", "model_results", "model.anova.RData"))


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
nut <- unique(consumption_nutrients_day$Nutrient)

prop_FAO<- lapply (nut, function (nut) 
  
  lapply (d3, function (informant) {

        # subset
        d4 <- informant [informant$Nutrient == nut,] # nut == one nutrient
        # seafood
        seaf<- d4$QTD[which (d4$sea_food == "Seafood")]/sum(d4$QTD) # proportion of seafood in the diet
        seaf_nut <- d4$Quantity[which (d4$sea_food == "Seafood")] # seafood nutrient intake 
        seaf_quantity <- d4$QTD[which (d4$sea_food == "Seafood")] # daily quantity of seafood
        # proportion to achieve FAO's recommendations
        proportion_FAO <- seaf * FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)]/seaf_nut
        
        # all other sources
        # all_food<- d4$QTD[which (d4$sea_food == "All minus seafood")]/sum(d4$QTD)# proportion of remaining food
        all_sf<- d4$QTD[which (d4$sea_food == "All minus seafood")]/sum(d4$QTD) # proportion of all in the diet
        all_nut <- d4$Quantity[which (d4$sea_food == "All minus seafood")] # all nutrient intake 
        all_quantity <- d4$QTD[which (d4$sea_food == "All minus seafood")] # daily quantity of all
        # proportion to achieve FAO's recommendations
        proportion_FAO_all <- all_sf * FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)]/all_nut
        
        # dataframe
        # seafood
        df_seafood <- data.frame (proportion = as.numeric(seaf),
                          prop_to_achive = as.numeric(proportion_FAO),
                          already_meet = seaf>=proportion_FAO,
                          prop = as.numeric(proportion_FAO/seaf),
                          threshold = FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)],
                          nutrient = nut,
                          quantity=seaf_quantity,
                          type = "seafood"
                          
                          )
        # all food
        df_all_minus <- data.frame (proportion = as.numeric(all_sf),
                                    prop_to_achive = as.numeric(proportion_FAO_all),
                                    already_meet = all_sf>=proportion_FAO_all,
                                    prop = as.numeric(proportion_FAO_all/all_sf),
                                    threshold = FAO_threshold$threshold[which(FAO_threshold$Nutrient %in% d4$Nutrient)],
                                    nutrient = nut,
                                    quantity=all_quantity,
                                    type = "all_minus_seafood")
        
        res <- rbind (df_seafood,
                     df_all_minus)
        ;
        res
}))


# melt into df
df_prop_FAO <- lapply (prop_FAO, function (i)
  do.call(rbind,i)
)
df_prop_FAO <- do.call(rbind,df_prop_FAO)

# plot
tot_prop <- df_prop_FAO %>% 
  filter (nutrient == "Calcium") %>%
  ggplot (aes (x=log(prop),fill=type)) +
  geom_density(alpha=0.5) + 
  theme_bw() +
  geom_vline(aes(xintercept=median(log(prop)),group=type),linetype=2)

# proportion
# mean
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(mean(proportion))
# median
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(median(proportion))
# sd
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(sd(proportion))
# range
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(range(proportion))

# grams
# mean
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(mean(quantity))
# median
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(median(quantity))
# sd
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(sd(quantity))
# sd
df_prop_FAO %>%
  filter (nutrient == "PTN") %>% 
  group_by(type) %>%
  summarise(range(quantity))

# each nutr
each_nut <- lapply (nut, function (nut)
  
  df_prop_FAO %>%

    filter (nutrient == nut) %>%
    filter (prop != "Inf") %>%
  ggplot (aes (x=log(prop),
               fill = type,
               col=type)) +
  geom_vline(data = df_prop_FAO %>%
               
               filter (nutrient == nut) %>%
               filter (prop != "Inf") %>%
               group_by(type) %>%
               summarize (prop=median(prop,na.rm=T)),
               
               aes(xintercept=(log(prop))),
             linetype=2)+theme_bw()+
    theme(legend.position = "none")+
  geom_density(alpha=0.5) +
    
    facet_wrap(~nutrient) 
  
)


# arrange
pdf (here ("output", "proportionFAO.pdf"),width = 7,height=4)
grid.arrange(each_nut[[1]],each_nut[[2]],
             each_nut[[3]],each_nut[[4]],
             each_nut[[5]],each_nut[[6]],
             each_nut[[7]],ncol=4)
dev.off()

class(df_prop_FAO$prop)

df_prop_FAO %>%
  filter (prop != "Inf") %>%
  group_by(nutrient,type) %>%
  summarize (median (prop,na.rm=T)) 


