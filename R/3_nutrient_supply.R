
# -------------------------------------------------------------

# Nutrition
# load packages
require(here)
require(openxlsx)
require(vegan)
require(ape)
source ("functions.R")
require(ggrepel)
require(gridExtra)
require(dplyr)
require("openxlsx")



# load fisheries data (Freire et al. 2021)
fisheries <- read.xlsx (here ("data_fisheries_nutrients", 
                              "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
                        sheet = 2)


# adjust name
fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"
fisheries$TaxonName <- gsub (" ", "_", fisheries$TaxonName)



# separate genus, epithet
fisheries$genus <- do.call (rbind, strsplit (fisheries$TaxonName,"_"))[,1]
fisheries$epithet <- do.call (rbind, strsplit (fisheries$TaxonName,"_"))[,2]

# the do.call is repeating the name for species lacking epithet


fisheries$epithet <- ifelse (fisheries$genus == fisheries$epithet, 
                             NA,
                             fisheries$epithet)


# taxonomic level

# genus equal to epithet (no problem -- they differ regarding upper and lower case)
# check example
# fisheries [grep ("Bagre", fisheries$genus),"genus"] == fisheries [grep ("Bagre", fisheries$genus),"epithet"]


#fisheries$level <- ifelse (fisheries$epithet == "sp", "genus", 
#        
#                           ifelse (fisheries$epithet == "spp", "genus",
#                
#                                   
#                                           "species")
#                                   
#                           )

# NAs belong to broader taxonomic groups

# unique (fisheries$genus [is.na(fisheries$epithet)])
# fisheries$level [is.na(fisheries$epithet)] <- "other"



# worms's validation of taxonomic ranks
#require(worrms)
#
fisheries$TaxonName <- gsub ("_", " ", fisheries$TaxonName)
## search
#worms_record_fish <- lapply (unique(fisheries$TaxonName), function (i) 
#  
#  tryCatch (
#    
#    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
#    
#    error = function (e) print(NA)
#    
#    
#  )
#  
#)
#

## save this
#save (worms_record_fish, file = "worms_fish.RData")
load(here ("output","worms_fish.RData"))


# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record_fish))
#df_worms_record$scientificname <- gsub ("", "_", df_worms_record$scientificname)


# valid name WoRMS 
fisheries$scientificName <- (df_worms_record$scientificname [match (fisheries$TaxonName,
                                                                     (df_worms_record$scientificname))])

# taxon rank of the identified level
fisheries$taxonRank <- (df_worms_record$rank [match (fisheries$TaxonName,
                                                     (df_worms_record$scientificname))])


# match & bind
# or taxonID
fisheries$scientificNameID<-(df_worms_record$lsid [match (fisheries$TaxonName,
                                                          (df_worms_record$scientificname))])

# kingdom
fisheries$kingdom<-(df_worms_record$kingdom [match (fisheries$TaxonName,
                                                          (df_worms_record$scientificname))])
# class
fisheries$class<-(df_worms_record$class [match (fisheries$TaxonName,
                                                          (df_worms_record$scientificname))])
# family
fisheries$family<-(df_worms_record$family [match (fisheries$TaxonName,
                                                            (df_worms_record$scientificname))])



# not only fish
unique(fisheries[which(fisheries$class == 'Malacostraca'),"TaxonName"])




# matching with nutrients (predictions from Hicks et al. 2019)
# only fish


nutrients <- read.csv (here ("data_fisheries_nutrients",
                             "Species_Nutrient_Predictions.csv"))




#  genus
nutrients$species <- gsub ("_", " ", nutrients$species)
nutrients$Genus <- unlist(lapply (strsplit (nutrients$species, " "), function (i) i [[1]]))



# dataset of species nutrients
fisheries_species <- cbind (fisheries,
                    
                    
                    nutrients [match (fisheries$scientificName,
       
                                      nutrients$species
       
                                      ), #grep ("mu",colnames(nutrients) )
                               ]

)


# removing missing data (at higher levels than species)
fisheries_species <- fisheries_species[is.na(fisheries_species$scientificName) != T,]




# matching  nutrient data for non-fish resources
TBCA_nutrients <- read.xlsx (here ("data_fisheries_nutrients","Nutrients_TBCA.xlsx"),sheet=1)
TBCA_nutrients$especific_source <- gsub ("_", " ", TBCA_nutrients$especific_source)


# set trace nutrients (tr) to 0.005 , from here http://www.tbca.net.br/

TBCA_nutrients$`VA(mcg)_re` <- ifelse (TBCA_nutrients$`VA(mcg)_re` =="tr", 0.005,TBCA_nutrients$`VA(mcg)_re`)
TBCA_nutrients$`VA(mcg)_rae` <- ifelse (TBCA_nutrients$`VA(mcg)_rae` =="tr", 0.005,TBCA_nutrients$`VA(mcg)_rae`)
TBCA_nutrients$`Fe(mg)` <- ifelse (TBCA_nutrients$`Fe(mg)` =="tr", 0.005,TBCA_nutrients$`Fe(mg)`)


# change colnames to match with other nutrient data
colnames(TBCA_nutrients) <- c("scientificName", "protein_type", "Protein_mu", "Zinc_mu", "Selenium_mu", "Calcium_mu", 
                              "Iron_mu", "Vitamin_A_RE","Vitamin_A_mu", "Omega_3_mu",  "Seafood")

# numeric
TBCA_nutrients <- TBCA_nutrients %>% mutate(Protein_mu = as.numeric(Protein_mu),
                          Zinc_mu = as.numeric(Zinc_mu),
                          Selenium_mu = as.numeric(Selenium_mu),
                          Calcium_mu = as.numeric(Calcium_mu),
                          Iron_mu = as.numeric(Iron_mu),
                          Vitamin_A_RE = as.numeric(Vitamin_A_RE),
                          Vitamin_A_mu = as.numeric(Vitamin_A_mu),
                          Omega_3_mu = as.numeric(Omega_3_mu))

# raw table (used to plot general content of nutrients)
TBCA_nutrients_raw <- TBCA_nutrients

# match
TBCA_nutrients <- (TBCA_nutrients [which (TBCA_nutrients$scientificName %in% fisheries_species$scientificName),])

# remove fish
TBCA_nutrients<-TBCA_nutrients[which(TBCA_nutrients$protein_type %in% c("Ifish", "SWfish") !=T),]

# bind nutrients (each species in each step)
fisheries_species [which(fisheries_species$scientificName %in% TBCA_nutrients$scientificName[1]),
                   c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]  <- TBCA_nutrients[which(TBCA_nutrients$scientificName %in% TBCA_nutrients$scientificName[1]),
                                                                                                                                  c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]
fisheries_species [which(fisheries_species$scientificName %in% TBCA_nutrients$scientificName[2]),
                   c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]  <- TBCA_nutrients[which(TBCA_nutrients$scientificName %in% TBCA_nutrients$scientificName[2]),
                                                                                                                                  c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]
fisheries_species [which(fisheries_species$scientificName %in% TBCA_nutrients$scientificName[3]),
                   c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]  <- TBCA_nutrients[which(TBCA_nutrients$scientificName %in% TBCA_nutrients$scientificName[3]),
                                                                                                                                  c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]
fisheries_species [which(fisheries_species$scientificName %in% TBCA_nutrients$scientificName[4]),
                   c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]  <- TBCA_nutrients[which(TBCA_nutrients$scientificName %in% TBCA_nutrients$scientificName[4]),
                                                                                                                                  c("Protein_mu","Zinc_mu","Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]

# matching at higher levels

TBCA_nutrients_raw$genus <- sapply (strsplit (TBCA_nutrients_raw$scientificName, " "), "[[",1)

# select genus of fish dataset with missing nutrient data
sel_genus <-unique(fisheries_species[is.na(fisheries_species$Calcium_mu),"genus"])[which(unique(fisheries_species[is.na(fisheries_species$Calcium_mu),"genus"]) %in% TBCA_nutrients_raw$genus)]

# data to fill
data_to_fill <- fisheries_species[which(is.na(fisheries_species$Calcium_mu) & 
                          fisheries_species$genus %in% sel_genus)
                  ,which(colnames(fisheries_species) %in% colnames(TBCA_nutrients_raw))]

# match
data_to_match <- TBCA_nutrients_raw [match (data_to_fill$genus, TBCA_nutrients_raw$genus),which(colnames(TBCA_nutrients_raw) %in% colnames(data_to_fill))]
rownames(data_to_match) <- rownames(data_to_fill)
# match
fisheries_species[which(rownames(fisheries_species) %in% rownames(data_to_match)),c("Protein_mu", "Zinc_mu", "Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")] <-data_to_match[,c("Protein_mu", "Zinc_mu", "Selenium_mu","Calcium_mu","Iron_mu","Vitamin_A_mu","Omega_3_mu")]


# remove missing data
fisheries_species  <- fisheries_species[is.na(fisheries_species$Protein_mu) != T,] # removing also other taxa besides fish (missing nutritional data)

table(fisheries_species$taxonRank) # only species

# obtain high tax level data
fisheries_genus <- fisheries[is.na(fisheries$taxonRank) == T,]


# nutrients at the genus level
require(dplyr)

nutrients_genus <- nutrients %>% 
  
  group_by(Genus) %>%
  
  summarise(Selenium_mu = mean(Selenium_mu,na.rm=T),
            Selenium_l95 = mean(Selenium_l95,na.rm=T),
            Selenium_u95 =mean(Selenium_u95,na.rm=T),
            Zinc_mu = mean(Zinc_mu,na.rm=T),
            Zinc_l95 = mean(Zinc_l95,na.rm=T),
            Zinc_u95 =mean(Zinc_u95,na.rm=T),
            Protein_mu = mean(Protein_mu,na.rm=T),
            Protein_l95 = mean(Protein_l95,na.rm=T),
            Protein_u95 = mean(Protein_u95,na.rm=T),
            Omega_3_mu = mean(Omega_3_mu,na.rm=T),
            Omega_3_u95 = mean(Omega_3_u95,na.rm=T),
            Omega_3_l95 = mean(Omega_3_l95,na.rm=T),
            Calcium_mu = mean(Calcium_mu,na.rm=T),
            Calcium_l95 = mean(Calcium_l95,na.rm=T),
            Calcium_u95 = mean(Calcium_u95,na.rm=T),
            Iron_mu = mean(Iron_mu,na.rm=T),
            Iron_u95 = mean(Iron_u95,na.rm=T),
            Iron_l95 = mean(Iron_l95,na.rm=T),
            Vitamin_A_mu = mean(Vitamin_A_mu,na.rm=T),
            Vitamin_A_u95 = mean(Vitamin_A_u95,na.rm=T),
            Vitamin_A_l95 = mean(Vitamin_A_l95,na.rm=T)
            
  )




# dataset of species nutrients
fisheries_genus_match <- cbind (fisheries_genus,
                    
                    
                          nutrients_genus [match (fisheries_genus$genus,
                                      
                                                  nutrients_genus$Genus
                                      
                    ), #grep ("mu",colnames(nutrients_genus))
                    ]
                    
)



# removing missing data (at higher levels than species)
fisheries_genus_match  <- fisheries_genus_match[is.na(fisheries_genus_match$Protein_mu) != T,] # removing also other taxa besides fish (missing nutritional data)


# bind data

fisheries_wtrait <- rbind (fisheries_genus_match,
                           fisheries_species[,
                                                 which(colnames(fisheries_species) %in%
                                                         colnames(fisheries_genus_match))])
# save
write.xlsx (fisheries_wtrait, file = here ("output", "fisheries_wtrait.xlsx"))



# ==============================================================

# amount of landed nutrients

# scale of nutrients
#  From Hicks et al.: 
# Plots show the concentrations of 
# calcium (in mg per 100 g), 
# iron (in mg per 100 g), 
# selenium (in µg per 100 g), 
# zinc (in mg per 100 g), 
# vitamin A (µg per 100 g), 
# omega-3 fatty acids (g per 100 g) 
# and protein (%) in each EEZ.


# function to transform g into kg
trans_qtd <- function (x) {x/0.1}


table_supply_state <- fisheries_wtrait %>% 
  filter (Year %in% seq (2000,2015,1)) %>% # choose a year
  mutate (across (ends_with("mu"),list(kg = trans_qtd)),
          CatchAmount_kg = CatchAmount_t*1000) %>% # catch into kg
  mutate_each(funs(.*CatchAmount_kg), ends_with("kg")) %>% # summarize by state
  group_by (OtherArea) %>% # group and 
  summarise(across (ends_with("kg"), list(~mean(.x,na.rm=T))))  # summarize per state

# save
save (table_supply_state, file = here ("output", "table_supply_state.RData"))



# most trapped fish

fisheries_wtrait %>%
  filter (OtherArea == "Pará") %>%
  filter (Year %in% seq (2000,2015,1)) %>% # choose a year
  group_by (TaxonName) %>%
  summarise(catch_spp = sum (CatchAmount_t)) %>%
  arrange(desc(catch_spp)) 




# ==============================================================


fish_year <- split (fisheries_wtrait, fisheries_wtrait$Year)


# Are fisheries getting nutritionally poorer over time?
# zinc

zinc_genus <- tapply (fisheries_wtrait$Zinc_mu,
                      list (fisheries_wtrait$TaxonName,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# iron
iron_genus <- tapply (fisheries_wtrait$Iron_mu,
                      list (fisheries_wtrait$TaxonName,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# omega 3
omega_genus <- tapply (fisheries_wtrait$Omega_3_mu,
                       list (fisheries_wtrait$TaxonName,
                             fisheries_wtrait$Region),
                       mean,na.rm=T)

# protein
#protein_genus <- tapply (fisheries_wtrait$Protein_mu,
#                      list (fisheries_wtrait$Genus_match,
#                            fisheries_wtrait$Region),
#                      mean,na.rm=T)

# calcium
calcium_genus <- tapply (fisheries_wtrait$Calcium_mu,
                         list (fisheries_wtrait$TaxonName,
                               fisheries_wtrait$Region),
                         mean,na.rm=T)

# selenium
selenium_genus <- tapply (fisheries_wtrait$Selenium_mu,
                          list (fisheries_wtrait$TaxonName,
                                fisheries_wtrait$Region),
                          mean,na.rm=T)

# vitA
vitA_genus <- tapply (fisheries_wtrait$Vitamin_A_mu,
                      list (fisheries_wtrait$TaxonName,
                            fisheries_wtrait$Region),
                      mean,na.rm=T)

# list of nutrient data
nutrient_data <- list (zinc_genus,iron_genus,omega_genus,
                       #protein_genus,
                       calcium_genus,selenium_genus , vitA_genus)


# match with catch amount data
#nutrient_data <- lapply (nutrient_data, function (i)
#  
#  i <- cbind (i, amount = catch_year_genus [match (rownames(i), 
#                           catch_year_genus$Genus_match), 
#                    "sum_catch"])
#  
#)


nsp_choose <- 20



# extract data
fish_year_nutrition <- lapply (fish_year, function (i) 
  do.call(rbind, lapply (nutrient_data, function (k)  { # bind nutrient data
    
    
    fish_year_genus <- tapply (i$CatchAmount_t,
                               list (i$TaxonName,
                                     i$Region),
                               sum)
    
    # Mean size among the most frequent nsp_choose 
    # across regions
    nut_reg <- do.call(cbind,   # melt nutrient data per region
                       
                       lapply (seq (1,ncol (fish_year_genus)), function (reg) {
                         
                         nut_collect_species <- names (fish_year_genus[,reg])[order(fish_year_genus[,reg],decreasing=T)][1:nsp_choose]
                         nut_collect_species <- k[which(names (k[,reg]) %in% nut_collect_species),reg]
                         mean_nut_collect_species<- mean(nut_collect_species,na.rm=T)
                         mean_nut_collect_species
                       }))
    colnames(nut_reg) <- colnames(fish_year_genus)
    ; # return
    nut_reg
    
    
  })))

# name rows
fish_year_nutrition<- lapply (fish_year_nutrition, function (i) {
  rownames (i)<- c("Zinc","Iron", "Omega-3", 
                   #"Protein",
                   "Calcium","Selenium","Vitamin-A")
  ;
  i
})

# name year
names (fish_year_nutrition) <- names(fish_year)

# melt
fish_year_nutrition <-( do.call(rbind.data.frame, fish_year_nutrition))
fish_year_nutrition$year <- sapply (strsplit ( rownames(fish_year_nutrition), "\\."), "[",1)
fish_year_nutrition$nutrient <- sapply (strsplit ( rownames(fish_year_nutrition), "\\."), "[",2)

# melt to fit ggplot format
require(reshape)
fish_year_nutrition<- melt(fish_year_nutrition,id.var=c("year", "nutrient"))
colnames(fish_year_nutrition)[which(colnames(fish_year_nutrition) == "variable")] <- "Region"

# plot 
require(ggplot2)
plot_nut <- ggplot (fish_year_nutrition, aes (x=year, 
                                              y=value,
                                              group=Region,
                                              colour=Region)) + 
  geom_point(alpha=0.2)+
  facet_wrap(~nutrient,scales = "free_y",ncol=3)+
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4))+
  scale_x_discrete(
    
    breaks = seq(min(fish_year_nutrition$year), 
                 max(fish_year_nutrition$year), by = 20),
    
  ) + 
  ylab ("Content of nutrients (unit per 100 g protein)") + 
  xlab("Year")+
  theme_classic() + 
  theme(axis.text = element_text(size=8),
        axis.title = element_text(size=13),
        strip.text = element_text(face="bold"),
        strip.text.x = element_text(size = 10, color = "black", 
                                    face = "bold"),
        strip.background = element_rect(color="black", 
                                        fill="gray90",
                                        size=1.5, linetype="solid"
        ),
        legend.position = "top",
        legend.direction = "horizontal") + 
  scale_fill_distiller(palette = "Spectral") 


plot_nut



# -----------------------------------------------------------------------

# ordination to show the nutrition content of fish genus


# find the 20 most catched spp in BR


most_catched_BR <- tapply (fisheries_wtrait$CatchAmount_t,
        list (fisheries_wtrait$TaxonName
        ),
        sum)
names(most_catched_BR[order(most_catched_BR,decreasing=T)][1:20])


# the 20 most catched in each region



fish_genus_region <- tapply (fisheries_wtrait$CatchAmount_t,
                           list (fisheries_wtrait$TaxonName,
                                 fisheries_wtrait$Region
                                 ),
                           sum)

# have the 20 most trapped spp
fish_genus_region <- lapply (seq (1,ncol(fish_genus_region)), function (i){
  
  data_per_region <- fish_genus_region[,i]
  spp_catch <- (data_per_region[order(data_per_region,decreasing=T)] [1:nsp_choose])
  # calculate the contribution to the total catch amount
  contribution<-sum(data_per_region[order(data_per_region,decreasing=T)] [1:nsp_choose],na.rm=T)/sum(data_per_region[order(data_per_region,decreasing=T)],na.rm=T)
  res <- list (spp_catch = spp_catch,
               contribution=contribution)
})

# list most catched spp
most_catched_list <- (sapply (fish_genus_region, "[[", "spp_catch",simplify=F))
names(most_catched_list) <- c("North","Northeast", "Southeast", "South")

# table of names

tab_names <- lapply (most_catched_list, function (i) 
  
  data.frame (Catch_amount =i,
              Species_taxon= names(i),
             Abbreviation = paste (substr (sapply (strsplit (names(i), " "), "[[", 1),1,3),
                         substr (sapply (strsplit (names(i), " "), "[[", 2),1,3),
                         sep=" ")
  )
)
# melt
tab_names<-do.call(rbind, tab_names)

# save
write.xlsx (tab_names, file = here ("output", "tab_names_catch.xlsx"), 
            asTable = F, rowNames =T)



# gather the nutrient data for these spp

genus_nutrient_composition <- lapply (nutrient_data, function (i) 
  lapply (seq(1,ncol (i)), function (k){
  
        sel_spp <- i [,k] # which fish is in the list of most catched spp
        sel_spp<-sel_spp[which(names(sel_spp) %in% names(most_catched_list[[k]]))];
        sel_spp
  }
  )
  )


# transform to have the nutrition data per region
genus_nutrient_composition <- lapply (seq (1,4), function (i) {# 4 regions
  
            data_nutrition <- sapply (genus_nutrient_composition, "[[",i)
            colnames(data_nutrition) <- c("Zinc","Iron", "Omega-3",
                                          #"Protein",
                                          "Calcium","Selenium","Vitamin-A")
            # match names
            data_nutrition<-data_nutrition[match (names(most_catched_list[[i]]),rownames(data_nutrition)),]
            # multiply nutrients by catch amount
            data_nutrition<-data_nutrition * log(most_catched_list[[i]])
            data_nutrition<-decostand (data_nutrition,method= "standardize",na.rm=T)# standardize vals
            
            ;
            data_nutrition

             } )


# ordination analysis using these matrices as the basis
# one ordination per region
ordination_nut <- lapply(genus_nutrient_composition, function (i) {
  
    # distance matrix to project nutrients in the space
    dist_nut <- vegdist (i,  # weighted matrix
                       "euclidean",
                       na.rm=T)
      
    
    # ordination
    pcoa_nut <- pcoa(dist_nut)
    
    
    # variance explained by the first and second axes
    (Exp_axis1<-pcoa_nut$values$Eigenvalues[1]/sum(pcoa_nut$values$Eigenvalues)*100)
    (Exp_axis2<-pcoa_nut$values$Eigenvalues[2]/sum(pcoa_nut$values$Eigenvalues)*100)
    
    # data
    
    #pcoa_fish_year <- melt (pcoa_fish_year)
    pcoa_nut <- data.frame (pcoa_nut$vectors[,1:2],
                            sp = rownames(pcoa_nut$vectors))
    pcoa_nut$abb_spp <-paste (substr(sapply (strsplit(pcoa_nut$sp," "), "[[",1),1,3),
                              substr(sapply (strsplit(pcoa_nut$sp," "), "[[",2),1,3),sep=" ")
    
    
    # corelation to project nutrients
    correlation_nut <-data.frame( cor (i,
                                       pcoa_nut[,1:2],
                                       method = "pearson",
                                       use = "complete.obs"))
    correlation_nut$nutrient <- rownames(correlation_nut)
    # ordination
    # help here
    # https://ggplot2.tidyverse.org/reference/geom_path.html
    
    ordination_nut<-ggplot(data=pcoa_nut,
                           aes(x=Axis.1,y=Axis.2)) + 
      geom_point(colour="black",alpha=0.5,stroke=1.5,
                 shape=1,size=1.5) + # add the point markers
      
      #geom_text_repel(aes(label=sp ),size=3,
      #                vjust=1,
      #                max.overlaps=20)+
      
      geom_text(aes(label=ifelse(Axis.1>1.5 |
         Axis.1 < -1.5 |
         Axis.2 >1.5 |
         Axis.2 < -1.5,as.character(abb_spp),'')),
      size=2.5,vjust=1) +
      coord_equal() +
      theme_bw() +
      theme(legend.position = "none") + 
      xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
      ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))+
      xlim (c(-6,5))+#(c(min(pcoa_nut$Axis.1)-2,max(pcoa_nut$Axis.1)+2)) + 
      ylim (c(-4,4))#(c(min(pcoa_nut$Axis.2)-2,max(pcoa_nut$Axis.2)+2)) 
    
    
    
    # correction factor to avoid label overlap
    fact_corr <- 3
    # project genus names
    ordination_nut<-ordination_nut + 
      
      geom_text(data = correlation_nut,
                aes (x=Axis.1*fact_corr,
                     y = Axis.2*fact_corr,
                     label = (nutrient)),
                size=3,fontface = "italic",
                colour="#5BB318")  
    
    ordination_nut
    
    # arrows
    ordination_nut <- ordination_nut + geom_segment(aes(x = 0, y = 0, 
                                                        xend = correlation_nut[1,1]*fact_corr, 
                                                        yend = correlation_nut[1,2]*fact_corr),size = 1,
                                                    color="#2B7A0B",alpha=0.05,
                                                    arrow = arrow(length = unit(.35, "cm"))) + 
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlation_nut[2,1]*fact_corr, 
                       yend = correlation_nut[2,2]*fact_corr),size = 1,
                   color="#2B7A0B",alpha=0.05,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlation_nut[3,1]*fact_corr, 
                       yend = correlation_nut[3,2]*fact_corr),size = 1,
                   color="#2B7A0B",alpha=0.05,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlation_nut[4,1]*fact_corr, 
                       yend = correlation_nut[4,2]*fact_corr),size = 1,
                   color="#2B7A0B",alpha=0.05,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlation_nut[5,2]*fact_corr, 
                       yend = correlation_nut[5,2]*fact_corr),size = 1,
                   color="#2B7A0B",alpha=0.05,
                   arrow = arrow(length = unit(.35, "cm"))) + 
      geom_segment(aes(x = 0, y = 0, 
                       xend = correlation_nut[6,1]*fact_corr, 
                       yend = correlation_nut[6,2]*fact_corr),size = 1,
                   color="#2B7A0B",alpha=0.05,
                   arrow = arrow(length = unit(.35, "cm")))
    
    
            
            ; # return
            ordination_nut


})



# arrange and save
  




pdf (here ("output", "nutrients_composition"),height=6,width=6)

composition2<-grid.arrange(ordination_nut[[1]]+ggtitle ("North"),
                           ordination_nut[[2]]+ggtitle ("Northeast"),
                           ordination_nut[[3]]+ggtitle ("Southeast"),
                           ordination_nut[[4]]+ggtitle ("South"),
                           ncol=4,nrow=4,
                           layout_matrix = rbind (c (1,1,2,2),
                                                  c (1,1,2,2),
                                                  c (3,3,4,4),
                                                  c (3,3,4,4)))



dev.off()

# nutrient over time



pdf (here ("output", "nutrients_time.pdf"),height=6,width=6)

plot_nut

dev.off()




# coeff plot


nutrients_more_catched <- (fisheries_wtrait[which(fisheries_wtrait$TaxonName %in% names(most_catched_BR[order(most_catched_BR,decreasing=T)][1:20])
),])

# SUMMARIZE data by taxonname
require(tidyr)
nutrients_more_catched <- nutrients_more_catched %>% 
  group_by(Sector,TaxonName) %>%
  select (Selenium_mu:Vitamin_A_l95) %>%
  summarise(across(everything(), list(mean))) #%>% 
  #pivot_longer(cols=Selenium_mu_1:Vitamin_A_l95_1,names_to = 'Category', values_to = 'Total')%>%
  #mutate( Total = as.numeric( Total ))# %>%
  #separate( Category,  c("Nutrient", "N","Vals"))


# ucides = the average of other mollusks
nutrients_more_catched [grep ("Ucides",nutrients_more_catched$TaxonName),"Omega_3_mu_1"] <-TBCA_nutrients_raw %>%
  filter (protein_type == "crustacean") %>%
  group_by(protein_type) %>%
  summarise (across (ends_with("_mu"), ~mean(.x,na.rm=T))) %>%
  select(Omega_3_mu)



# organize data to plot
nutrients_to_plot <- lapply (c("Zinc","Iron", "Omega_3", "Calcium","Selenium","Vitamin_A"), function (i)  {
                 
  # subset of columns
  edited_data <- nutrients_more_catched[,grep (i,colnames(nutrients_more_catched))]
  # order
  edited_data<- edited_data[,order(colnames(edited_data))]
  # naming
  colnames(edited_data) <- c("Lower", "Average", "Upper")
  edited_data <- cbind (nutrients_more_catched[,c("Sector", "TaxonName")],
                        edited_data,
                        Nutrient=i);
  edited_data
  

  
  })

#  melt
nutrients_to_plot<- do.call(rbind, nutrients_to_plot)
# adjust names
nutrients_to_plot<- nutrients_to_plot %>%
  mutate (original_name= TaxonName) %>%
  separate(TaxonName, c("first", "second") ) %>%
  mutate (gen = substr(first,1,3),
          spp=substr(second,1,3),
          abb_name = paste (gen, spp))


# plot  
# groups means to plot
gr.means<- nutrients_to_plot %>% 
  group_by(Nutrient) %>%
  summarise_all(mean,na.rm=T)


# plot 1 (the most catched spp)
mc_spp <- ggplot (nutrients_to_plot,aes (y=reorder(abb_name,Average),
                x=Average ))+
  geom_point() +
  geom_vline(data= gr.means,
             aes (xintercept = Average),
             size=1,
             linetype=1,
             alpha=0.3,
             col="red") +
  geom_errorbar(aes(xmin=Lower,xmax=Upper),width=0.15)+
  facet_wrap(~Nutrient,scales = "free_x",ncol=6)+
  theme_bw() +
  theme(axis.text.y=element_text(size=10),
        axis.title.x = element_text(size=10),
        panel.background = element_rect(fill = "lightyellow",
                                        colour = "gray",
                                        size = 0.5, 
                                        linetype = "solid"))+
  ylab ("Taxon Name")+
  xlab ("Nutrient content (Average and Credible Interval)") +
  labs(subtitle = "A) Nutrient content of the 20 most catched taxa (Freire et al., 2021)")




# plot of general types of protein
# summarize

# match fish nutrients in the TBCA_nutrients_raw table

taxon_nutrient <- bind_rows (
  # brazilian table of nutrients
  TBCA_nutrients_raw %>% 
                      filter (protein_type != "SWfish") %>%  # rm sea water fish
                      group_by(protein_type) %>%
                      summarise(across(Protein_mu:Omega_3_mu, ~ mean(.x, na.rm = TRUE))),
  # nutrients hicks                    
  fisheries_wtrait %>%
                     filter (class %in% c("Actinopteri","Elasmobranchii")) %>%
                     #filter (TaxonName %in% nutrients_more_catched$TaxonName)%>% # the most catched fish
                     group_by(class) %>%
                     summarize (across (ends_with("_mu"), ~mean(.x,na.rm=T))) %>%
                     dplyr::rename("protein_type"="class"))%>%
  
  
  select ("protein_type",#"Protein_mu", 
          "Zinc_mu", "Selenium_mu", "Calcium_mu", "Iron_mu", 
          #"Vitamin_A_RE",
          "Vitamin_A_mu", "Omega_3_mu") %>%
  mutate(protein_type = forcats::fct_relevel(protein_type, c(
                                     "beef",
                                     "goat",
                                     "pork",
                                     "poultry",
                                     "FWfish",
                                     "Ifish",
                                     "crustacean",
                                     "cephalopod",
                                     "mollusk",
                                      "Elasmobranchii", 
                                     "Actinopteri"))) %>%
  
  mutate(protein_type = recode(protein_type,
                               "beef" = "Beef",
                               "goat" = "Goat",
                               "pork" = "Pork",
                               "poultry" = "Poultry",
                               "FWfish" = "Freshwater fish",
                               "Ifish" = "Imported fish",
                               "crustacean" = "Crustacean",
                               "cephalopod" = "Cephalopods",
                               "mollusk" = "Mollusks")) %>%
  
  pivot_longer(-protein_type) 
  
  

# group means per taxon
gr.means.tax <- taxon_nutrient %>% 
  group_by(name) %>% 
  summarise(value= mean(value, na.rm = TRUE))
  
# plot FAO thresholds

#FAO<- read.xlsx(here ("data_fisheries_nutrients","Threshold_FAO.xlsx"),
#                sheet=1)
#
## match
#FAO<-FAO [match (gr.means.tax$name, FAO$label),]
# plot 

plot2<- ggplot (taxon_nutrient,aes(x=value, 
                                   y=protein_type))+#reorder(protein_type, value)))+
    geom_point()+
    facet_wrap(~name ,scales = "free_x",ncol=6)+
  theme_bw() +
  theme(axis.text.y=element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=0),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "lightcyan",
                                        colour = "gray",
                                        size = 0.5, 
                                        linetype = "solid"))+
  ylab ("")+
  xlab ("Nutrient content")+
  geom_vline(data= gr.means.tax,
             aes (xintercept = value),
             size=1,
             linetype=1,
             alpha=0.3,
             col="red") + 
  labs(subtitle = "B) Nutrient content in several protein sources (Brazilian Food Composition Table, 2022; Hicks et al. (2019))")
  




# arrange
pdf(here ("output", "Nutrients_supply_consumed"),width=10,height=8)
gridExtra::grid.arrange(mc_spp,
             plot2,
             nrow=5,
             ncol=7,
             layout_matrix=as.matrix(
               
               rbind (
                      c(1,1,1,1,1,1,1),
                      c(1,1,1,1,1,1,1),
                      c(1,1,1,1,1,1,1),
                      c(2,2,2,2,2,2,2),
                      c(2,2,2,2,2,2,2))
               )
)

dev.off()

# end
rm(list=ls())



