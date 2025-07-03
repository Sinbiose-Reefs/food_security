
# ----------------------------------------------------------------------------------------------


#              Work with fisheries data from Freire et al. (2021)
#       Create values of seafood supply to compare with the demand calculated from POF(RFB) data
#                    Match nutrient and fisheries data


# ----------------------------------------------------------------------------------------------

# load packages
rm(list=ls())
source ("R/functions.R")
source ("R/packages.R")

# load fisheries data (Freire et al. 2021)
fisheries <- read.xlsx (here ("data_fisheries_nutrients", 
                              "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
                        sheet = 2)

# point size (calculate tht total catch per year and state)
catch_year <- tapply (fisheries$CatchAmount_t,
                      list(fisheries$OtherArea,
                           fisheries$Year),
                      sum) 

# adjust name
fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"
fisheries$TaxonName <- gsub (" ", "_", fisheries$TaxonName)
# separate genus, epithet
fisheries$genus <- do.call (rbind, strsplit (fisheries$TaxonName,"_"))[,1]
fisheries$epithet <- do.call (rbind, strsplit (fisheries$TaxonName,"_"))[,2]

# the do.call is repeating the name for species lacking epithet - adjust below
fisheries$epithet <- ifelse (fisheries$genus == fisheries$epithet, 
                             NA,
                             fisheries$epithet)
# taxonomic level
# genus equal to epithet (no problem -- they differ regarding upper and lower case)
# check example
# fisheries [grep ("Bagre", fisheries$genus),"genus"] == fisheries [grep ("Bagre", fisheries$genus),"epithet"]
# NAs belong to broader taxonomic groups - check:
# unique (fisheries$genus [is.na(fisheries$epithet)])
# fisheries$level [is.na(fisheries$epithet)] <- "other"

# worms's validation of taxonomic ranks -------------------------
# done and saved as RData (if want to redo remove the "#" from the next lines)
fisheries$TaxonName <- gsub ("_", " ", fisheries$TaxonName)

## search
#require(worrms)
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
#save (worms_record_fish, file = here ("processed_data","worms_fish.RData"))
load(here ("processed_data","worms_fish.RData")) # load the taxonomic rank data

# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record_fish))

# valid name WoRMS 
fisheries$scientificName <- (df_worms_record$scientificname [match (fisheries$TaxonName,
                                                                     (df_worms_record$scientificname))])

# taxon rank of the identified level
fisheries$taxonRank <- (df_worms_record$rank [match (fisheries$TaxonName,
                                                     (df_worms_record$scientificname))])


# match & bind data 
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

# not only fish in the data set
unique(fisheries[which(fisheries$class == 'Malacostraca'),"TaxonName"])

# load nutrient data from FishBase ---------------------------------------
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

# matching  nutrient data for non-fish resources (From TBCA) ------------------------------
TBCA_nutrients <- read.xlsx (here ("data_fisheries_nutrients","Nutrients_TBCA.xlsx"),sheet=1)
TBCA_nutrients$especific_source <- gsub ("_", " ", TBCA_nutrients$especific_source)

# set trace nutrients (tr) to 0.005, from here http://www.tbca.net.br/
TBCA_nutrients$`VA(mcg)_re` <- ifelse (TBCA_nutrients$`VA(mcg)_re` =="tr", 0.005,TBCA_nutrients$`VA(mcg)_re`)
TBCA_nutrients$`VA(mcg)_rae` <- ifelse (TBCA_nutrients$`VA(mcg)_rae` =="tr", 0.005,TBCA_nutrients$`VA(mcg)_rae`)
TBCA_nutrients$`Fe(mg)` <- ifelse (TBCA_nutrients$`Fe(mg)` =="tr", 0.005,TBCA_nutrients$`Fe(mg)`)

# change colnames to match with other nutrient data
colnames(TBCA_nutrients) <- c("scientificName", "protein_type", "Protein_mu", "Zinc_mu", "Selenium_mu", "Calcium_mu", 
                              "Iron_mu", "Vitamin_A_RE","Vitamin_A_mu", "Omega_3_mu",  "Seafood")

# numeric
TBCA_nutrients <- TBCA_nutrients %>% 
                   mutate(Protein_mu = as.numeric(Protein_mu),
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
saveRDS (TBCA_nutrients_raw, file = here ("processed_data", "TBCA_nutrients_raw.rda"))


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

# nutrients at the genus level (higher taxonomic level) 
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


# Matching nutrient and fisheries data
# dataset of nutrients per genus
fisheries_genus_match <- cbind (fisheries_genus,
                    
                    
                          nutrients_genus [match (fisheries_genus$genus,
                                      
                                                  nutrients_genus$Genus
                                      
                    ), #grep ("mu",colnames(nutrients_genus))
                    ]
                    
)


# removing missing data (at higher levels than species)
fisheries_genus_match  <- fisheries_genus_match[is.na(fisheries_genus_match$Protein_mu) != T,] # removing also other taxa besides fish (missing nutritional data)


# rbind data at the genus and species level
# This will create a big table with all data: fisheries + nutrient data, at several different taxonomic levels
fisheries_wtrait <- rbind (fisheries_genus_match,
                           fisheries_species[,
                                                 which(colnames(fisheries_species) %in%
                                                         colnames(fisheries_genus_match))])
# save
write.xlsx (fisheries_wtrait, file = here ("processed_data", "fisheries_wtrait.xlsx"))
save (fisheries_wtrait, file = here ("processed_data", "fisheries_wtrait.RData"))
load(file = here ("processed_data", "fisheries_wtrait.RData"))

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


# point size (catch per year)
catch_year <- tapply (fisheries_wtrait$CatchAmount_t,
                      list(fisheries_wtrait$OtherArea,
                           fisheries_wtrait$Year),
                      sum) 


# transform nutrients into grams
fisheries_wtrait <- fisheries_wtrait %>% 
  
  mutate (Vitamin_A_mu = Vitamin_A_mu/1e+6,
          Calcium_mu = Calcium_mu/1000,
          Zinc_mu = Zinc_mu/1000,
          Iron_mu = Iron_mu/1000,
          Selenium_mu = Selenium_mu/1e+6) %>% # into grams
  
  
  mutate_each(funs((.*10)/1000), ends_with("mu")) # content g per 1kg

# protein
# 1 g (observed value) --- 100 g
# x g --- 1000 g
# (1*1000)/100 = 10


# gramas por kilograma --> kg / desembarque

# 10 g / 1 kg

# 1 ---- 1000
# x  ---- 10

# 10/1000

# transform tons of catch into kg
fisheries_wtrait$CatchAmount_kg_mu <- fisheries_wtrait$CatchAmount_t*1000

# calculate nutrient landings
table_supply_state  <- fisheries_wtrait %>%
  filter (Year == 2015) 

# calculate 
table_supply_state$Vitamin_A_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Vitamin_A_mu)
table_supply_state$Iron_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Iron_mu)
table_supply_state$Zinc_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Zinc_mu)
table_supply_state$Omega_3_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Omega_3_mu)
table_supply_state$Protein_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Protein_mu)
table_supply_state$Calcium_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Calcium_mu)
table_supply_state$Selenium_mu <- (table_supply_state$CatchAmount_kg_mu * table_supply_state$Selenium_mu)

# function to transform g into kg
# trans_qtd <- function (x) {(x/1000)} #  and then kg / kg

# calculate the supply (summarize per state)

# point size (catch per year)
table_supply_state <- data.frame (
   
 
  
  CatchAmount_kg_mu = tapply (table_supply_state$CatchAmount_kg_mu,
                       list(table_supply_state$OtherArea),
                       sum,na.rm=T) ,     
  Protein_mu = tapply (table_supply_state$Protein_mu,
                      list(table_supply_state$OtherArea),
                      sum,na.rm=T) ,
  Iron_mu = tapply (table_supply_state$Iron_mu,
                       list(table_supply_state$OtherArea),
                       sum,na.rm=T) ,
  Zinc_mu = tapply (table_supply_state$Zinc_mu,
                    list(table_supply_state$OtherArea),
                    sum,na.rm=T) ,
  Omega_3_mu = tapply (table_supply_state$Omega_3_mu,
                         list(table_supply_state$OtherArea),
                         sum,na.rm=T) ,

  Calcium_mu = tapply (table_supply_state$Calcium_mu,
                         list(table_supply_state$OtherArea),
                         sum,na.rm=T) ,
  Vitamin_A_mu = tapply (table_supply_state$Vitamin_A_mu,
                         list(table_supply_state$OtherArea),
                         sum,na.rm=T) ,
  Selenium_mu = tapply (table_supply_state$Selenium_mu,
                         list(table_supply_state$OtherArea),
                         sum,na.rm=T)
  )

# set names
table_supply_state$OtherArea <- rownames(table_supply_state)

# save
save (table_supply_state, file = here ("processed_data", "table_supply_state.RData"))

rm(list=ls())
