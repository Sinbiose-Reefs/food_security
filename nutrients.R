
# -------------------------------------------------------------

# Nutrition


# load packages
require(here)
require(openxlsx)

source ("functions.R")


# load fisheries data (Freire et al. 2021)
fisheries <- read.xlsx (here ("data_fisheries_nutrients", 
                              "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
                        sheet = 2)

# adjust name
fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"
fisheries$TaxonName <- gsub (" ", "_", fisheries$TaxonName)


# separate genus, spp e zaz
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
require(worrms)


worms_record_fish <- lapply (unique(fisheries$TaxonName), function (i) 
  
  tryCatch (
    
    wm_records_taxamatch(i, fuzzy = TRUE, marine_only = TRUE)[[1]],
    
    error = function (e) print(NA)
    
    
  )
  
)



# two rows
df_worms_record <- data.frame(do.call(rbind,worms_record_fish))
df_worms_record$scientificname <- gsub (" ", "_", df_worms_record$scientificname)


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







# matching with nutrients (predictions from Hicks et al. 2019)
# only fish


nutrients <- read.csv (here ("data_fisheries_nutrients",
                             "Species_Nutrient_Predictions.csv"))




#  genus
nutrients$species <- gsub (" ", "_", nutrients$species)
nutrients$Genus <- unlist(lapply (strsplit (nutrients$species, "_"), function (i) i [[1]]))



# dataset of species nutrients
fisheries_species <- cbind (fisheries,
                    
                    
                    nutrients [match (fisheries$scientificName,
       
                                      nutrients$species
       
                                      ), grep ("mu",colnames(nutrients) )]

)


# removing missing data (at higher levels than species)
fisheries_species <- fisheries_species[is.na(fisheries_species$scientificName) != T,]
fisheries_species  <- fisheries_species[is.na(fisheries_species$Protein_mu) != T,] # removing also other taxa besides fish (missing nutritional data)
table(fisheries_species$taxonRank) # only species


# obtain high tax level data
fisheries_genus <- fisheries[is.na(fisheries$taxonRank) == T,]



# nutrients at the genus level
require(dplyr)

nutrients_genus <- nutrients %>% 
  
  group_by(Genus) %>%
  
  summarise(Selenium_mu = mean(Selenium_mu,na.rm=T),
            Zinc_mu = mean(Zinc_mu,na.rm=T),
            Protein_mu = mean(Protein_mu,na.rm=T),
            Omega_3_mu = mean(Omega_3_mu,na.rm=T),
            Calcium_mu = mean(Calcium_mu,na.rm=T),
            Iron_mu = mean(Iron_mu,na.rm=T),
            Vitamin_A_mu = mean(Vitamin_A_mu,na.rm=T)
            
  )




# dataset of species nutrients
fisheries_genus_match <- cbind (fisheries_genus,
                    
                    
                          nutrients_genus [match (fisheries_genus$genus,
                                      
                                                  nutrients_genus$Genus
                                      
                    ), grep ("mu",colnames(nutrients_genus) )]
                    
)



# removing missing data (at higher levels than species)
fisheries_genus_match  <- fisheries_genus_match[is.na(fisheries_genus_match$Protein_mu) != T,] # removing also other taxa besides fish (missing nutritional data)


# bind data

fisheries_wtrait <- rbind (fisheries_species,
                           fisheries_genus_match[,
                                                 which(colnames(fisheries_genus_match) %in%
                                                         colnames(fisheries_species))])

require("openxlsx")
write.xlsx (fisheries_wtrait, file = here ("output", "fisheries_wtrait.xlsx"))



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


# plot 
require(ggplot2)
plot_nut <- ggplot (fish_year_nutrition, aes (x=year, 
                                              y=value,
                                              group=variable,
                                              colour=variable)) + 
  geom_point(alpha=0.2)+
  facet_wrap(~nutrient,scales = "free_y",ncol=7)+
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs",k=4))+
  scale_x_discrete(
    
    breaks = seq(min(fish_year_nutrition$year), 
                 max(fish_year_nutrition$year), by = 20),
    
  ) + 
  ylab ("Content of nutrients") + 
  xlab("Year")+
  theme_classic() + 
  theme(axis.text = element_text(size=5),
        axis.title = element_text(size=13),
        strip.text = element_text(face="bold"),
        strip.text.x = element_text(size = 10, color = "black", 
                                    face = "bold"),
        strip.background = element_rect(color="black", 
                                        fill="gray60",
                                        size=1.5, linetype="solid"
        )) + 
  scale_colour_viridis_d(name = "Region", begin=0,end=1)



plot_nut



# ordination to show the nutrition content of fish genus
genus_nutrient_composition <- lapply (nutrient_data, function (i) 
  
  apply (i,1, mean,na.rm=T)
  
)
genus_nutrient_composition <- do.call(cbind,genus_nutrient_composition)
colnames(genus_nutrient_composition) <- c("Zinc","Iron", "Omega-3",
                                          #"Protein",
                                          "Calcium","Selenium","Vitamin-A")



# removing missing data
genus_nutrient_composition<-genus_nutrient_composition[which(rowSums(genus_nutrient_composition>0,
                                                                     na.rm=T)>0),]

genus_data <- rownames(genus_nutrient_composition)


# standardize values
genus_nutrient_composition<-apply (genus_nutrient_composition, 2,scale)
rownames(genus_nutrient_composition) <- genus_data


# weigth nutrient composition by catch amount
catch_amount_genus <- tapply (fisheries_wtrait$CatchAmount_t,
                              list (fisheries_wtrait$TaxonName),
                              sum,na.rm=T)
# removing missing data
catch_amount_genus<-catch_amount_genus[which(rownames (catch_amount_genus) %in% genus_data)]
catch_amount_genus[order(catch_amount_genus,decreasing=T)]

# standardize values
require(vegan)
catch_amount_genus<-decostand ((catch_amount_genus),method= "standardize")
catch_amount_genus[order(catch_amount_genus)]


# distance matrix
dist_nut <- vegdist (genus_nutrient_composition*as.matrix(catch_amount_genus)[,1],  # weighted matrix
                     "euclidean")

# ordination
require(ape)
pcoa_nut <- pcoa(dist_nut)


# variance explained by the first and second axes
Exp_axis1<-pcoa_nut$values$Eigenvalues[1]/sum(pcoa_nut$values$Eigenvalues)*100
Exp_axis2<-pcoa_nut$values$Eigenvalues[2]/sum(pcoa_nut$values$Eigenvalues)*100

# data

#pcoa_fish_year <- melt (pcoa_fish_year)
pcoa_nut <- data.frame (pcoa_nut$vectors[,1:2],
                        sp = rownames(pcoa_nut$vectors))



# corelation to project nutrients
correlation_nut <-data.frame( cor (genus_nutrient_composition,
                                   pcoa_nut[,1:2],
                                   method = "pearson"))
correlation_nut$nutrient <- rownames(correlation_nut)
# ordination
# help here
# https://ggplot2.tidyverse.org/reference/geom_path.html

require(ggrepel)
ordination_nut<-ggplot(data=pcoa_nut,
                       aes(x=Axis.1,y=Axis.2)) + 
  geom_point(colour="black",alpha=0.5,stroke=1.5,
             shape=1,size=3) + # add the point markers
  #geom_path(aes(colour=as.numeric(sp)),alpha=0.5)+
  
  geom_text(aes(label=sp ),size=3,vjust=1)+#ifelse(Axis.1>1.5 |
  #   Axis.1 < -1 |
  #   Axis.2 >1 |
  #   Axis.2 < -2,as.character(sp),'')),
  #size=2.5,vjust=1) +
  #geom_path(aess(group=year)) +# add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "none") + 
  xlab (paste ("Axis 1 (", round(Exp_axis1,2), "%)",sep="")) +
  ylab (paste ("Axis 2 (", round(Exp_axis2,2), "%)",sep=""))+
  xlim (c(-4.75,3)) + 
  ylim (c(-2.5,2)) 




# project genus names
ordination_nut<-ordination_nut + 
  
  geom_text(data = correlation_nut,
            aes (x=Axis.1*5,
                 y = Axis.2*5,
                 label = (nutrient)),
            size=5,fontface = "italic",
            colour="#5BB318",
            max.overlaps = 100)  
# arrows
ordination_nut <- ordination_nut + geom_segment(aes(x = 0, y = 0, 
                                                    xend = correlation_nut[1,1]*4, 
                                                    yend = correlation_nut[1,2]*4),size = 1,
                                                color="#2B7A0B",alpha=0.05,
                                                arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[2,1]*4, 
                   yend = correlation_nut[2,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[3,1]*4, 
                   yend = correlation_nut[3,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[4,1]*4, 
                   yend = correlation_nut[4,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[5,2]*4, 
                   yend = correlation_nut[5,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm"))) + 
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlation_nut[6,1]*4, 
                   yend = correlation_nut[6,2]*4),size = 1,
               color="#2B7A0B",alpha=0.05,
               arrow = arrow(length = unit(.35, "cm")))



# arrange
require(gridExtra)

pdf (here ("output", "nutrients.pdf"),height=5,width=9)

composition2<-grid.arrange(ordination_nut,
                           plot_nut,
                           ncol=5,nrow=5,
                           layout_matrix = rbind (c (1,1,1,1,1),
                                                  c (1,1,1,1,1),
                                                  c (1,1,1,1,1),
                                                  c (2,2,2,2,2),
                                                  c (2,2,2,2,2)))



dev.off()



# ======================

# 
#source (here ("R", "function_poncho.R"))
#
#pdf("year.pdf")
#poncho (log(year_composition+1),
#        gradient = seq(1,nrow(year_composition)),
#        col = as.numeric(as.factor(fisheries_wtrait$Diet_2012[match (colnames(year_composition),
#                                                fisheries_wtrait$Genus)])),lty = 0)
#
#dev.off()
#





