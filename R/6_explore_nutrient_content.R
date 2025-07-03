
# -------------------------------------------------------------------

#           Explore nutrient content of food items (nutrient data bases)
#                   Create Fig. 3

# --------------------------------------------------------------------

rm(list=ls())

# load packages
source("R/packages.R")

# load data
load ( here ("processed_data", "fisheries_wtrait.RData"))
TBCA_nutrients_raw<- readRDS (file = here ("processed_data", "TBCA_nutrients_raw.rda"))

# function to transform g into kg
trans_qtd <- function (x) {(x/1000)} #  and then kg / kg

# most trapped fish in a single state
fisheries_wtrait %>%
  filter (OtherArea == "Pará") %>%
  filter (Year %in% seq (2000,2015,1)) %>% # choose a year
  group_by (TaxonName) %>%
  summarise(catch_spp = sum (CatchAmount_t)) %>%
  arrange(desc(catch_spp)) 

# histogram of nutrient availability per state (not used in the paper)
p_state_consumption <- fisheries_wtrait %>% 
  filter (Year %in% seq (2000,2015,1)) %>% # choose a year
  mutate (across (ends_with("mu"),list(kg = trans_qtd)),
          CatchAmount_kg = CatchAmount_t*1000) %>% # catch into kg
  mutate_each(funs(.*CatchAmount_kg), ends_with("kg")) %>% 
  select(starts_with("Other") | ends_with("kg") | starts_with("Region")) %>%
  mutate(OtherArea = fct_reorder(OtherArea, CatchAmount_kg)) %>%
  select (!c("CatchAmount_kg", "Protein_mu_kg")) %>%
  
  gather ("nutrient", "value", -OtherArea,-Region) %>%
  ggplot (aes (x =  (OtherArea), 
               y=log(value)))+
  geom_boxplot(aes (fill=Region)) + 
  theme_bw()+
  facet_wrap(~nutrient,
             scales = "fixed",ncol=3) + 
  theme(axis.text.x = element_text(angle=90)) + 
  ylab ("Nutrient landings (log kg / year)") + 
  xlab ("State") 

# save
ggsave(p_state_consumption, file=here ('output',"state_consumption.pdf"), 
       width=10, height=8,bg="white")


# -----------------------------------------------------------------------

# explore nutrition content of fish genus
# find the 20 most catched spp in BR
nsp_choose <- 20

# summarize data (amounts per taxon)
most_catched_BR <- tapply (fisheries_wtrait$CatchAmount_t,
                           list (fisheries_wtrait$TaxonName
                           ),
                           sum)
names(most_catched_BR[order(most_catched_BR,decreasing=T)][1:nsp_choose]) # select


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
names(most_catched_list) <- c("Northeast","North", "Southeast", "South")

# table of names

tab_names <- lapply (most_catched_list, function (i) 
  
  data.frame (Catch_amount =round (i,2),
              Species_taxon= names(i),
              Abbreviation = paste (substr (sapply (strsplit (names(i), " "), "[[", 1),1,3),
                                    substr (sapply (strsplit (names(i), " "), "[[", 2),1,3),
                                    sep=" ")
  )
)

# melt
tab_names<-do.call(rbind, tab_names)
tab_names<-cbind (tab_names,
                  fisheries_wtrait [match (tab_names$Species_taxon,
                                           fisheries_wtrait$TaxonName),c("class","family")])


# save
# write.xlsx (tab_names, file = here ("output", "tab_names_catch.xlsx"), 
#             asTable = F, rowNames =T)



# Organize data to produce FIg. 3

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


# Creating Fig. 3 (taxa nutrient content) (Upper part A)
# plot A (the most catched spp)
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


# Creating Fig. 3 (taxa nutrient content) (Upper part B)
# COntent at higher taxonomic levels
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


# Create the lower part (Fig. 3B)
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



# Arrange plots to create Fig. 3
pdf(here ("output", "Nutrients_supply_consumed.pdf"),width=10,height=8)
  
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



