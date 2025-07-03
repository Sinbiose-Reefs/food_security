Food beyond land: seafood contribution to human nutrition in Brazil
================
Maria Luiza Gallina et al., ReefSynthesis Working Group
2025-07-03

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Repository containing the data and scripts used in the article “Food
beyond land: seafood contribution to human nutrition in Brazil”, being
reviewed in Nature Food.

<!-- badges: start -->
<!-- badges: end -->

## The project is organized as follows:

## **Root**

\|— **data_fisheries_nutrients**: Raw nutritional and trait data.  
\|———–
“Atributos_especies_Atlantico\_&\_Pacifico_Oriental_2020_04_28.csv”:
Reef fish traits (Quimbayo et al. 2021, DOI: doi/10.1002/ecy.3298).  
\|———–
“FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx”:
Time series of fisheries catches (reconstructions) (Freire et al. 2021,
from <http://www.propesq.pesca.sp.gov.br/37/conteudo>).  
\|———– “Nutrients_TBCA.xlsx”: Table of nutritional content (Brazilian
Table), from <http://www.tbca.net.br/>.  
\|———– “Species_Nutrient_Predictions.csv”: data from Hicks et
al. (2019), available at <https://github.com/mamacneil/NutrientFishbase>
and
<https://fishbase.mnhn.fr/Nutrients/manual/FishBaseNutrients_2021_06.pdf>
.  
\|———– Nutrients_imputation.xlsx: List of taxa with nutritional data in
some data base that were used to calculate the nutritional content of
food items with missing data in RFB. For example, average nutrient
values for *Penaeus*, *Penaeus esculentus*, *Penaeus latisulcatus*,
*Penaeus merguiensis*, *Penaeus monodon*, *Penaeus semisulcatus* were
used to input nutritional data for *Penaeus brasiliensis*.  
\|———– uFiSh1.0.xlsx: uFish nutritional data. Obtained from
<https://www.fao.org/infoods/infoods/tables-and-databases/faoinfoods-databases/en/>.  
\|———– “Threshold_FAO.xlsx”: FAO’s thresholds of nutrient consumption  
  
\|— **POF_government**: Raw data (POF, or RFB in the article) from
Brazilian government. The data we used is in the folder “Dados_20210304”
(not visible on GitHub as it has 1.14GB size). The folder was created
from data found at <a
href="https://www.ibge.gov.br/estatisticas/sociais/saude/24786-pesquisa-de-orcamentos-familiares-2.html?=&amp;t=microdados.\"
class="uri">https://www.ibge.gov.br/estatisticas/sociais/saude/24786-pesquisa-de-orcamentos-familiares-2.html?=&amp;t=microdados.\</a>
\|———– “Omega3_QTD.csv”: Content of Omega-3 in food. Values were
gathered from FAO and FishBase. Note that values were only recorded for
fish (red meat sources have Omega-3 equal to 0).  
\|———– “POP2017_20220905.xls”: Brazilian population in 2017, per
municipality. Data from IBGE.  
\|———– “POP2018_20220905.xls”: Brazilian population in 2017, per
municipality. Data from IBGE.  
\|———– “Cadastro de Produtos do Consumo Alimentar.xlsx”: List of food
items in POF/RFB data. Definition of animal protein items and seafood
items was done based on this table. Data from POF database.  
\|———– “Pib_BR.xlsx”: Gross domestic product of Brazilian states. Data
from IBGE.  
  
\|— **processed_data**: Data sets obtained after processing the raw data
sets.  
\|———– “POF_processed_data.RData”: processed data using POF dataset
hosted in the folder “POF_government”. This resulted from running the R
script “0_extract_POF_data.R”  
\|———– “consumption_nutrients.RData”: processed data concerning the
consumption of nutrients from seafood”. This resulted from running the R
script “2_nutrient_demand.R”. This data set is used to define seafood
demand.  
\|———– “consumption_nutrients_Other.RData”: processed data concerning
the consumption of nutrients from other sources than seafood”. This
resulted from running the R script “2_nutrient_demand.R”  
\|———– “fishConsumption_Income_all_food.RData”: processed data
concerning the consumption of nutrients from all food sources, to
explore overall consumption”. This resulted from running the R script
“1_pof_nutrients_NUM_UC.R”  
\|———– “fishConsumption_Income_meat.RData”: processed data concerning
the consumption of nutrients from animal protein items, to explore
overall consumption of animal protein”. This resulted from running the R
script “1_pof_nutrients_NUM_UC.R”  
\|———– “table_supply_state.RData”: processed data concerning the catch
amount per state, after matching fisheries and nutritional data. This
resulted from running the R script “5_generate_nutrient_supply.R”  
\|———– “fisheries_wtrait.xlsx”: Nutritional data per taxon. Obtained
after matching fisheries, trait and nutritional data at the different
taxonomic levels of fisheries data. This resulted from running the R
script “5_generate_nutrient_supply.R”  
\|———– “state_interviewees.xlsx”: Number of interviewees per state. This
resulted from running the R script “1_pof_nutrients_NUM_UC.R”  
\|———– “worms_fish.RData”: Taxonomic classification of fisheries and
nutritional data (validation using WORMS data - worrms R package). This
resulted from running the R script “5_generate_nutrient_supply.R”  
  
  
\|— **output**  
\|———– figures (pdf) and RData  
  
\|— **R**  
\|———– several R scripts (numbered according to the processing order -
to reproduce results run in that order, from “0_extract_POF_data.R” to
“8_supp_map_consumption_allstates.R”.)  

#### This paper was produced using the following software and associated packages:

    ## R version 4.4.1 (2024-06-14 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: Europe/Paris
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] ape_5.8           vegan_2.6-6.1     lattice_0.22-6    permute_0.9-7    
    ##  [5] patchwork_1.3.0   ggbreak_0.1.2     cowplot_1.1.3     viridis_0.6.5    
    ##  [9] viridisLite_0.4.2 gridExtra_2.3     scatterpie_0.2.4  ggrepel_0.9.6    
    ## [13] lubridate_1.9.3   forcats_1.0.0     stringr_1.5.1     purrr_1.0.2      
    ## [17] readr_2.1.5       tibble_3.2.1      tidyverse_2.0.0   sf_1.0-18        
    ## [21] tidyr_1.3.1       reshape2_1.4.4    ggplot2_3.5.1     reshape_0.8.9    
    ## [25] openxlsx_4.2.7.1  readxl_1.4.3      dplyr_1.1.4       here_1.0.1       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1   farver_2.1.2       fastmap_1.2.0      tweenr_2.0.3      
    ##  [5] digest_0.6.35      timechange_0.3.0   lifecycle_1.0.4    cluster_2.1.6     
    ##  [9] magrittr_2.0.3     compiler_4.4.1     rlang_1.1.3        tools_4.4.1       
    ## [13] utf8_1.2.4         yaml_2.3.8         knitr_1.48         classInt_0.4-10   
    ## [17] plyr_1.8.9         aplot_0.2.3        KernSmooth_2.23-24 withr_3.0.0       
    ## [21] grid_4.4.1         polyclip_1.10-7    fansi_1.0.6        e1071_1.7-16      
    ## [25] colorspace_2.1-1   scales_1.3.0       MASS_7.3-60.2      cli_3.6.2         
    ## [29] rmarkdown_2.28     generics_0.1.3     rstudioapi_0.16.0  tzdb_0.4.0        
    ## [33] DBI_1.2.3          ggforce_0.4.2      proxy_0.4-27       splines_4.4.1     
    ## [37] parallel_4.4.1     ggplotify_0.1.2    cellranger_1.1.0   vctrs_0.6.5       
    ## [41] yulab.utils_0.1.8  Matrix_1.7-0       gridGraphics_0.5-1 hms_1.1.3         
    ## [45] units_0.8-5        glue_1.7.0         stringi_1.8.4      gtable_0.3.5      
    ## [49] munsell_0.5.1      pillar_1.9.0       htmltools_0.5.8.1  R6_2.5.1          
    ## [53] rprojroot_2.0.4    evaluate_1.0.1     ggfun_0.1.6        class_7.3-22      
    ## [57] Rcpp_1.0.13        zip_2.3.1          nlme_3.1-164       mgcv_1.9-1        
    ## [61] xfun_0.51          fs_1.6.4           pkgconfig_2.0.3
