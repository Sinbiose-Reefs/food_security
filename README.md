Food beyond land: seafood contribution to human nutrition in Brazil
================
Maria Luiza Gallina, ReefSynthesis Working Group
2023-04-27

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Repository containing the data and scripts used in the article “Food
beyond land: seafood contribution to human nutrition in Brazil”, to be
submitted to Current Biology.

<!-- badges: start -->
<!-- badges: end -->

#### This paper was produced using the following software and associated packages:

    ## R version 4.2.2 (2022-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] ape_5.6-2         vegan_2.6-4       lattice_0.20-45   permute_0.9-7    
    ##  [5] patchwork_1.1.2   ggbreak_0.1.1     cowplot_1.1.1     viridis_0.6.2    
    ##  [9] viridisLite_0.4.1 gridExtra_2.3     scatterpie_0.1.8  ggrepel_0.9.2    
    ## [13] forcats_0.5.2     stringr_1.5.0     purrr_1.0.1       readr_2.1.3      
    ## [17] tibble_3.1.8      tidyverse_1.3.2   sf_1.0-9          tidyr_1.3.0      
    ## [21] reshape2_1.4.4    ggplot2_3.4.0     reshape_0.8.9     openxlsx_4.2.5.1 
    ## [25] readxl_1.4.1      dplyr_1.1.0       here_1.0.1       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-160        fs_1.6.0            lubridate_1.9.1    
    ##  [4] httr_1.4.4          rprojroot_2.0.3     tools_4.2.2        
    ##  [7] backports_1.4.1     utf8_1.2.2          R6_2.5.1           
    ## [10] KernSmooth_2.23-20  mgcv_1.8-41         DBI_1.1.3          
    ## [13] colorspace_2.1-0    withr_2.5.0         tidyselect_1.2.0   
    ## [16] compiler_4.2.2      cli_3.6.0           rvest_1.0.3        
    ## [19] xml2_1.3.3          scales_1.2.1        classInt_0.4-8     
    ## [22] proxy_0.4-27        yulab.utils_0.0.6   digest_0.6.31      
    ## [25] rmarkdown_2.20      pkgconfig_2.0.3     htmltools_0.5.4    
    ## [28] dbplyr_2.3.0        fastmap_1.1.0       rlang_1.0.6        
    ## [31] rstudioapi_0.14     gridGraphics_0.5-1  generics_0.1.3     
    ## [34] farver_2.1.1        jsonlite_1.8.4      zip_2.2.2          
    ## [37] googlesheets4_1.0.1 magrittr_2.0.3      ggplotify_0.1.0    
    ## [40] Matrix_1.5-1        Rcpp_1.0.10         munsell_0.5.0      
    ## [43] fansi_1.0.4         lifecycle_1.0.3     stringi_1.7.12     
    ## [46] yaml_2.3.7          MASS_7.3-58.1       plyr_1.8.8         
    ## [49] grid_4.2.2          parallel_4.2.2      crayon_1.5.2       
    ## [52] splines_4.2.2       haven_2.5.1         hms_1.1.2          
    ## [55] knitr_1.42          pillar_1.8.1        reprex_2.0.2       
    ## [58] glue_1.6.2          evaluate_0.20       ggfun_0.0.9        
    ## [61] modelr_0.1.10       vctrs_0.5.2         tzdb_0.3.0         
    ## [64] tweenr_2.0.2        cellranger_1.1.0    gtable_0.3.1       
    ## [67] polyclip_1.10-4     assertthat_0.2.1    xfun_0.36          
    ## [70] ggforce_0.4.1       broom_1.0.3         e1071_1.7-12       
    ## [73] class_7.3-20        googledrive_2.0.0   gargle_1.2.1       
    ## [76] aplot_0.1.9         cluster_2.1.4       units_0.8-1        
    ## [79] timechange_0.2.0    ellipsis_0.3.2
