# first letter in a string to upper case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# create the function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# poison smooth 
poison_smooth <- function(...) {
  geom_smooth(method = "gam", 
              method.args = list(family = "negbin(1)"),...)
  # geom_smooth(method = "glm", method.args = list(family = "poisson"), ...) # glm option
}


# FUnction
# point within polygon
st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}


# tukey test
# from here: https://rpubs.com/Edbbio/885399

GGTukey.2<-function(Tukey){
  A<-require("tidyverse")
  if(A==TRUE){
    library(tidyverse)
  } else {
    install.packages("tidyverse")
    library(tidyverse)
  }
  B<-as.data.frame(Tukey[1])
  colnames(B)[1:4]<-c("diff",
                      "min",
                      "max",
                      "p")
  C<-data.frame(id=row.names(B),
                diff = B$diff,
                min=B$min,
                max=B$max,
                idt=ifelse(B$p<0.05,
                           "significant",
                           "not significant")
  )
  
  D<-C%>%
    #filter (idt == "not significant") %>%
    ggplot(aes(id,color=idt))+
    geom_errorbar(aes(ymin=min,
                      ymax=max),
                  width = 0.5,
                  size=1.25)+
    geom_point (aes(y=diff,x=id),size=2)+
    
    labs(x="Comparison",
         y="Difference",
         color=NULL)+
    scale_color_manual(values=c("red",
                                     "green")
    )+
    coord_flip()+
    theme(title=element_text(color="black",size=15),
          axis.text = element_text(color="black",size=10),
          axis.title = element_text(color="black",size=10),
          panel.grid=element_line(color="grey75"),
          axis.line=element_blank(),
          plot.background=element_rect(fill="white",color="white"),
          panel.background=element_rect(fill="white"),
          panel.border = element_rect(colour = "black", fill = NA,size=0.59),
          legend.key= element_rect(color="white",fill="white")
    )
  return(D)
}
