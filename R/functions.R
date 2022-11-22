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
