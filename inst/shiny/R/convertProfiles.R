#Functions for converting mixture and reference profiles to correct format for relMix()

#Get mixture in list format with one element per marker
convertMixture <- function(E){

  m <- length(unique(E$Marker)) #Number of markers
  n <- length(unique(E$SampleName)) #Number of samples (if replicates)
  #Split according to markers and then sample
  mix <- split(E,E$Marker)
  mix2 <- lapply(mix,function(x) split(x[,-c(1,2)],x$SampleName))
  #Remove NA's
  lapply(mix2,function(z) lapply(z, function(z2) z2[!is.na(z2)]))
}

#Reads reference profiles to list format with one element per marker.
#Each marker element is a list with one element per reference profile
convertReference <- function(GT){

  m <- length(unique(GT$Marker)) #Number of markers
  n <- length(unique(GT$SampleName)) #Number of contributors
  gt <- split(GT,GT$Marker) #Split according to markers
  lapply(gt,function(x) split(x[,3:4],x$SampleName)) #Split according to individual
}