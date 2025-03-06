createPedigree <- function(pedigree){
  if(pedigree=="Paternity"){
    persons <- c("Mother", "Father", "Child")
    sex <- c("female", "male", "male")
    ped <- Familias::FamiliasPedigree(id=persons, dadid=c(NA,NA,"Father"), momid=c(NA,NA,"Mother"), sex=c("female", "male", "male"))
    #} else if(pedigree=="Non-paternity"){
  } else if(pedigree=="Non-paternity") {
    #Define the persons involved in the case
    persons <- c("Mother", "Father", "Child")
    sex <- c("female", "male", "male")
    ped <- Familias::FamiliasPedigree(id=persons, dadid=c(NA,NA,NA), momid=c(NA,NA,"Mother"), sex=c("female", "male", "male"))
  }
  ped
}