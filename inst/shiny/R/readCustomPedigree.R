readCustomPedigree <- function(filename){

  #Read pedigree from ped file
  df <- tryCatch(
    read.table(filename, header=TRUE, sep="\t", stringsAsFactors=FALSE),
    error = function(e) { NULL });
  if(is.null(df)) message("Error importing pedigree file")
  #FamiliasPedigree requries NA instead of 0
  df$fid[df$fid==0] <- NA
  df$mid[df$mid==0] <- NA
  df$sex <- ifelse(df$sex==1,'male','female')
  ped <- Familias::FamiliasPedigree(id=df$id,dadid=df$fid,momid=df$mid,sex=df$sex)

  #allowedKinships <- ped$id;
  #Make names of individuals in pedigree with first letter uppercase
  #allowedKinships <- sapply(allowedKinships, titleize)
  #ped$id <- allowedKinships

  ped
}