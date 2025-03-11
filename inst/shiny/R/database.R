# Functions for making modifications to the database ----

#Add silent allele to database
silent_allele <- function(freqs,ps){

  freqsS <- rbind(freqs[,-1],rep(as.numeric(ps),ncol(freqs)-1))
  freqsS <- cbind(Allele=c(freqs[,1],'Silent'),freqsS)
  freqsS

  #Scale frequencies to sum to 1
}



#mix <- read.table(here::here("inst","extdata","testdata","Unobserved_allele","mixture_unobs.txt"), header = TRUE, sep = "\t")
#ref <- read.table(here::here("inst","extdata","testdata","Unobserved_allele","references_unobs.txt"), header = TRUE, sep = "\t")
#db <- read.table(here::here("inst","extdata","frequencies22Markers.txt"), header = TRUE, sep = "\t")

#Add alleles not in database with frequency MAF
#Checks if any alleles are below MAF (by running MAF_allele)
#Input: frequencies, mixture and reference profiles in format read in from file
#and a MAF value
unobserved_allele <- function(db,mix,ref,MAF,session){

  db <- tidyr::pivot_longer(db,!Allele, names_to="Marker", values_to = "Frequency", values_drop_na = TRUE)

  n <- ncol(mix)
  mIx <- alNotDB <- numeric()
  #Go through each marker in mixture profile to look for alleles not in database
  #Simultaneously checking in reference profile (assumes that markers in reference and
  #mixture are the same)
  for(i in 1:nrow(mix)){
    #Get all database frequencies for the marker

    m <- mix$Marker[i]

    #All alleles in database for given marker
    al <- dbLong$Allele[dbLong$Marker==m]
    #Alleles in mixture for given marker
    am <- mix[i,3:n][!is.na(mix[i,3:n])]
    #Alleles in reference files
    ag <- ref[ref$Marker==m,3:4]
    ag <- ag[!is.na(ag)]
    aa <- as.character(unique(c(am,ag)))
    #Check if all alleles in mixture and reference exist in database
    allele_exist <- c(aa%in%al)

    #Check if marker name is the same in mixture file and database, otherwise give error
    #(This has already been checked once the data was imported in)
    #if(length(ix)==0) f_errorWindow(paste("Marker",mark,"not found in database"))


    # if the allele is missing, schedule it to be added to the database,
    # unless it was scheduled already
    #if(any(!allele_exist) && !(aa[!allele_exist] %in% alNotDB)){
    if(any(!allele_exist)) {
      cat(i,"\n")
      mIx <- c(mIx,rep(m,sum(!allele_exist))) #Name of marker
      alNotDB <- c(alNotDB,aa[!allele_exist]) #Alleles not found in db
    }
  }

  if(length(alNotDB)>0) { #There are alleles not in database

    shinyalert::shinyalert(paste("Allele",alNotDB, "will be added to marker",mIx, "with frequency",MAF, "and the frequencies will be scaled"), type = "info", session=session)

    #Add new allele at the end of database
    db <- rbind(db,data.frame(Marker=mIx,Allele=alNotDB,Frequency=MAF))
  }

  db

}# end unobserved_allele

#Check if any allele frequencies are below MAF and sets frequency to MAF
#Scales frequencies if necessary (with MAF_allele)
#Function used by f_unobserved
MAF_allele <- function(db,MAF){

  #keep alleles with NA for all markers
  dbLong <- tidyr::pivot_longer(db,!Allele, names_to="Marker", values_to = "Frequency", values_drop_na = FALSE, cols_vary="slowest")
  dbLong <- dbLong[,c("Marker","Allele","Frequency")]

  dbLong$Frequency[!is.na(dbLong$Frequency) & dbLong$Frequency < MAF] <- MAF

  #scale frequencies

  #Convert back to wide format
  dbWide <- tidyr::pivot_wider(dbLong,names_from="Marker", values_from="Frequency")
  dbWide

}


# #Check that all frequencies sum to 1, otherwise scale
# #Sort database and assign final database to environment
# #Function used by f_MAF
# scale_allele <- function(db){
#
#   #db <- tidyr::pivot_longer(db,!Allele, names_to="Marker", values_to = "Frequency", values_drop_na = TRUE)
#
#   # Define a tolerance level
#   tolerance <- 1e-4
#
#   # Check if all elements are approximately equal to 1
#   ix1 <- which((colSums(db[,-1],na.rm=TRUE) - 1) > tolerance) + 1 #Scale
#   ix2 <- which((1 - colSums(db[,-1],na.rm=TRUE)) >  tolerance) + 1 #Scale or rest allele
#   if(length(ix1)>0) { #Scale
#     shinyalert::shinyalert(paste("The sum of the frequencies for", markerNames[ix1], "is larger than 1 and will be scaled."), type="info")
#     db[,ix1] <- apply(db[,ix1], 2, function(x) x/sum(x, na.rm=TRUE))
#   }
#     if(length(ix2)>0)  {
#     shinyalert::shinyalert(paste("The sum of the frequencies for", markerNames[ix2], "is less than 1. Do you want to scale? If not, a rest allele will be added.", type="input"))
#     db[,ix2] <- apply(db[,ix2], 2, function(x) x/sum(x, na.rm=TRUE))
#                            }
#
#
#
#
#
#    markerNames <- unique(db$Marker)
#
#   #Sort database according to marker, then allele. Silent allele last
#   #First reorder levels of Allele
#   aL <- unique(db$Allele)
#   if(any(aL == 'Silent')) {
#     db$Allele <-
#       factor(db$Allele, c(sort(aL[which(!aL == 'Silent')]), 'Silent'))
#   } else {
#     db$Allele <- factor(db$Allele, sort(aL))
#   }
#
#   db <- db[order(db$Marker,db$Allele),]
#
#   #Check that frequencies sum to 1, otherwise scale
#   sums <- sapply(1:length(markerNames),function(i) sum(db[db[,1]==markerNames[i],3]))
#   ix <- which(sums!=1)
#   if(length(ix)>0) {
#
#     #shinyalert::shinyalert("Frequencies do not sum to 1. Do you want to scale? If not, a rest allele will be added.", type="info")
#     #if(w){ #Scale
#       for(m in markerNames[ix]){
#         db[db[,1]==m,3] <- db[db[,1]==m,3]/sum(db[db[,1]==m,3])
#       }
#     # } else{ #Rest allele, or scale if frequencies sum > 1
#     #   for(i in ix){
#     #     if(sums[i]>1) { #Enforce scaling
#     #       db[db[,1]==markerNames[i],3] <- db[db[,1]==markerNames[i],3]/sum(db[db[,1]==markerNames[i],3])
#     #     } else { #Rest allele
#     #       db <- rbind(db,data.frame(Marker=markerNames[i],Allele='r',Frequency=1-sums[i]))
#     #     }
#     #   }
#
#
#     #}
#
#   }
#   db #Final database
# }
#
# #Put together all changes
#
#
