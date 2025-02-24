# Functions for making modifications to the database ----

# mixEx <- read.table(here::here("inst","extdata","mixture.txt"), header = TRUE, sep = "\t")
# refEx <- read.table(here::here("inst","extdata","references.txt"), header = TRUE, sep = "\t")
# databaseEx <- read.table(here::here("inst","extdata","frequencies22Markers.txt"), header = TRUE, sep = "\t")
#
# rownames(freqs) <- freqs[,1]
# freqs <- freqs[,-1]
# dbS <- silent_allele(freqs,0.1)
# dbS <- unobserved_allele(dbS,mixEx,refEx,0.01)



#Function that converts database from wide to long format
wideToLongDatabase <- function(freqs){

  alleleNames <- freqs$Allele
  rownames(freqs) <- alleleNames
  freqs <- freqs[,-1]
  markerNames <- colnames(freqs)

  db <- numeric()
  for(i in 1:ncol(freqs)){
    ix <- which(!is.na(freqs[,i]))
    a <- alleleNames[ix]
    f <- freqs[ix,i]
    Marker <- rep(markerNames[i],length(ix))
    dbNew <- data.frame(Marker,a,f)
    colnames(dbNew) <- c("Marker","Allele","Frequency")
    db <- rbind(db,dbNew)
  }
  db
}



#Add silent allele to database
silent_allele <- function(freqs,ps){

  if(ps>0) {
    freqsS <- rbind(freqs,rep(as.numeric(ps),ncol(freqs)))
    rownames(freqsS) <- c(rownames(freqs),'Silent')
  } else freqsS <- freqs
  freqsS
}


#unobserved <- function(db,
#    shinyalert::shinyalert(paste("Allele",alNotDB, "will be added to marker",markerNames[keepIx], "with frequency",MAF), type = "info")

#Add alleles not in database with frequency MAF
#Checks if any alleles are below MAF (by running MAF_allele)
#Input: frequencies, mixture and reference profiles in format read in from file
#and a MAF value
unobserved_allele <- function(freqs,M,G,MAF){

  alleleNames <- rownames(freqs)
  markerNames <- colnames(freqs)

  n <- ncol(M)
  keepIx <- alNotDB <- numeric()
  #Go through each marker in mixture profile to look for alleles not in database
  #Simultaneously checking in reference profile (assumes that markers in reference and
  #mixture are the same)
  for(i in 1:nrow(M)){
    #Get all database frequencies for the marker
    mark <- M[i,2]
    ix <- which(mark==markerNames)
    #Check if marker name is the same in mixture file and database, otherwise give error
    #(This has already been checked once the data was imported in)
    #if(length(ix)==0) f_errorWindow(paste("Marker",mark,"not found in database"))
    al <- alleleNames[!is.na(freqs[,ix])] #All alleles in database for given marker
    #Alleles in mixture
    am <- M[i,3:n][!is.na(M[i,3:n])]
    #Alleles in genotypes
    ag <- unlist(G[G[,2]==mark,3:4])
    aa <- as.character(unique(c(am,ag)))
    #Check if all alleles in mixture and reference exist in database
    idx <- c(aa%in%al)

    # if the allele is missing, schedule it to be added to the database,
    # unless it was scheduled already
    if(any(!idx) && !(aa[!idx] %in% alNotDB)){
      cat(i,"\n")
      keepIx <- c(keepIx,rep(ix,sum(!idx))) #Index of marker
      alNotDB <- c(alNotDB,aa[!idx]) #Alleles not found in db
    }
  }

  #Change format of database before adding new alleles
  db <- wideToLongDatabase(freqs)


  if(length(alNotDB)>0) { #There are alleles not in database

    shinyalert::shinyalert(paste("Allele",alNotDB, "will be added to marker",markerNames[keepIx], "with frequency",MAF), type = "info")

    #Add new allele at the end of database
    newData <- data.frame(markerNames[keepIx],alNotDB,MAF)
    colnames(newData) <- c('Marker','Allele','Frequency')
    db <- data.frame(Marker=c(as.character(db$Marker),as.character(newData$Marker)),
                     Allele=c(as.character(db$Allele),as.character(newData$Allele)),
                     Frequency=c(db$Frequency,newData$Frequency))
    #Check for MAF, scale and sort
    db <- MAF_allele(db,MAF)
    # }) #end handler add alleles

  }else{ #No alleles added to database
    #Check for MAF, scale and sort
    db <- MAF_allele(db,MAF)
    db
  }
}# end unobserved_allele

#Check if any allele frequencies are below MAF and sets frequency to MAF
#Scales frequencies if necessary (with MAF_allele)
#Function used by f_unobserved
MAF_allele <- function(db,MAF){

  if(any(db$Frequency<MAF)){ #Frequencies below MAF
    shinyalert::shinyalert("Some frequencies are below the min. allele frequency.
                Change the indicated frequencies?", title="info")
    if(w) {
      db$Frequency[db$Frequency<MAF] <- MAF
    }
  }
  #Check if scaling is necessary
  db <- scale_allele(db)
  db
}


#Check that all frequencies sum to 1, otherwise scale
#Sort database and assign final database to environment
#Function used by f_MAF
scale_allele <- function(db){

  markerNames <- unique(db[,1])

  #Sort database according to marker, then allele. Silent allele last
  #First reorder levels of Allele
  aL <- unique(db[,2])
  if(any(aL == 'Silent')) {
    db$Allele <-
      factor(db$Allele, c(sort(aL[which(!aL == 'Silent')]), 'Silent'))
  } else {
    db$Allele <- factor(db$Allele, sort(aL))
  }

  db <- db[order(db$Marker,db$Allele),]

  #Check that frequencies sum to 1, otherwise scale
  sums <- sapply(1:length(markerNames),function(i) sum(db[db[,1]==markerNames[i],3]))
  ix <- which(sums!=1)
  if(length(ix)>0) {

    #shinyalert::shinyalert("Frequencies do not sum to 1. Do you want to scale? If not, a rest allele will be added.", type="info")
    #if(w){ #Scale
      for(m in markerNames[ix]){
        db[db[,1]==m,3] <- db[db[,1]==m,3]/sum(db[db[,1]==m,3])
      }
    # } else{ #Rest allele, or scale if frequencies sum > 1
    #   for(i in ix){
    #     if(sums[i]>1) { #Enforce scaling
    #       db[db[,1]==markerNames[i],3] <- db[db[,1]==markerNames[i],3]/sum(db[db[,1]==markerNames[i],3])
    #     } else { #Rest allele
    #       db <- rbind(db,data.frame(Marker=markerNames[i],Allele='r',Frequency=1-sums[i]))
    #     }
    #   }


    #}

  }
  db #Final database
}


