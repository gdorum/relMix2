#Convert mixture, reference and database to correct format and perform calculations
#with relMix()

#drop is a list of drop-in/dropout values per contributor
#Default: drop <- list("Mother"=0,"Child"=0,dropin=0.05)
calculateLR <- function(mix, ref, db, pedigrees, idxC1, idxC2, drop, theta=0, mutModel="Equal", femaleMutationRate=0,
                        maleMutationRate=0, MutationRange=0.5){

  #Convert mixture and reference profiles to list format
  R <- convertMixture(mix)
  knownGenos <- convertReference(ref)

  #Convert database to long format
  dbL <- wideToLongDatabase(db)
  #Scale
  dbL <- scale_allele(dbL)

  alleleNames <- as.character(dbL$Allele)

  #Check if there are non-numeric allele names and stepwise mutation model
  #If so, give a warning
  isNumeric <- suppressWarnings(as.numeric(alleleNames[alleleNames!="Silent"]))
  if(any(is.na(isNumeric)) && mutModel=='Stepwise'){
    stop("Stepwise mutation model requires numeric allele names.
                    Change allele names or choose a different mutation model.")
  }

  #Make lists of alleles and frequencies per marker
  allelesAll <- split(dbL$Allele,dbL$Marker)
  afreqAll <- split(dbL$Frequency,dbL$Marker)

  idxK <- unique(ref$SampleName) #Individuals with known genotypes
  idxC <- union(idxC1,idxC2)
  idxU <- idxC[!idxC%in%idxK] #Contributors with uknown genotypes. Assuming that all individuals are represented in both pedigrees!

  #Dropout/drop-in
  D <- drop[idxC]
  di <- drop[["dropin"]]


  ############# Computations ###########

  markers <- names(R)
  LRmarker <- numeric(length(markers))
  lik1 <- lik2 <- numeric(length(markers))
  for(i in 1:length(R)){


    #Create locus object for each marker
    alleles <- as.character(allelesAll[[which(names(allelesAll)==markers[i])]])
    afreq <- afreqAll[[which(names(afreqAll)==markers[i])]]
    #Set MutationRate2 to a very small number to avoid probability 0 of mutation to and from microinvariants
    #This is same as is done in Familias (MutationRate2 will be ignored unless mutation model is stepwise)
    locus <- Familias::FamiliasLocus(frequencies=afreq,name=markers[i],
                                     allelenames=alleles, MutationModel=mutModel, femaleMutationRate=femaleMutationRate,
                                     maleMutationRate=maleMutationRate, MutationRange=MutationRange, MutationRate2 = 1e-06)

    #If there is a silent allele we need to modify the mutation matrix.
    #Silent allele must be given as 's' (not 'silent' as in Familias)
    #That way Familias will treat it like a regular allele,
    #while relMix will treat is specially
    if('Silent'%in%alleles){
      newAlleles <- c(alleles[-length(alleles)],'s')
      mm <- locus$femaleMutationMatrix #Assuming same mutation matrix for male and female
      colnames(mm) <- rownames(mm) <- newAlleles
      locus <- Familias::FamiliasLocus(frequencies=afreq,name=markers[i],
                                       allelenames= newAlleles, MutationModel='Custom', MutationMatrix=mm)
    }

    names(pedigrees) <- c("H1","H2")

    if(identical(idxC1,idxC2)){
      #Find genotypes for known and unknown individuals involved in the hypothesis
      datamatrix <- createDatamatrix(locus,knownGenos[[which(names(knownGenos)==markers[i])]],idsU=idxU)
      res <- relMix(pedigrees, locus, R=R[[i]], datamatrix, ids=idxC, D=lapply(D[idxC],function(x) c(x,x^2)),di=di, kinship=theta)
      lik1[i] <- res$H1
      lik2[i] <- res$H2

    } else {
      #relMix must be run twice with different contributors specified
      #Find genotypes for known and unknown individuals involved in each hypothesis
      datamatrix1 <- createDatamatrix(locus,knownGenos[[which(names(knownGenos)==markers[i])]],idsU=intersect(idxU,idxC1))
      datamatrix2 <- createDatamatrix(locus,knownGenos[[which(names(knownGenos)==markers[i])]],idsU=intersect(idxU,idxC2))
      res1 <- relMix(pedigrees$H1, locus, R=R[[i]], datamatrix1, ids=idxC1, D=lapply(D[idxC1],function(x) c(x,x^2)),di=di, kinship=theta)
      res2 <- relMix(pedigrees$H2, locus, R=R[[i]], datamatrix2, ids=idxC2, D=lapply(D[idxC2],function(x) c(x,x^2)),di=di, kinship=theta)
      lik1[i] <- res1[[1]]
      lik2[i] <- res2[[1]]
    }
  }
  LRmarker <- lik1/lik2
  #Set NaN's to 0 (likelihood 0 also under H2)
  LRmarker[is.na(LRmarker)] <- 0
  LRmarker
}