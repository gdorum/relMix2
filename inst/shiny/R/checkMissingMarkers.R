#Function that checks that all markers in mixture and reference can also be found in database
checkMissingMarkers <- function(mix, ref, db){

    marker1 <- mix$Marker
    marker2 <- ref$Marker
    marker3 <- colnames(db)[-1]

    missing_in_1 <- setdiff(marker1, union(marker2,marker3))
    missing_in_2 <- setdiff(marker2, union(marker1,marker3))
    missing_in_3 <- setdiff(marker3, union(marker1,marker2))

    data.frame(
      Marker = c(missing_in_2, missing_in_3),
      MissingIn = c(rep("Database", length(missing_in_1)),rep("Mixture profile", length(missing_in_2)), rep("Reference profile", length(missing_in_3)))
    )
}

