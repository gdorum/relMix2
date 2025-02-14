# Reference table: Convert tall -> wide + prettify for display in app
prettyRef = function(x) {
  print(x)
  if(is.null(x)) return(x)
  x$Geno = paste(x$Allele1, x$Allele2, sep = "/")
  x$Allele1 = x$Allele2 = NULL

  proflist = split(x, x$SampleName)
  m = proflist[[1]]$Marker

  res = do.call(cbind, lapply(proflist, function(p) p$Geno))
  res = data.frame(Marker = proflist[[1]]$Marker, res)
  res
}
