#Plot pedigrees
# Code by Magnus Dehli Vigeland
# Function for plotting a list of pedigrees with typed members specified for each
plotPeds = function(peds, typed) {
  npeds <- length(peds)

  # Make sure each pedigree is a list of components
  pedlist <- lapply(peds, function(p) if(pedtools::is.ped(p)) list(p) else p)

  # Component-wise plot data
  plotdat <- lapply(1:npeds, function(i) {
    pedi <- pedlist[[i]]
    ty <- typed[[i]]
    lapply(pedi, function(cmp) list(cmp, carrier = ty))
  })

  # Remove outer list layer
  plotdat <- unlist(plotdat, recursive = FALSE)

  # Group comps according to original pedigrees
  ncomps <- lengths(pedlist)
  groups <- split(seq_along(plotdat), rep(seq_along(ncomps), ncomps))

  # Titles
  titles <- paste0("H", 1:npeds)

  # Plot!
  pedtools::plotPedList(plotdat, frames = TRUE, groups = groups, titles = titles,
                        ### Further args to consider/tweak:
                        hatched = pedtools::typedMembers,
                        cex = 1.2,
                        cex.main = 1.2,
                        fmar = 0.02
  )
}