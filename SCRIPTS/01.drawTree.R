### BiocManager needed for installing ggtree
# install.packages("BiocManager")
# 
# BiocManager::install("ggtree")

require(ggtree)
require(ggplot2)
require(dplyr)
library(tidytree)
library(TreeTools)

labeledtreeplot <- function(phylotree, 
                            specieslist,
                            tr.dat,
                            label_ranks = c('clade', 'subsection', 'section')
                            ) {
  phylotree <- phylotree %>% 
    ape::keep.tip(specieslist)
  
  linethickness = c(clade = 1, subsection = 1.1, section = 1.2)
  cladelabelsize = c(clade = 2.5, 
                     subsection = 2.5, 
                     section = 3)
  
  offsetLabel = c(clade = 10, subsection = 15, section = 20)
  barExtend = 0.1 #-0.2 for fan?
  
  
  p <- ggtree(phylotree,
              layout = "rectangular", #'fan', 
              ladderize = F,
              open.angle = 180, # limits fan extent, otherwise does nothing
              size = 0.01
              )         +
    geom_tiplab(fontface='italic',
                size = 3)          +
    theme(legend.position='none')  +
    scale_color_manual(values=c("gray20", "black"))
  
  
  ## add clade labels across the grouped species
  for(i in label_ranks) {
    for(j in unique(filter(tr.dat, sp %in% specieslist)[[i]])) {
      if((j == "") | (is.na(j))) {
        message(paste("skipping, j = ", j))
        next
      }
      
      message(paste('doing i =', i, "; j =", j))
      
      
      # get the most recent common ancestor of a collection of species
      mrcaNode <-
        ape::getMRCA(phylotree,
                     tr.dat %>%
                       dplyr::filter(.data[[i]] == j) %>% 
                       pull(sp))
      message(paste("mrcaNode:", mrcaNode))
      p <- p +
        geom_cladelabel(
          node = mrcaNode,
          offset = offsetLabel[i],
          fontsize = cladelabelsize[i],
          barsize = linethickness[i],
          label = j,
          extend = barExtend
        ) # close geom_cladelabel
    }
  }
  p
}

