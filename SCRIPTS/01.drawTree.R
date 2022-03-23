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
                            cladelevels = c('clade', 'subsection', 'section')
                            ) {
  phylotree <- phylotree %>% 
    ape::keep.tip(specieslist)
  
  lwdLabel = c(clade = 1, subsection = 1, section = 1)
  cexLabel = c(clade = 2, 
               subsection = 2, 
               section = 3)
  colLab <- c(Lobatae = 'red3', 
              Quercus = 'black',
              Protobalanus = 'gray20', 
              Ponticae = 'gray20', 
              Virentes = 'gray20',
              
              ## now red oak subsections
              Agrifoliae = 'red',
              Palustres = 'red',
              Phellos = 'red',
              Coccineae = 'red',
              
              ## ... and white oak subsections
              Dumosae = 'gray20',
              Albae = 'gray20',
              Prinoideae = 'gray20',
              Stellatae = 'gray20',
              Polymorphae = 'gray20',
              
              ### ... and informal clades
              'TX red oaks' = 'red',
              'Erythromexicana [in part]' = 'red',
              'Roburoids [in part]' = 'gray20',
              'Leucomexicana [in part]' = 'gray20'
              )
  
  offsetLabel = c(clade = 10, subsection = 15, section = 20)
  offsetTemp <- min(offsetLabel)
  barExtend = -0.2
  
  
  p <- ggtree(phylotree,
              #layout = 'fan', 
              ladderize = F,
              #open.angle = 180, 
              size = 0.01)         +
    geom_tiplab(fontface='italic',
                size = 3)          +
    theme(legend.position='none')  +
    scale_color_manual(values=c("gray20", "black"))
  
  
  ## add clade labels across the grouped species
  for(i in cladelevels) {
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
      
      p <- p +
        geom_cladelabel(
          node = mrcaNode,
          offset = offsetLabel[i],
          fontsize = cexLabel[i],
          barsize = lwdLabel[i],
          label = j,
          color = colLab[j],
          extend = barExtend
        ) # close geom_cladelabel
    }
  }
  p
}

