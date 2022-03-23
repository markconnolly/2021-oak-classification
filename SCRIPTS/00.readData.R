### code derived from https://github.com/andrew-hipp/2021-oak-classification
### which has provided me with an excellent learning experience for Quercus
### and for presenting phylograms

library(ape) # for reading and manipulating the tree structure
library(stringr)
library(magrittr)
library(TreeTools)
require(ggtree)   ### wmc
require(ggplot2)
require(dplyr)
library(tidytree)

##  Sources for data frame

###   The phylogenetic tree that contains oak species.
###   The data structure taken directly from Hipp's repository.
tr <- ape::read.tree('DATA/tr.singletons.correlated.1.taxaGrepStem.tre')
tr$tip.label %>%
  stringr::str_extract("[^|]+") %>%      
  stringr::str_replace_all("_", " ") -> 
  tr$tip.label


###   Data about the individual oak species.
###   Includes the taxonomy membership of the species below genus down to species:
###   subgenus, section, subsection, clade.  This information is used to derive
###   labels in a plot of a phylogenic tree.
###   The data structure taken directly from Hipp's repository.
tip.dat <- read.csv('DATA/tips.data.csv', as.is = TRUE)
tip.dat$sp <- stringr::str_replace_all(tip.dat$sp, "_", " ")


###   The list of oaks I care about.  In this case, oaks native to the 
###   southeastern US.  Contains the list of species and their common names.
###   This list was constructed by hand.
seoaks.dat <- read.csv('DATA/seoaks.csv')
seoaks.dat <- dplyr::left_join(seoaks.dat, 
                           tip.dat %>% 
                             dplyr::select(subgenus, section, subsection, clade, sp), 
                           by = "sp")

whiteoakspecies <- seoaks.dat %>% 
  dplyr::filter(section %in% c("Virentes", "Quercus")) %>%
  select(sp) %>% 
  pull(sp)

redoakspecies <- seoaks.dat %>% 
  dplyr::filter(section == "Lobatae") %>%
  select(sp) %>% 
  pull(sp)


woplot <- labeledtreeplot(phylotree = tr, 
                          specieslist = whiteoakspecies,
                          tr.dat = seoaks.dat)

woplot



# # print stuff to files
# pdf('OUT/verticalclades.pdf', 8.5,15)
# plot(tr, cex = 0.7)
# dev.off()
