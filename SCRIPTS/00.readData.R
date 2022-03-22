### code derived from https://github.com/andrew-hipp/2021-oak-classification
### which has provided me with an excellent learning experience for Quercus
### and for presenting phylograms
###
### modified to select Quercus species native to southeast US
## 1. get the tree that represents all of the genus Quercus
## 2. prune the tree to leave just those species identified as native to the southeast US
## 3. normalize species labels (to be used as joining keys in the next script)
## 4. capture work to presentation files

##   important output: the list tr
##   tr defines the phylogram of the target oak species
##   the attribute tr$tip.label contains normalized species name for each leaf on the tree

library(ape) # for reading and manipulating the tree structure
library(stringr)
library(magrittr)
library(TreeTools)
require(ggtree)   ### wmc
require(ggplot2)
require(dplyr)
library(tidytree)

# get the tree that represents all of Quercus
# This is the critical data structure for the process.  The data structure 
# is as-is from Hipp's repository.  The information contained represents
# significant and exciting work by the collaboration related to Hipp's
# repository output.  Rendering the phylogram as a plot is icing on a 
# very large cake (I think the cake is made with acorn flour, BTW).
tr <- ape::read.tree('DATA/tr.singletons.correlated.1.taxaGrepStem.tre')

# one time derive keys for tree pruning, keeping those for southeast Quercus
# tr.label <- as.data.frame(tr$tip.label) 


# Prune the tree by keeping parts that contain those identified as southeast US oaks
# The values in seoaks.csv were written from tr$tip.label to a cav file,
# which was then edited by hand to remove unwanted species.  This preserved the original
# values for the proper functioning of ape::keep.tip
seoaks <- read.csv('DATA/seoaks.csv', header = T)
tr <- tr %>% 
  ape::keep.tip(seoaks$tip.key)


# extract the species name for the label
tr$tip.label %>% 
  stringr::str_extract("[^|]+") %>%
  stringr::str_replace_all("_", " ") -> tr$tip.label


tip.dat <- read.csv('DATA/tips.data.csv', 
                    as.is = TRUE)
tip.dat$sp <- tip.dat$sp %>% 
              gsub('_', ' ', ., fixed = T)
row.names(tip.dat) <- tip.dat$sp  #  hmm about the row names
tip.dat <- tip.dat[tr$tip.label, ]
tip.dat$node <- tidytree::nodeid(tibble::as_tibble(tr), 
                                 row.names(tip.dat))

seoaks <- dplyr::left_join(seoaks, 
                           select(tip.dat, subgenus, section, subsection, clade, sp), 
                           by = "sp")

whiteoakspecies <- (filter(seoaks, section %in% c("Virentes", "Quercus")) %>%
  select(sp))[[1]]

redoakspecies <- (filter(seoaks, section %in% c("Lobatae")) %>%
  select(sp))[[1]]

woplot <- labeledtree(whiteoakspecies)


### add the common name to the tip label species name
### the common name comes from seoaks
### may cause downstream issues   
# tr$tip.label <- unlist(tr$tip.label %>% 
#                          lapply( function(species) {
#                                    paste(species, 
#                                          (filter(seoaks, sp == species) %>% select(cn)), 
#                                          sep = ", ")
#                                  }
#                                )
#                       )


# print stuff to files
pdf('OUT/verticalclades.pdf', 8.5,15)
plot(tr, cex = 0.7)
dev.off()
