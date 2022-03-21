# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")

require(ggtree)   ### wmc
require(ggplot2)
require(dplyr)
library(tidytree)
library(TreeTools)



### wmc -- for now, plot and pick the numbers to get desired 
### subtree.  For later, figure out how to get node numbers
### at a specific depth
#plot(tr, cex = 0.5)
#ape::nodelabels()

troubleshoot = F


### wmc -- gray50 to gray20 to make the content a bit more readable without further processing
lwdLabel = c(clade = 0.3, subsection = 1, section = 1)
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

offsetLabel = c(clade = 15, subsection = 20, section = 25)
offsetTemp <- min(offsetLabel)
barExtend = -0.2

## make base tree
tr.plot <- tidytree::left_join(whiteoaktree, tip.dat, by = 'node')

p <- ggtree(whiteoaktree,   #tr.plot, 
            #layout = 'fan', 
            ladderize = F,
            #open.angle = 180, 
            size = 0.01) +
  geom_tiplab(fontface='italic',
                      size = 3
                    ) +
  theme(legend.position='none') +
  scale_color_manual(values=c("gray20", "black"))


## add clade labels
for(i in c('clade', 'subsection')) {
  for(j in unique(unique(filter(seoaks, sp %in% whiteoakspecies$sp)[[i]]))) {
    if((j == "") | (is.na(j))) next
    
    message(paste('doing i = ', i, "; j =", j))
    
    mrcaNode <- ape::getMRCA(whiteoaktree, row.names(tip.dat)[which(tip.dat[[i]] == j)])
    
    p <- p +
      geom_cladelabel(
        node = mrcaNode,
        offset = offsetTemp,
        fontsize = cexLabel[i],
        barsize = lwdLabel[i],
        label = j,
        color = colLab[j],
        extend = barExtend
      ) # close geom_cladelabel
  }
}


pdf('OUT/whiteoakstree.pdf', 12, 8)
  print(p)
dev.off()

