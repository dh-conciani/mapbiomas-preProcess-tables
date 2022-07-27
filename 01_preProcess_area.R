## pre-process mapbioamas tables (area) to be used in dashboard
## from collection 3.1 to 7
## dhemerson.costa@ipam.org.br

## avoid scientific notation
options(scipen=999)

## import libraries
library(readxl)
library(tools)
library(reshape2)
library(ggplot2)
library(xlsx)

## set root
root <- './table/' 

## list files
files <- list.files(root)

## create recipe to receive data
recipe <- as.data.frame(NULL)

for (i in 1:length(files)) {
  ## read collection 
  x <- read_excel(paste0(root, files[i]))
  
  ## insert collection name
  x$collection <- gsub('collection_', 'Collection ', 
                       file_path_sans_ext(files[i]))
  
  ## melt data
  y <- melt(x, id.vars=c('biome', 'state', 
                         'level_0', 'level_1', 'level_2', 'level_3', 'level_4', 'collection'))
  
  ## rename melted columns
  names(y)[names(y) == 'variable'] <- 'year'
  names(y)[names(y) == 'value'] <- 'area'
  
  ## replace NA by zero
  y[is.na(y)] <- 0
  
  ## insert into recipe 
  recipe <- rbind(recipe, y)
  rm(x,y)
}

## plot (inspect)
ggplot(data=recipe, mapping=aes(x= as.numeric(year), y= area, col=collection)) +
  stat_summary(geom='line', fun='sum') +
  facet_grid(biome~level_0, scales='free') +
  theme_bw()

## export 
write.table(x= recipe,
            file= './table/area_biomes_per_state_from_5_to_7.csv', 
            fileEncoding='UTF-8',
            row.names= FALSE,
            sep='\t',
            dec='.',
            col.names= TRUE)

#write.xlsx(x= recipe,
#           file= './table/area_biomes_per_state_from_5_to_7.xlsx', 
#           col.names = TRUE, 
#           row.names = FALSE, 
#           append = FALSE)

