## accuracy analisys from collection 6

library(ggplot2)
library(ggrepel)
library(dplyr)
library(sf)
library(stringr)
library(tools)
library(ggpmisc)

## avoid sicentific notations
options(scipen=999)

## define function to read data
readData <- function (path) {
  ## list files
  files <- list.files(path, full.names= TRUE)
  ## create an empty recipe 
  recipe <- as.data.frame(NULL)
  ## start file reading
  for (k in 1:length(files)) {
    ## read raw file
    x <- read.csv(files[k], dec=',', sep=',', encoding="UTF-8")
    
    ## build data frame
    y <- as.data.frame(cbind(
      year = rownames(x),
      accuracy = paste0(x$X.U.FEFF.Ano, '.', x$Acurácia),
      area_discordance = paste0(x$Discordância.de.Área, '.', x$Discordância.de.Alocação),
      allocation_discordance = paste0(x$X, '.', x$Y),
      ## parse collection string
      collection = rep(sapply(strsplit(
        file_path_sans_ext(
          list.files(path)),
        split='_', fixed=TRUE),
        function(x) (x[1]))[k], nrow(x)),
      ## parse level string
      level = rep(sapply(strsplit(
        file_path_sans_ext(
          list.files(path)), 
        split='_', fixed=TRUE),
        function(x) (x[2]))[k], nrow(x)
      ),
      ## parse biome
      biome = rep(sapply(strsplit(
        file_path_sans_ext(
          list.files(path)), 
        split='_', fixed=TRUE),
        function(x) (x[3]))[k], nrow(x)
      )
    )
    )
    ## merge with recipe
    recipe <- rbind(recipe, y)
  }
  return (recipe)
}

## import file
data <- readData(path= './table/')
