#####################
### IMPORTANT!!! ###
#####################
## Don't forget to set a seed before any project!
set.seed(42)

suppressPackageStartupMessages({
library(dplyr)
library(spatstat.core)
library(Seurat)
library(patchwork)
library(DoubletFinder)
library(SingleR)
library(enrichR)
library(SingleCellExperiment)
library(SeuratWrappers)
library(tidyverse)
library(celldex)
})

#define directory with scRNAseq data
directory <- '/icbb/projects/igunduz/scb_ws2425/project_1_bm/data/'

#Sample Information
samples <- data.frame(file = c('GSM4138872_scRNA_BMMC_D1T1.rds', 'GSM4138873_scRNA_BMMC_D1T2.rds', 'GSM4138874_scRNA_CD34_D2T1.rds', 'GSM4138875_scRNA_CD34_D3T1.rds' ), 
                      names = c('BMMC_D1T1', 'BMMC_D1T2', 'CD34_D2T1', 'CD34_D3T1'), 
                      donor = c('D1', 'D1', 'D2', 'D3'), 
                      replicate = c('T1', 'T2', 'T1', 'T1'), 
                      sex = c('F', 'F', 'M', 'M'), 
                      group = c('BMMC', 'BMMC', 'CD34', 'CD34'))


#function for loading a sample and turning it into a Seurat-Object
loadDataSet <- function(directory, sample, i){
  filename <-paste(directory, sample$file[i], sep="")
  raw_counts <- readRDS(file =filename)
  pbmc <- CreateSeuratObject(counts=raw_counts, project=sample$names[i], assay = "RNA")
  return(pbmc)
}

list_of_samples <- c()
for (i in 1: length(samples$file)){
  pbmc <- loadDataSet(directory, samples, i);
  list_of_samples <- c(list_of_samples, pbmc);
}

addMetaData <- function(pbmc, samples,i){
  pbmc$orig.ident <- samples$names[i]
  pbmc$donor <- samples$donor[i]
  pbmc$replicate <- samples$replicate[i]
  pbmc$sex <- samples$sex[i]
  pbmc$group <- samples$group[i]
  return(pbmc)
}
list_of_samples <- lapply(c(1:4),function(i) addMetaData(list_of_samples[[i]], samples, i))

for (sample in list_of_samples){
  print(ncol(sample))
  print(nrow(sample))
  print(head(pbmc@meta.data))
}

