
context("calculate load curtailment")

library(reshape2)
library(tidyverse)
library(lubridate)


test_that("load curtailement volums are correctly calculated", {
  
  
  tbl_test1_eff = LoadEffacements(dir = paste0(getwd(), '/data'))
  tbl_test1_cdc = LoadCdC(dir = paste0(getwd(), '/data'), files = 'C:/Users/d33071/Documents/R/MyProj/modcor/data/NEBEF_CRS_GRD_20171230_17X100A100A0001A_20180209170814.csv')
  
  tbl_test1_curtailement = CR_RectangleDouble(cdc = tbl_test1_cdc, eff = tbl_test1_eff)
  
  expect_equal(tbl_test1, 1)
  
})

test_that("NEBEF and MA programs are correcly concatenated", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})


# Chargement des fonctions et librairies necessaires au traitement

source(paste(getwd(),'R/LoadData.R', sep="/"))

source(paste(getwd(),'R/CR_Rectangles.R', sep="/"))

source(paste(getwd(),'R/CRModeCorrige.R', sep="/"))


# test des effacements

