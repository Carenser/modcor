
context("Import program")

library(reshape2)
library(tidyverse)
library(lubridate)


test_that("program periods are correctly identified", {
  
  
  tbl_test1 = LoadEffacements(dir = paste0(getwd(), '/data'))
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

