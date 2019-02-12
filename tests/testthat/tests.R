library(dplyr)
library(tidyr)

setwd(system.file("extdata", package = "Fars"))

# load data conventionally
#data_2014 <- read.csv("accident_2014.csv.bz2")
# load data with the package's function
#data_2014_fars <- fars_read("accident_2014.csv.bz2")

# compare the output
#test_that("fars_read output", {
#  expect_is(data_2014_fars, c("tbl_df", "tbl", "data.frame")) # the output should be a dataframe, tibble or tibble dataframe
#  expect_equal(colnames(data_2014_fars), colnames(data_2014)) # the column names should be the same as when you read it in with read.csv
#  expect_error(fars_read("accident_2017.csv.bz2")) # it should throw an error when prompted to read in a file that does not exist
#})

# make_filename test
years <- c(2013, 2014, 2015)
# create the files names as they should be
names <- c("accident_2013.csv.bz2",
           "accident_2014.csv.bz2",
           "accident_2015.csv.bz2")
# let function create the filesames
names_fars <- make_filename(years)
# compare the output of the function to the manually constructed filenames
test_that("make_filename output", {
  expect_is(names_fars, "character")
  expect_identical(names_fars, names)
})
