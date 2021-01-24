#THIS IS A TEST SCRIPT TO MAKE SURE THAT THE FUNCTION IN create_summary_data
#RETURNS A DATASET OF dim 2 x 12 AS THE FUNCTION final_set IS SUPPOSED TO 
#DO THAT

source("create_summary_data.R")
library(testthat)

#correct dimensions
dim_sum <- c(2,12)

#dataframe from function final_set
superId<- "252669123725536,[SAT-AIS],3,Tug"
example <- final_set(ships,superId)

#testing dimensions
test_that("dim mapdata", {
  expect_equal(dim(example),dim_sum)
})

