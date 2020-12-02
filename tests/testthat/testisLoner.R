context("check function: isLoner")

library("dataReporter")


## Read atomic vectors
source("atomic.R")

x <- factor(LETTERS[c(4,4,1,2,1,3,2,4,2, 4,4,1,2,1,3,2,4,2, 4,4,1,2,1,3,2,4,2)+1], levels=c("A", "B", "C", "D", "E"))
                                       
## Each type should result in a list
test_that("isLoner returns the right values", {
    expect_equal(reporteR::identifyLoners(x)$problemValues, "D")
})

