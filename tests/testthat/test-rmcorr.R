test_that("symbol column names work", {
  expect_warning(rmcorr(Subject, PacO2, pH, bland1995), "coerced into a factor")
})

test_that("string column names work", {
    expect_warning(rmcorr("Subject", "PacO2", "pH", bland1995), "coerced into a factor")
})

test_that("dynamic column names work", {
    col_names <- c("Subject", "PacO2", "pH")
    expect_warning(rmc.out <- rmcorr(col_names[1], col_names[2], col_names[3], bland1995), "coerced into a factor")
    expect_equal(rmc.out$r, -0.507, tolerance = 0.001)
})

test_that("different classes of sub_ids work", {
    bland1995mod <- bland1995
    #change the subject column from integers to characters
    bland1995mod$Subject[1:4]  <- "a"
    
    expect_warning(rmc.out <- rmcorr(Subject, PacO2, pH, bland1995mod), "coerced into a factor")
    expect_equal(rmc.out$CI[1], -0.711, tolerance = 0.001)
    
    #change to factor, shouldn't get warning
    expect_silent(rmc.out <- rmcorr(as.factor(Subject), PacO2, pH, bland1995mod))
})

test_that("non-numeric data columns throw error", {
    bland1995mod <- bland1995
    bland1995mod$pH <- as.character(bland1995mod$pH)
    expect_warning(expect_error(rmcorr(Subject, as.character(PacO2), pH, bland1995),"must be numeric"), "coerced")
})

test_that("changing CI level works", {
    #use default 95% CIs
    expect_warning(rmc.out <- rmcorr(sub, rt, acc, gilden2010))
    expect_equal(rmc.out$CI[1], -0.661, tolerance = 0.001)
    
    set.seed(1753)
    expect_warning(rmc.out <- rmcorr(sub, rt, acc, gilden2010, CIs = "bootstrap"))
    expect_equal(rmc.out$CI[[1]], -0.604, tolerance = 0.001)
    
    #use 99% CIs
    expect_warning(rmc.out <- rmcorr(sub, rt, acc, gilden2010, CI.level = 0.99))
    expect_equal(rmc.out$CI[1], -0.721, tolerance = 0.001)
    
    set.seed(1212)
    expect_warning(rmc.out <- rmcorr(sub, rt, acc, gilden2010, CIs = "bootstrap", CI.level = 0.99))
    expect_equal(rmc.out$CI[[1]], -0.610, tolerance = 0.001)
})