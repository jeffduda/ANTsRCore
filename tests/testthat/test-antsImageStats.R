context("antsImageStats")

# Use pixeltype=="double" for tests, if using the default pixeltype=="float"
# then all test will fail due loss of precision
set.seed(20170525)

values = rnorm(100)
varimg = makeImage(c(10, 10), values, pixeltype = "double")
mask = varimg > 0

test_that("mean in image is correct", {
  expect_equal( mean(values), mean(varimg) )
})

test_that("mean in masked image is correct", {
  expect_equal(mean(values[values>0]), mean(varimg,mask) )
})

test_that("var in image is correct", {
  expect_equal( var(values), var(varimg) )
})

test_that("var in masked image is correct", {
  expect_equal(var(values[values>0]), var(varimg,mask) )
})

test_that("sd in image is correct", {
  expect_equal( sd(values), sd.antsImage(varimg) )
})

test_that("sd in masked image is correct", {
  expect_equal(sd(values[values>0]), sd.antsImage(varimg,mask) )
})
