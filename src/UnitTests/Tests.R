library(testthat)

# 1) Small point cloud (format = 0
test_that("ripser_cpp works on a tiny 2D point cloud", {
  # Three points in 2D making a right triangle
  points <- matrix(c(
    0, 0,
    1, 0,
    0, 1
  ), nrow = 3, byrow = TRUE)
  
  # dim = 1 => we want up to 1D homology (connected components + loops)
  # threshold = 2, p = 2 (prime field), format = 0 => point cloud
  barcodes <- ripser_cpp(points, dim = 1, thresh = 2.0, p = 2, format = 0)
  
  # At least confirm we get something back
  expect_true(length(barcodes) > 0)
  # Each interval is dimension, birth, death -> length(barcodes) should be multiple of 3
  expect_equal(length(barcodes) %% 3, 0)
})

# 2) Small distance matrix (format = 1
test_that("ripser_cpp works on a 3x3 distance matrix", {
  # Distances for 3 points
  dist_mat <- matrix(c(
    0,   1,   1.4,
    1,   0,   1,
    1.4, 1,   0
  ), nrow = 3, byrow = TRUE)
  
  # format = 1 => interpret it as a lower-distance matrix
  barcodes <- ripser_cpp(dist_mat, dim = 1, thresh = 2.0, p = 2, format = 1)
  
  expect_true(length(barcodes) > 0)
  expect_equal(length(barcodes) %% 3, 0)
})

#3) Single point edge case
test_that("ripser_cpp works with a single point (dim=1)", {
  # One point in 2D
  single_pt <- matrix(c(0, 0), nrow = 1)
  
  # Might only get H0 (connected component) with infinite death
  # or no intervals, depending on how code handles it
  barcodes <- ripser_cpp(single_pt, dim = 1, thresh = 10, p = 2, format = 0)
  
  # Just confirm it doesn't crash or error
  expect_true(length(barcodes) >= 0)
})

# 4) Non-prime modulus => expect error
test_that("ripser_cpp stops if p is non-prime", {
  pts <- matrix(c(
    0, 0,
    1, 1
  ), nrow = 2, byrow = TRUE)
  
  # p = 4 is not prime => expect the code to throw an error
  expect_error(
    ripser_cpp(pts, dim = 1, thresh = 2.0, p = 4, format = 0),
    regexp = "Non-prime"
  )
})

# 5) Request a large dimension
test_that("ripser_cpp clamps dim if it's bigger than (n - 2)", {
  pts <- matrix(c(
    0,0,0,
    1,0,0,
    0,1,1
  ), nrow = 3, byrow = TRUE)
  
  # We have 3 points, so dim=5 is bigger than n-2=1
  # The code should clamp dim to 1 and not fail
  expect_silent({
    barcodes <- ripser_cpp(pts, dim = 5, thresh = 10, p = 2, format = 0)
  })
  expect_true(length(barcodes) %% 3 == 0)
})

# 6) Zero or negative threshold
test_that("ripser_cpp runs with threshold <= 0", {
  pts <- matrix(c(
    0,0,
    1,0,
    0,1
  ), nrow = 3, byrow = TRUE)
  
  # Code may interpret threshold <= 0 as infinite or skip all edges
  # We just check it doesn't crash
  barcodes_zero <- ripser_cpp(pts, dim = 1, thresh = 0, p = 2, format = 0)
  expect_true(length(barcodes_zero) >= 0)
  
  barcodes_neg <- ripser_cpp(pts, dim = 1, thresh = -1, p = 2, format = 0)
  expect_true(length(barcodes_neg) >= 0)
})

# 7) Repeated points 
test_that("ripser_cpp handles repeated points", {
  # Two copies of the same point
  pts <- matrix(c(
    0,0,
    0,0
  ), nrow = 2, byrow = TRUE)
  
  barcodes <- ripser_cpp(pts, dim = 1, thresh = 2, p = 2, format = 0)
  # Possibly an edge of distance 0
  expect_true(length(barcodes) >= 0)
  expect_equal(length(barcodes) %% 3, 0)
})

# 8) Test ripser_cpp_dist() 
test_that("ripser_cpp_dist handles manually supplied distances", {
  # For 4 points => 6 pairwise distances in the upper-triangular form
  dist_vec <- c(1,2,3, 4,5, 6)  # length = 6
  # Not necessarily a "nice" geometry, but valid length
  out <- ripser_cpp_dist(dist_vec, dim = 1, thresh = 10, p = 2)
  
  expect_true(length(out) %% 3 == 0)
})
