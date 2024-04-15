test_that("bouts are detected as expected", {
  # generate data
  x = c(rep(1, 10), # start with 10-min consecutive bout (no gaps)
        rep(0, 10), # break
        rep(1, 9),  # 9 consecutive minutes
        rep(0, 6),  #break
        c(1, 1, 0, 1, 1, 0, 1, 0, 1, 1), # bout with 70% crit but no absolute gaps longer than 1 minute
        rep(0, 10),
        c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1)) # bout with 90% crit and no absolute gaps longer than 1 minute
  x15 = rep(x, each = 60/15) # 15s epochs

  # test getBout function with 10 minute bouts ------
  # percentage boutcriter
  # 60-s epoch
  b = getBout(x, boutduration = 10, boutcriter = 0.8, boutmaxgap = 1, epoch = 60)
  rleB = rle(b)
  nBouts = sum(rleB$values)
  tBouts_min = sum(x == 1 & b == 1)*60/60
  expect_equal(nBouts, 3)
  expect_equal(tBouts_min, 28)
  # 15-s epoch
  b = getBout(x15, boutduration = 10*60/15, boutcriter = 0.8, boutmaxgap = 1, epoch = 15)
  rleB = rle(b)
  nBouts = sum(rleB$values)
  tBouts_min = sum(x15 == 1 & b == 1)*15/60
  expect_equal(nBouts, 3)
  expect_equal(tBouts_min, 28)
  # absolute boutcriter
  # 60-s epoch
  b = getBout(x, boutduration = 10, boutcriter = 2, boutmaxgap = 1, epoch = 60)
  rleB = rle(b)
  nBouts = sum(rleB$values)
  tBouts_min = sum(x == 1 & b == 1)*60/60
  expect_equal(nBouts, 4)
  expect_equal(tBouts_min, 35)
  # 15-s epoch
  b = getBout(x15, boutduration = 10*60/15, boutcriter = 2, boutmaxgap = 1, epoch = 15)
  rleB = rle(b)
  nBouts = sum(rleB$values)
  tBouts_min = sum(x15 == 1 & b == 1)*15/60
  expect_equal(nBouts, 4)
  expect_equal(tBouts_min, 34)
})
