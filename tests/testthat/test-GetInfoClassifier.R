test_that("GetInfoClassifer works", {

  # Preschool Wrist Random Forest Free Living
  x = GetInfoClassifier("Preschool Wrist Random Forest Free Living")
  expect_equal(x$epoch, 15)
  expect_equal(class(x$rfmodel)[1], "train")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "lightGames", "moderatetovigorousGames",
                            "walk", "run" ))
  # Preschool Hip Random Forest Free Living
  x = GetInfoClassifier("Preschool Hip Random Forest Free Living")
  expect_equal(x$epoch, 15)
  expect_equal(class(x$rfmodel)[1], "train")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "lightGames", "moderatetovigorousGames",
                            "walk", "run" ))
  # Preschool Hip Random Forest Free Living Lag-Lead
  x = GetInfoClassifier("Preschool Hip Random Forest Free Living Lag-Lead")
  expect_equal(x$epoch, 15)
  expect_equal(class(x$rfmodel)[1], "randomForest")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "lightGames", "moderatetovigorousGames",
                            "walk", "run" ))
  # Preschool Wrist Random Forest Free Living Lag-Lead
  x = GetInfoClassifier("Preschool Wrist Random Forest Free Living Lag-Lead")
  expect_equal(x$epoch, 15)
  expect_equal(class(x$rfmodel)[1], "randomForest")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "lightGames", "moderatetovigorousGames",
                            "walk", "run" ))
  # School age Wrist Random Forest
  x = GetInfoClassifier("School age Wrist Random Forest")
  expect_equal(x$epoch, 15)
  expect_equal(class(x$rfmodel)[1], "randomForest.formula")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "lightGames", "moderatetovigorousGames",
                            "walk", "run" ))
  # School age Hip Random Forest
  x = GetInfoClassifier("School age Hip Random Forest")
  expect_equal(x$epoch, 10)
  expect_equal(class(x$rfmodel)[1], "train")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "lightGames", "moderatetovigorousGames",
                            "walk", "run" ))
  # Adult Wrist RF Trost
  x = GetInfoClassifier("Adult Wrist RF Trost")
  expect_equal(x$epoch, 10)
  expect_equal(class(x$rfmodel)[1], "randomForest.formula")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sedentary", "stationary", "walk", "run" ))
  # Adult women Wrist RF Ellis
  x = GetInfoClassifier("Adult women Wrist RF Ellis")
  expect_equal(x$epoch, 60)
  expect_equal(class(x$rfmodel)[1], "randomForest")
  expect_equal(class(x$hmmmodel)[1], "list")
  expect_equal(x$classes, c("biking", "sedentary", "standMoving",
                            "standStill", "vehicle", "walk"))
  # Adult women Hip RF Ellis
  x = GetInfoClassifier("Adult women Hip RF Ellis")
  expect_equal(x$epoch, 60)
  expect_equal(class(x$rfmodel)[1], "randomForest")
  expect_equal(class(x$hmmmodel)[1], "list")
  expect_equal(x$classes, c("biking", "sedentary", "standMoving",
                            "standStill", "vehicle", "walk"))
  # Thigh Decision Tree
  x = GetInfoClassifier("Thigh Decision Tree")
  expect_equal(x$epoch, 10)
  expect_equal(class(x$rfmodel)[1], "NULL")
  expect_equal(class(x$hmmmodel)[1], "NULL")
  expect_equal(x$classes, c("sit", "stand", "standMove",
                            "walk", "run", "climbStairs", "bike"))
})
