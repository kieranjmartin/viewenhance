context("Check subset_lab retains attributes and acts similiarly to subset otherwise")


test_that('subset examples work identically for both',{


expect_equal(subset_lab(airquality, Temp > 80, select = c(Ozone, Temp)),
             subset(airquality, Temp > 80, select = c(Ozone, Temp)))
expect_equal(subset_lab(airquality, Day == 1, select = -Temp),
             subet(airquality, Day == 1, select = -Temp))
expect_equal(subset_lab(airquality, select = Ozone:Wind),
             subet(airquality, select = Ozone:Wind))

})

test_that('column attributes are preserved',{
    airquality_test <- airquality
    attr(airquality_test$Temp, 'test') <- data.frame(x=1, y=2)
    attr(airquality_test$Ozone, 'test2') <- list(one = 'one', two = data.frame(x=1, y=2))

    expect_equal(attr(airquality_test$Temp, 'test'),
                 attr(subset_lab(airquality_test)$Temp, 'test'))
    expect_equal(attr(airquality_test$Temp, 'test'),
                 attr(subset_lab(airquality_test, Temp >80, select = Temp)$Temp, 'test'))
    expect_equal(attr(airquality_test$Temp, 'test'),
                 attr(subset_lab(airquality_test, select = -Ozone)$Temp, 'test'))
    expect_equal(attr(airquality_test$Temp, 'test'),
                 attr(subset_lab(airquality_test, select = 'Temp')$Temp, 'test'))

    expect_equal(attr(airquality_test$Ozone, 'test2'),
                 attr(subset_lab(airquality_test)$Ozone, 'test2'))
    expect_equal(attr(airquality_test$Ozone, 'test2'),
                 attr(subset_lab(airquality_test, Temp >80, select = Ozone)$Ozone, 'test2'))
    expect_equal(attr(airquality_test$Ozone, 'test2'),
                 attr(subset_lab(airquality_test, select = -Temp)$Ozone, 'test2'))
    expect_equal(attr(airquality_test$Ozone, 'test2'),
                 attr(subset_lab(airquality_test, select = 'Ozone')$Ozone, 'test2'))

})
