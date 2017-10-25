context("Check names label function works")

X<- data.frame(x=4, y=5, z=6)
Y<- data.frame(matrix(rnorm(100000),ncol=10000))
attr(X$x, 'label') <- 'label for x'
attr(X$z, 'label') <- 'label for z'

test_that('check non data frames return an error',{
  expect_error(names_label(c(1,2,3), 'Name'))
  expect_error(names_label(c(1,2,3), 'Label'))
  expect_error(names_label(list(data.frame(x=3)), 'Label'))
  expect_error(names_label(list(data.frame(x=3)), 'Name'))

})

test_that('Non name/labels error',{
  expect_error(names_label(X, 'ddd'))
  expect_error(names_label(Y, 111))
  expect_error(names_label(X, 'texther'))

})

test_that('Names are returned when names_label is applied to a data frame',
          {
            expect_equal(names(X), names_label(X))
            expect_equal(names(X), names_label(X, 'Name'))
            expect_equal(names(Y), names_label(Y))
            expect_equal(names(Y), names_label(Y, 'Name'))
          })

test_that('Labels are returned when names_label is applied to a data frame',{
  expect_equal(names_label(X, 'Label')[1], attr(X$x, 'label') )
  expect_equal(names_label(X, 'Label')[3], attr(X$z, 'label') )
})

test_that('When a column doesnt have a label, its assigned back to the name',{
  expect_equal(names_label(X, 'Label')[2], 'y' )
  expect_equal(names_label(X, 'Label')[3], attr(X$z, 'label') )
})
