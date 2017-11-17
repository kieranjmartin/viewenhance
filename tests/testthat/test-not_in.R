context("Check %not in% function works identically to ! %in%")

charvec <- letters[1:10]
charvec2 <- letters[5:12]
set.seed(17112017)
genwords <-function(N){
    paste0(sample(letters, N, replace=T), collapse = '')
}
genstring <- function(length, max = 100){
    unlist(lapply(sample(seq(1, max), size = length, replace = T), genwords))
}

string1 <- genstring(10)
string2 <- genstring(22)
string3 <- genstring(100)

string1_smalldiff <- string1
string1_smalldiff[7] <- 'different'

string2_smalldiff <- string2
string2_smalldiff[3] <- 'different words here'

string3_smalldiff <- string3
string3_smalldiff[76] <- 'ss'

testfun <- function(string, stringcomp){
    string <- get(string)
    stringcomp <- get(stringcomp)
    expect_equal(!(string %in% stringcomp),
                 string %not in% stringcomp)
}

allnames <- c('charvec', 'charvec2', 'string1', 'string2', 'string3',
              'string1_smalldiff', 'string2_smalldiff', 'string3_smalldiff')

allcombinations <- expand.grid(allnames, allnames)

test_that('not in and in work the same but with the negation',{

    for (i in 1:nrow(allcombinations)){
        testfun(string = as.character(allcombinations[i,1]),
                stringcomp = as.character(allcombinations[i,2]))
    }
    })
