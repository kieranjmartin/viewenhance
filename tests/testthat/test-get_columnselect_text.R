context('check that the string productioned by get_columnselect_text is correct')


test_that('correctly ordered statements',{
expect_equal(get_columnselect_text('expr', 'searchstring', 'Label', 'Dataframename'),
             "(expr(names_label(Dataframename,'Label'),'searchstring'))")

expect_equal(get_columnselect_text('expr', 'searchstring', 'Label', 'Dataframename', T),
             "(expr('searchstring', names_label(Dataframename,'Label')))")
})
