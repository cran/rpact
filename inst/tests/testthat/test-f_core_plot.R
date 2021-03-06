#:#  
#:#  *Unit tests*
#:#  
#:#  This file is part of the R package rpact:
#:#  Confirmatory Adaptive Clinical Trial Design and Analysis
#:#  
#:#  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
#:#  Licensed under "GNU Lesser General Public License" version 3
#:#  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
#:#  
#:#  RPACT company website: https://www.rpact.com
#:#  RPACT package website: https://www.rpact.org
#:#  
#:#  Contact us for information about our services: info@rpact.com
#:#  
#:#  File name: test-f_core_plot.R
#:#  Creation date: 09 November 2020, 11:48:24
#:#  File version: $Revision: 3854 $
#:#  Last changed: $Date: 2020-11-09 14:53:50 +0100 (Mon, 09 Nov 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing .reconstructSequenceCommand")


test_that("The output is as exptected", {
	expect_equal(.reconstructSequenceCommand(seq(-1, 1, 0.02)), "seq(-1, 1, 0.02)")
	expect_equal(.reconstructSequenceCommand(c()), NA_character_)
	expect_equal(.reconstructSequenceCommand(c(1)), "1")
	expect_equal(.reconstructSequenceCommand(c(1, 2)), "c(1, 2)")
	expect_equal(.reconstructSequenceCommand(c(1, 2, 3)), "c(1, 2, 3)")
	expect_equal(.reconstructSequenceCommand(c(1, 2, 3, 4)), "seq(1, 4, 1)")
	expect_equal(.reconstructSequenceCommand(c(1, 2, 3, 5)), "c(1, 2, 3, 5)")

	expect_true(grepl(.getRexepSaveCharacter("x$.design"), "x$.design"))
	expect_true(grepl(.getRexepSaveCharacter("x$.design"), "c(x$.design, xxx)"))
	expect_false(grepl(.getRexepSaveCharacter("x$.design"), "c(x$design, xxx)"))

})

