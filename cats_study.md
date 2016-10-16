# Simple Linear Correlation and Regression

## Correlation

Correlation is used to test for a relationship between two numerical variables or two ranked (ordinal) variables. In this tutorial, we assume the relationship (if any) is linear.

To demonstrate, we will begin with a data set called "cats" from the "MASS" library, which contains information on various anatomical features of house cats... 

	library("MASS")
	data(cats)
	str(cats)

`cats` dataset contains 144 observations of 3 variables.

`Bwt` is body weight, `Hwt` is heart weight.


## Regression

Simple (as well as multiple) linear regression is done using the lm( ) function. This function requires a formula interface, and for simple regression the formula takes the form DV ~ IV, which should be read something like "DV as a function of IV"

**Question**: use `cor`, `lm`, `anova` and use `R2` to generate a simple linear model and assess its quality.

