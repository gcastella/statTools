% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statTools_code_BIVAR.R
\name{bivarTable}
\alias{bivarTable}
\alias{bivarTable.default}
\alias{bivarTable.formula}
\title{Assessing differences in groups}
\usage{
bivarTable(X, ...)

\method{bivarTable}{default}(X, y = NULL, data = NULL,
  margin = getOption("margin"), rounding = getOption("rounding"),
  condense.binary.factors = getOption("condense.binary.factors"),
  drop.x = getOption("drop.x"), drop.y = getOption("drop.y"),
  test = c("none", "parametric", "non-parametric", "both"),
  outcome = getOption("outcome"),
  pvalues.model = getOption("pvalues.model"), fit.model = NULL,
  FUN.model = NULL, ...)

\method{bivarTable}{formula}(X, data = NULL, margin = getOption("margin"),
  rounding = getOption("rounding"),
  condense.binary.factors = getOption("condense.binary.factors"),
  drop.x = getOption("drop.x"), drop.y = getOption("drop.y"),
  test = c("none", "parametric", "non-parametric", "both"),
  outcome = getOption("outcome"),
  pvalues.model = getOption("pvalues.model"), fit.model = NULL,
  FUN.model = NULL, ...)
}
\arguments{
\item{X}{A data frame with variables in the rows (for .default) or a formula with additive terms (. and - are allowed too) and with 0 or 1 variable in the LHS.
LHS of the formula defines the grouping variable, and RHS where to assess differences (for .formula).}

\item{...}{Extra arguments for the functions in \code{FUN.model} or the descriptive functions \code{descr_var()}. To change arguments from the functions, add a list argument named 
as the model that the function is applied to, whose elements will be the lists with arguments to modify (passed to do.call), and the 
name of each list of arguments the function those arguments belong to (See Examples).}

\item{y}{Grouping variable in the default method.}

\item{data}{Data frame with the variables in \code{X}.}

\item{margin}{For factors, percentages are calculated. If \code{margin = 1} row percentages sum 100\%, 
if 2, column percentages sum 100\%. See \code{margin} from \code{prop.table}.}

\item{rounding}{Decimal places for all numeric values in the table. P-values are rounded with another criterion.}

\item{condense.binary.factors}{Use only one single row for binary factors to avoid redundancy.}

\item{drop.x}{Perform \code{factor(x)} before all computations to delete unused levels in RHS variables.}

\item{drop.y}{Perform \code{factor(y)} before all computations to delete unused levels the grouping variable.}

\item{test}{One of "both", "parametric", "non-parametric" or "none". Defines the type of tests to perform in all variables in RHS.
See Details section.}

\item{outcome}{If 1, RHS are understood as the dependent variables when fitting models. 
If 2, LHS variable is understood as the dependent variable in each model fitted.}

\item{fit.model}{A NAMED list with formulas for fitting models to each row. These formulas are passed to \code{update.formula}
so \code{.} here refers to each RHS or LHS variable, depending on argument \code{output}. See Details.}

\item{FUN.model}{A NAMED list. Each element has to be named like one of the elements in \code{fit.model}.
Each element in FUN.model will be a character vector with the names of functions to be executed in each model to return more particular outputs.
If the vector is named, these are used as column names in the final table (See Examples). The first argument of the function
has to be the model fitted. Other arguments can be changed in \code{...}. Examples of functions used for this are 
nagelkerke(), adjNagelkerke(), getPval(), getBetaSd(), getORCI(), verticalgetPval(), verticalgetBetaSd(), verticalgetORCI(), GoF().}

\item{pvalue.model}{Logical. If TRUE a p-value from the drop1 methods are provided for each model fitted.}
}
\value{
A list with class bivarTable, where the first argument is the table generated (a matrix) and the other arguments
are input arguments returned as outputs in the list, such as margin, outcome, fit.model, FUN.model, test, drop.x,
drop.y, condense.binary.factors, data.frame with the values from variables in the rows (named X), and the same for
the columns (named y), and extra arguments in ..., with maybe, the arguments for calling the functions in FUN.model.
}
\description{
This function creates a table resulting from an analysis comparing two or more groups .
}
\details{
The grouping variable (LHS of the formula or y for .default) is set on the columns and the other variables (RHS of the formula or X data frame for .default).
Variables are read from argument data. Default options for some arguments can be changed using options(argument = value).

The tests performed in each case are defined in options()$parametric.tests and options()$non.parametric.tests. Changing them, the tests
performed will change, however, to get the p-value from the tests, the new tests must return a list with an element called p.value. 
The default tests are:

Quantitative row variable vs binary grouping variable: t-test (t.test) or Mann-Whitney test (wilcox.test).

Quantitative row variable vs non-binary grouping variable: ANOVA (oneway.test) or Kruskal-Wallis test (kruskal.test).

Qualitative row variable: Chi-squared test (chisq.tes) or Fisher test (fisher.test).

For fitting models, first decide whether the grouping variable is the dependent variable or not. If it is, set ouput to 2 otherwise to 1,
meaning that the dependent variable will be the one in the rows of the table (RHS of the formula). Formulas to fit the model must use the dot
in the RHS of the formula to refer to the independent variable (either grouping variable or the one in the rows).
when the response variable is a binary factor, a logistic regression model is fitted. When is a non-binary factor, the variable
is converted to numeric preserving the order and a linear regression model is fitted. When the response is a quantitative variable linear models are
fitted.
}
\section{Methods (by class)}{
\itemize{
\item \code{default}: Default method for bivarTable

\item \code{formula}: Formula method for bivarTable
}}
\examples{
iris$prova <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 2)
iris$prova2 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 3)
iris$prova3 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 5)
bvt <- bivarTable(X = Species ~ . + I(Sepal.Width>3) - Sepal.Length, 
                  data = iris, 
                  margin = 2, 
                  outcome = 1, 
                  drop.x = TRUE, 
                  drop.y = TRUE, 
                  test = "non-par", 
                  condense.binary.factors = TRUE, 
                  rounding = 3,
                  fit.model = list(simple = ~ .,
                                   adj = ~ . + Sepal.Length), 
                  FUN.model = list(simple = c("getPval"), 
                                   adj = c("OR (95\% CI)" = "getORCI", 
                                           "p-value" = "getPval")),
                  adj = list(getPval = list(vars = 4), 
                             getORCI = list(vars = 4)))
bvt
bvt <- bivarTable(X = I(Sepal.Width>3) ~ . - Sepal.Length, data = iris,          ## STRUCTURE
                  rounding = 3, condense.binary.factors = FALSE,                 ## OPTIONS
                  drop.x = TRUE, drop.y = TRUE, margin = 2:1,                    ## OPTIONS
                  test = "both",                                                 ## TESTS
                  fit.model = list(simple = ~ .,                                 ## FIT MODEL WITH NO COVARIATES
                                   adj = ~ . + Sepal.Length),                    ## FIT ADJUSTED MODEL
                  outcome = 2,                                                   ## LHS AS DEPENDENT VARIABLE (COLUMNS)
                  FUN.model = list(simple = c("p-value" = "getPval"),            ## GET A P-VALUE FROM THE FIRST MODEL
                                   adj = c("OR (95\% CI)" = "verticalgetORCI",    ## GET ALL OR's FROM THE 2nd MODEL
                                           "p-value" = "verticalgetPval")),      ## GET ALL P-VALUES FROM THE 2nd MODEL
                  show.quantiles = c(0, 1))                                      ## QUANTILES: probs ARGUMENT
                                                                                 ## PASSED TO descr_var()
bvt
\dontrun{
export(bvt)                                                                      ## OPEN bvt IN EXCEL
}

}
\seealso{
\code{\link{export}}
}

