.onLoad <- function(libname = find.package("statTools"), pkgname = "statTools") {
  options(parametric.tests = list(numeric.binary = list("t.test"), 
                                  numeric.multi = list("oneway.test", var.equal = TRUE),
                                  factor = list("chisq.test")))
  options(non.parametric.tests = list(numeric.binary = list("wilcox.test"), 
                                  numeric.multi = list("kruskal.test"),
                                  factor = list("fisher.test", simulate.p.value = TRUE)))
  options(p.value.rounding = list("pvalue"))
  options(condense.binary.factors = TRUE)
  options(rounding = 2)
  options(drop.x = TRUE)
  options(drop.y = TRUE)
  options(margin = 2)
  options(outcome = 1)
  options(pvalues.model = TRUE)
}