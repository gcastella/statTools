image2 <- function(x, y = NULL, z = NULL,...){
  if(is.null(z)){
    x <- t(x)[, nrow(x):1]
    image(x, ...)
  } else {
    z <- t(z)[, nrow(z):1]
    image(x, y, z, ...)
  }
}

contour2 <- function(x, y = NULL, z = NULL, ...){
  if(is.null(z)){
    x <- t(x)[, nrow(x):1]
    contour(x, ...)
  } else {
    z <- t(z)[, nrow(z):1]
    contour(x, y, z, ...)
  }
}

BARplot <- function(formula, data = NULL, factor.sd = 1, margin = 1, beside = TRUE, freq = FALSE, mean.par = NULL, sd.par = NULL, xlab = NULL, ylab = NULL, ...){
  df <- model.frame(formula, data)
  x <- df[, 1]
  by <- df[, -1]
  sd.arrows <- NULL
  
  if(!is.factor(x)){
    mean.bars <- do.call(tapply, c(list(x, by, mean), mean.par)) 
    sd.arrows <- mean.bars + factor.sd*do.call(tapply, c(list(x, by, sd), sd.par)) 
  } else {
    mean.bars <- table(x, by)
    if(!freq) mean.bars <- prop.table(mean.bars, margin)*100
  } 
  
  if(is.null(xlab)) xlab <- deparse(substitute(by))
  if(is.null(ylab)) ylab <- deparse(substitute(x))
  a <- barplot(height = mean.bars, xlab = xlab, ylab = ylab, beside = beside, ...)
  if(!is.factor(x)){
    arrows(x0 = a[, 1], y0 = mean.bars, x1 = a[, 1], y1 = sd.arrows, length = 0.1, angle = 90)
  }
  return(invisible(list(mean = mean.bars, sd = sd.arrows, barplot = a)))
}

ORplot <- function(formula, data, model.args = NULL, print.pvals = T, xlab = NULL, cex = 3, pch = 18, ylim = NULL, ...){
  m <- do.call(glm, c(list(formula = formula, data = data, family = binomial), model.args))
  if(is.null(xlab)) xlab <- names(m$x)[1]
  ci <- rbind(c(NA, NA), exp(confint(m, trace = F)[-1, ]))
  OR <- c(1, exp(m$coef)[-1])
  if(is.null(ylim)) ylim <- c(0.1, max(OR, ci, na.rm = T))
  pvals <- getPval(model = m, vars = -1)
  pvals <- c(NA, paste(ifelse(is.na(as.numeric(pvals)), "p", "p="), pvals, sep = ""))
  par(bty="l")
  plot(OR, pch = pch, cex = cex, xaxt = "n", xlab = "", ylim = ylim, xlim = c(0.5, length(OR) + 0.5), log = "y", ...)
  abline(h = 1, lty = 2)
  arrows(x0 = 1:length(m$coef), y0 = ci[, 1], x1 = 1:length(m$coef), y1 = ci[, 2], angle = 90, code = 3, length = 0.1)
  if(print.pvals) mtext(at = 1:length(m$coef), side = 3, text = pvals, line = 1, cex = cex/5)
  axis(side = 1, at = 1:length(m$coef), labels = m$x[[1]])
  axis(side = 1, at = mean(1:length(m$coef)), labels = xlab, line = 2, tick = F)
  return(invisible(list(model = m, OR = OR, ci = ci, p.value = pvals)))
}

HRplot <- function(formula, data, legend = TRUE, args.legend = list(x = "bottomleft", bty = "n", lty = c(1, 1)), test = TRUE, args.test = list(x = "bottomright", bty = "n"), col, ...){
  
  llcovar <- 1
  covar <- NULL
  if(length(attr(terms(formula),"term.labels")) > 0){
    covar <- rev(all.vars(formula))[1]
    llcovar <- length(levels(data[[covar]]))
  } 
  if(missing(col)){
    col <- 1:llcovar
  }
  plot(survfit(formula = formula, data = data), col = col, ...)
  if(legend & (llcovar > 1)) do.call(what = "legend", args = c(args.legend, list(col = col, legend = levels(data[[covar]]))))
  if(test & (llcovar > 1)) do.call(what = "legend", args = c(args.test, list(legend = paste0("Log-Rank p-value = ", pvalue(pchisq(lower.tail = FALSE, df = 2, q = survdiff(formula, data = data)$chisq))))))
}