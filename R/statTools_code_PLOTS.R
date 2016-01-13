#' @export
image2 <- function(x, y = NULL, z = NULL,...){
  if(is.null(z)){
    x <- t(x)[, nrow(x):1]
    image(x, ...)
  } else {
    z <- t(z)[, nrow(z):1]
    image(x, y, z, ...)
  }
}

#' @export
contour2 <- function(x, y = NULL, z = NULL, ...){
  if(is.null(z)){
    x <- t(x)[, nrow(x):1]
    contour(x, ...)
  } else {
    z <- t(z)[, nrow(z):1]
    contour(x, y, z, ...)
  }
}

#' @export
BARplot <- function(formula, data = NULL, 
                    factor.sd = 1, margin = 1, 
                    beside = TRUE, freq = FALSE, 
                    mean.par = NULL, sd.par = NULL, 
                    xlab = NULL, ylab = NULL, ylim, ...){
  df <- model.frame(formula, data)
  x <- df[, 1]
  by <- df[, -1]
  sd.arrows <- NULL
  
  if(!is.factor(x)){
    mean.bars <- do.call(tapply, c(list(x, by, mean), mean.par))
    sds <- do.call(tapply, c(list(x, by, sd), sd.par))
    sd.arrows <- mean.bars + factor.sd * apply(sds, 1:length(dim(sds)), function(x) ifelse(is.na(x), 0, x))
  } else {
    mean.bars <- table(df)
    if(!freq) mean.bars <- prop.table(mean.bars, margin) * 100
    sd.arrows <- mean.bars
  } 
  
  if(is.null(xlab)) xlab <- paste0(names(df)[-1], collapse = " ")
  if(is.null(ylab)) ylab <- names(df)[1]
  if(missing(ylim)) ylim <- c(0, max(sd.arrows, na.rm = TRUE))
  a <- barplot(height = mean.bars, xlab = xlab, ylab = ylab, beside = beside, ylim = ylim, ...)
  if(!is.factor(x)){
    sapply(seq_along(a), function(x){
      arrows(x0 = as.numeric(a)[x], y0 = as.numeric(mean.bars)[x], 
             x1 = as.numeric(a)[x], y1 = as.numeric(sd.arrows)[x], 
             length = 0.1, angle = 90)
    })
  }
  return(invisible(list(mean = mean.bars, sd = sd.arrows, barplot = a)))
}

#' @export
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

#' @export
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

#' @export
missPlot <- function(data, dendrogram = TRUE, percents = TRUE, reorderfun, ...){
  NA_log <- is.na(data)
  NA_mat <- apply(X = NA_log, MARGIN = 1:2, FUN = as.integer)
  row_lab <- rownames(NA_mat) <- rep("", nrow(NA_mat))
  if(percents){colnames(NA_mat) <- inbra(colnames(NA_mat), 100 * colMeans(NA_mat), add = "%")}
  if(dendrogram){
    if(missing(reorderfun)){
      order_dend <-function(d, w){
        n <- attributes(d)$members
        bool <- n == ncol(data)
        if(bool){
          reorder(d, colSums(NA_mat), agglo.FUN = mean)
        } else {
          reorder(d, -rowSums(NA_mat), agglo.FUN = mean)
        }
      }
      heatmap(NA_mat, scale = "none", labRow = row_lab, reorderfun = order_dend, ...)
    } else {
      heatmap(NA_mat, scale = "none", labRow = row_lab, ...)
    }
  } else {
    NA_mat <- NA_mat[, order(colMeans(NA_mat), decreasing = TRUE)]
    heatmap(NA_mat, scale = "none", labRow = rownames(NA_mat), ...)
  }
}  

