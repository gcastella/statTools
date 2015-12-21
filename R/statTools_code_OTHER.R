#' Rounding criterion for p-values
#' 
#' Rounding significant p-values to the first non-zero decimal number and non-significant to two decimal places.
#' 
#' @details
#' P-values below 0.00001 are displayed as \code{"<0.00001"}. This is the default function for rounding p-values. 
#' In case of confusion when the rounding returns exactly the value alpha, another decimal place is given.
#' This can be changed in \code{options("p.value.rounding")}, with is a list where the first element is the name
#'  of the function as a string, and the other elements, the parameters to pass to this function.  
#' 
#' @param pval Vector or matrix of p-values to be rounded.
#' @param show.sig.stars Default to FALSE. TRUE for displaying stars, dots or nothing depending on the significance of the p-value.
#' @param alpha Default to 0.05. Threshold for significance.
#' 
#' @return An object with the same class of pval, with all the p-values properly rounded and returned as strings.
#' 
#' @examples
#' pvalue(c(0.684736, 0.06543, 0.05146673, 0.05042, 0.049831, 0.04395, 
#'          0.0232, 0.0266, 0.0001235, 0.000000124527))
#' pvalue(c(0.684736, 0.06543, 0.05146673, 0.05042, 0.049831, 0.04395, 
#'          0.0232, 0.0066, 0.0001, 0.0001235, 0.000000124527), show.sig.stars = TRUE)
#' pvalue(c(0.684736, 0.16543, 0.10146673, 0.10042, 0.09949831, 0.0904395, 
#'          0.0910232, 0.090266, 0.04001235, 0.0002546, 0.000000124527), alpha = 0.1)
#' \dontrun{
#' # Use stars as a default option when calling pvalue() from another function such as bivarTable().
#' options("p.value.rounding" = list("pvalue", show.sig.stars = TRUE))
#' }
#' @seealso \code{\link{bivarTable}}
#' @export
pvalue <- function(pval, show.sig.stars = FALSE, alpha = 0.05){
  arrodonir <- function(pval, show.sig.stars, alpha){
    if(is.na(pval) | pval < 0) return("-")
    ndec <- nchar(as.character(signif(alpha,1)))-2
    pval2 <- if(pval >= alpha) round(pval, ndec) else signif(pval, 1)
    
    i <- 1
    while(pval2 == signif(alpha, i)){
      pval2 <- round(pval, ndec + i)
      if(pval2 == pval) break
      i <- i+1
    }
    pval2 <- format(pval2, scientific = F)
    if(pval < 0.00001){
      pval2 <- "<0.00001"
    }
    
    if(show.sig.stars){
      cpoints <- c(0, alpha/50, alpha/5, alpha, ceiling(alpha*10^(ndec-1))/10^(ndec)+alpha, 1)
      pval2 <- paste(pval2, symnum(pval, corr = FALSE, na = FALSE, cutpoints = cpoints, symbols = c("***", "**", "*", ".", " ")))
    }
    return(pval2)
  }
  
  ret <- sapply(pval, function(x) arrodonir(x, show.sig.stars = show.sig.stars, alpha = alpha))
  if(is.matrix(pval)) ret <- matrix(ret, ncol = ncol(pval), dimnames = dimnames(pval))
  return(ret)
}

#' Windows path to R string
#' 
#' Reads the clipboard (with the copied windows path) and returns (and writes) in it a string to be pasted in an R environment.
#' 
#' @details
#' repath() has only two parameters. One is \code{write}, and the other is the windows path already copied in the clipboard. If 
#' the copied path does not exist, the working directory is returned with a warning. 
#' 
#' @param writecb Defaults to TRUE. If TRUE, the changed path to be read in R will be writted in the clipboard so that it can
#'  be pasted in R. Done via 
#' 
#' @return Returns a string with the new path adapted for R or the working directory if it did not exist.
#' 
#' @examples
#' \dontrun{
#' # copy an R path from Windows and do 
#' repath()
#' # now paste where you need the adapted path. Voila!
#' }
#' @export
repath <- function(writecb = TRUE) {
  path <- readClipboard()
  if(file.exists(path)){
    cpath <- gsub('\\\\', '/', path)
    if(writecb) writeClipboard(paste("'", paste(cpath, collapse = " "), "'", sep = ""))
    cpath <- paste(cpath, collapse = " ")
    return(cpath)
  } else {
    if(writecb)  writeClipboard(paste("'", getwd(), "'", sep = ""))
    warning("Path not valid. getwd() is returned.")
    return(getwd())
  }
}

#' Paste objects between brackets
#' 
#' The first element is placed outside brackets and the rest inside. In de form of \code{before (inside1, inside2, ...)}
#' 
#' @details
#' When \code{before} is a data frame, it returns a vector of strings where the first column from the data frame is outside the brackets
#' and all the others inside separated by the symbol given in \code{between}.
#' When \code{before} is a vector and \code{...} are also vectors of the same length, a data frame is created and the same that above is performed.
#' 
#' @param before Vector with the values written before the bracket starts. It can also be a data frame or matrix (see Details).
#' @param ... Other vectors to be displayed between brackets.
#' @param between Defaults to "-". Character vector with the strings to separate values inside the brackets. If it has length 1, it is replicated for all elements.
#' @param add Defaults to \code{""}. A character vector with strings to add after every value inside the brackets. If it has length 1, it is replicated for all elements.
#' @param rounding Defaults to getOption("rounding"). Number of decimals used for rounding. See \code{digits} from \code{\link[base]{round}}.
#' 
#' @return Returns a vector of strings with the values before and between the brackets. Has an attribute named \code{data.frame} with the values used.
#' 
#' @examples
#' mins <- apply(iris[1:4], 2, min)
#' maxs <- apply(iris[1:4], 2, max)
#' means <- apply(iris[1:4], 2, mean)
#' inbra(means, mins, maxs, between = " to ")
#' 
#' freqs <- as.numeric(table(iris$Species))
#' percents <- as.numeric(100*prop.table(table(iris$Species)))
#' inbra(freqs, percents, rounding = 2, add = "%")
#' 
#' # with a data frame:
#' values <- inbra(iris[c(5, 1:4)], between = c("; ", " - ", " - "), add = "cm")
#' values
#' 
#' # data arguments are still stored in
#' attributes(values)$data
#' 
#' @seealso \code{\link{bivarTable}}
#' @export
inbra <- function(before, ..., between = "-", add = "", rounding = getOption("rounding")){
  
  if(is.null(before) | missing(before)) stop("before argument must be set and non-NULL")
  extra <- list(...)
  before_class <- class(before)
  before <- as.data.frame(before)

  if(!inherits(before_class, what = c("data.frame", "matrix")) & length(extra) > 0){
    for(i in 1:length(extra)){
      before[[i + 1]] <- extra[[i]]
    }
    colnames(before)[-1] <- paste0("inside", 1:length(extra))
  }
  
  before2 <- before
  before <- as.data.frame(lapply(before, FUN = function(x){
    if(is.numeric(x)) return(round(x, rounding)) else return(x)
  }))
  
  if(length(add) == 1) add <- rep(add, ncol(before) - 1)
  if(ncol(before) > 2 & length(between) == 1) between <- rep(between, ncol(before) - 2)
  if(ncol(before) > 1){
    inside <- apply(before, 1, FUN = function(x){
      dins <- paste0(x[2], add[1])
      if(length(x[-(1:2)]) > 0){
        dins <- paste0(dins, paste0(between, x[-(1:2)], add[-1], collapse = ""))
      }
      dins
    })
    out <- paste0(before[, 1], " (", inside, ")")
  } else {
    out <- before[, 1]
  }
  attr(out, "data") <- before2
  return(out)
}

unAsIs <- function(x) {
  if("AsIs" %in% class(x)){
    class(x) <- class(x)[-match("AsIs", class(x))]
    if("logical"%in%class(x)) x <- factor(x, levels = c(FALSE, TRUE), labels = 0:1)
  }
  return(x)
}

#' Nagelkerke's pseudo r-squared
#' 
#' Calculating Nagelkerke's r-squared (or adjusted r-squared) coefficient from a model (glm object). 
#' Can be passed as functions to the argument FUN.model from \code{\link{bivarTable}}.
#' 
#' @param model glm object.
#' @param rounding Number of decimal places for the roundings.
#' 
#' @return Nagelkerke's r-squared or adjusted r-squared.
#' 
#' @examples
#' m <- glm(I(Sepal.Width > 3) ~ Species + Sepal.Length, iris, family = binomial)
#' nagelkerke(m, rounding = 2)
#' adjNagelkerke(m, rounding = 2)
#' 
#' @seealso \code{\link{getPval}}, \code{\link{GoF}}, \code{\link{getORCI}}, \code{\link{getBetaSd}},
#' \code{\link{verticalgetORCI}}, \code{\link{verticalgetBetaSd}}, \code{\link{verticalgetPval}}.
#' @export
nagelkerke <- function(model, rounding = getOption("rounding")){
  if(! "glm" %in% class(model)){
    warning("It's not a glm object")
    return("-")
  }
  n <- length(model$residuals)
  num <- 1 - exp((model$deviance - model$null.deviance) / n)
  den <- 1 - exp(-model$null.deviance / n)
  return(round(num / den, rounding))
}

#' @describeIn nagelkerke Adjusted Nagelkerke's pseudo r-squared.
#' @export
adjNagelkerke <- function(model, rounding = getOption("rounding")){
  if(! "glm"%in%class(model)){
    warning("it's not a glm object")
    return("-")
  }
  r2 <- nagelkerke(model, rounding = rounding)
  return(round(1-(1-r2)*(model$df.null)/(model$df.residual), rounding))
}

#' Goodness of Fit p-value
#' 
#' Assessing the fit of a model with Pearson's GoF statistic
#' 
#' @param model Model passed to \code{residuals(., "pearson")}.
#' 
#' @return Resulting p-value from the test. 
#' 
#' @examples
#' m <- glm(I(Sepal.Width > 3) ~ Species + Sepal.Length, iris, family = binomial)
#' GoF(m)
#' 
#' @export
#' @seealso \code{\link{getPval}}, \code{\link{nagelkerke}}, \code{\link{getORCI}}, \code{\link{getBetaSd}},
#' \code{\link{verticalgetORCI}}, \code{\link{verticalgetBetaSd}}, \code{\link{verticalgetPval}}.
GoF <- function(model){
  residus <- residuals(model, "pearson")
  pval.gf <- do.call(getOption("p.value.rounding")[[1]], c(list(pval = 1 - pchisq(sum(residus^2), length(residus) - length(model$coef))), getOption("p.value.rounding")[-1]))
  return(pval.gf)
}

#' Extracting P-values, Coefficients or OR (95\% CI) from a lm or glm object
#' 
#' Functions for fancy printing P-values, Coefficients or OR (95\% CI) from glm object. Prepared for \code{\link{bivarTable}} FUN.model.
#' 
#' @details
#' These functions are mainly thought to be used in the bivarTable FUN.model argument. The first argument has to be a model.
#' They all get some information from the model (OR, p-values, coefficients, etc.) and display them as a vector. 
#' The vertical version of the functions, return a one column matrix with the results: one value for each level of the variable.
#' Appends "Ref." or "1" to the reference level. In case of quantitative input variable, the output is the same
#' for the vertical and non-vertical versions. See examples.
#' 
#' @param model glm or lm object.
#' @param var Vector for extracting values from the summary(model)$coef matrix. Row 1 is for the intercept.
#' 
#' @return A vector with p-values, Beta's(SD) or OR (95\% CI)
#' @examples
#' m <- glm(I(Sepal.Width > 3) ~ Species + Sepal.Length, iris, family = binomial)
#' getPval(m, vars = -1) # p-values from everything but the intercept
#' getPval(m, vars = 2:3) # p-values from rows 2 and 3 of the matrix
#' getPval(m, vars = grep(pattern = "Species", x = names(m$coeff))) # or coefficients from Species
#' getORCI(m, vars = 2:3)
#' getBetaSd(m, vars = 2:3)
#' 
#' verticalgetBetaSd(m, vars = grep("Species", x = names(m$coeff)), reference = 1)
#' verticalgetPval(m, vars = grep("Species", x = names(m$coeff)), reference = 1)
#' verticalgetORCI(m, vars = grep("Species", x = names(m$coeff)), reference = 1)
#' 
#' bivarTable(I(Sepal.Width > 3) ~ . - Sepal.Length - Sepal.Width, data = iris,  
#'            fit.model = list(adjusted = ~ . + Sepal.Length), # model to fit for every variable in rhs of the formula.
#'            outcome = 2, # the output of the models is the variable in the column (lhs of formula)
#'            FUN.model = list(adjusted = c("verticalgetBetaSd", "verticalgetORCI", "verticalgetPval"))) # add columns with extra info for the adjusted model.
#' 
#' @seealso \code{\link{GoF}}, \code{\link{nagelkerke}}
#' @export
getPval <- function(model, vars = 2){
  sm <- summary(model)
  pval <- sm$coef[vars, 4, drop = FALSE]
  out <- do.call(getOption("p.value.rounding")[[1]], c(list(pval=pval), getOption("p.value.rounding")[-1]))
  out <- matrix(out, nrow = 1)
  return(out)
}

#' @describeIn getPval Get the coefficients of the model and SD from lm or glm.
#' @export
getBetaSd <- function(model, vars = 2){
  sm <- summary(model)
  betasd <- sm$coef[vars, 1:2, drop = FALSE]
  out <- inbra(betasd)
  out <- matrix(out, nrow = 1)
  return(out)
}

#' @describeIn getPval For glm models, get the OR and 95\% CI. 
#' @param between String for collapsing lower and upper limits of the CI.
#' @export
getORCI <- function(model, vars = 2, between = ";"){
  if("glm" %in% class(model)){
    suppressMessages(ci <- exp(confint(object = model)[vars, , drop=F]))
    out <- inbra(before = cbind(exp(model$coef[vars]), ci), between = between)
    out <- matrix(out, nrow = 1)
    return(out)
  } else {return(rep("-", length(vars)))}
}

#' @describeIn getPval Vertical version of getPval. Used in bivarTable.
#' @param reference Which value is the refference level in case of categorical variable?.
#' @export
verticalgetPval <- function(model, vars = grep(pattern = "invar", x = names(model$coef)), reference = 1){
  sm <- summary(model)
  pval <- sm$coef[vars, 4, drop = FALSE]
  out <- do.call(getOption("p.value.rounding")[[1]], c(list(pval=pval), getOption("p.value.rounding")[-1]))
  if(! (is.numeric(model$model[[2]]) & length(vars)==1)){
    out <- matrix(append(c("", out), "Ref.", reference), ncol = 1)
  }
  return(out)
}

#' @describeIn getPval Vertical version of getBetaSd. Used in bivarTable.
#' @export
verticalgetBetaSd <- function(model, vars = grep(pattern = "invar", x = names(model$coef)), reference = 1){
  sm <- summary(model)
  betasd <- sm$coef[vars, 1:2, drop = FALSE]
  out <- inbra(betasd)
  if(! (is.numeric(model$model[[2]]) & length(vars)==1)){
    out <- matrix(append(c("", out), "Ref.", reference), ncol = 1)
  }
  return(out)
}

#' @describeIn getPval Vertical version of getORCI. Used in bivarTable.
#' @export
verticalgetORCI <- function(model, vars = grep(pattern = "invar", x = names(model$coef)), between = ";", reference = 1){
  if("glm" %in% class(model)){
    suppressMessages(ci <- exp(confint(object = model)[vars, , drop=F]))
    out <- inbra(before = cbind(exp(model$coef[vars]), ci), between = between)
    if(! (is.numeric(model$model[[2]]) & length(vars)==1)){
      out <- matrix(append(c("", out), "1", reference), ncol = 1)
    }
    return(out)
  } else {return("-")}
}

#' @export
check_if <- function(data, condition, select, drop = FALSE, showNA = FALSE, ...){
  rows <- ! eval(substitute(condition), data, parent.frame())
  cols <- if(missing(select)){
    names(data) %in% c(names(data)[1], all.vars(substitute(condition)))
  } else {
    nl <- as.list(seq_along(data))
    names(nl) <- names(data)
    eval(substitute(select), nl, parent.frame())
  }
  subdf <- if(!showNA) data[rows & !is.na(rows), cols, drop = drop] else data[rows | is.na(rows), cols, drop = drop]
  if(nrow(subdf) == 0){
    message("ALL GOOD! HOLDS TRUE FOR EVERYONE.")
    return(invisible(1))
  } else {
    message("CONDITION DOES NOT HOLD TRUE FOR: \n")
    return(subdf)
  }
}

#' @export
descomp <- function(x, get.all = FALSE, rev = FALSE, sum = x > 3) {
  stopifnot(x != 0)
  dividir <- 1:x
  factors <- dividir[x %% dividir == 0]
  if(get.all)  return(factors)
  mindif <- factors[which.min(abs(factors - rev(factors)))]
  if(mindif == 1 & sum){
    message("descomp(", x, ") was 1 and ", x, ", so calculating descomp(", x + 1, ").")
    return(descomp(x + 1, sum = sum, rev = rev))
  }
  out <- c(mindif, x/mindif)
  if(rev) out <- rev(out)
  return(out)
}

#' @export
survTable <- function(formula, data, when = c(1,2,5)){
  mcox <- coxph(formula = formula, data = data)
  df.km <- summary(survfit(formula, data))
  df.km <- with(df.km, data.frame(time, surv, strata, lower, upper))
  sum.mcox <- summary(mcox)  
  
  covar <- rev(all.vars(formula))[1]
  taula <- matrix("", ncol = 5, nrow = 1 + nrow(sum.mcox$coef))
  taula[,1] <- c("1", inbra(sum.mcox$conf[,1], sum.mcox$conf[,3], sum.mcox$conf[,4]))
  taula[,2] <- c("Ref.", pvalue(sum.mcox$coeff[,5]))
  taula[,3:5] <- t(sapply(levels(data[[covar]]), function(x){
    quins <- df.km$strata %in% (levels(df.km$strata)[levels(data[[covar]]) %in% x])
    temps <- findInterval(when, subset(df.km, quins)$time)
    ifelse(temps == 0, "100 (-)", 
           inbra(100*subset(df.km, quins)$surv[temps],
                 100*subset(df.km, quins)$lower[temps],
                 100*subset(df.km, quins)$upper[temps]))
  }))
  rownames(taula) <- levels(data[[covar]])
  colnames(taula) <- c("HR (95% CI)", "p-value", paste0("Surv ", when, " year"))
  taula
}

#' @export
toFactor <- function(data, select, levels = list(), labels = list()){
  
  nl <- as.list(seq_along(along.with = data))
  names(nl) <- names(data)
  select <- eval(substitute(select), nl, parent.frame())
  for(i in names(data[select])){
    levelsi <- if(sum(names(levels) %in% i) == 1) levels[[i]] else as.character(na.omit(unique(data[[i]])))
    labelsi <- if(sum(names(labels) %in% i) == 1) labels[[i]] else levelsi
    data[[i]] <- factor(data[[i]], levels = levelsi, labels = labelsi)
  }
  return(data)
}

#' @export
toNumeric <- function(data, select){
  
  nl <- as.list(seq_along(along.with = data))
  names(nl) <- names(data)
  select <- eval(substitute(select), nl, parent.frame())
  for(i in names(data[select])){
    data[[i]] <- as.numeric(data[[i]])
  }
  return(data)
}

#' @export
descr_var <- function(x, ...) UseMethod("descr_var")
descr_var.default <- function(x, y, ...){
  message("Unhandled class for ", deparse(substitute(x)))
  return("-")
}
descr_var.numeric <- function(x, y = NULL, rounding = getOption("rounding"), show.quantiles = NULL, ...){
  
  mitjanes <- ifelse(is.na(mean(x, na.rm = T)), 
                     "-", 
                     round(mean(x, na.rm = T), rounding))
  desvsd <- ifelse(is.na(sd(x, na.rm = T)), 
                   "-", 
                   round(sd(x, na.rm = T), rounding))
  quants <- NULL
  if(!is.null(show.quantiles)){
    quants <- ifelse(is.na(quantile(x, probs = show.quantiles, na.rm = TRUE)), 
                     "-", 
                     round(quantile(x, probs = show.quantiles, na.rm = TRUE), rounding))
    quants <- paste0(quants, collapse = " - ")
    out <- inbra(mitjanes, desvsd, quants, between = "; ", rounding = rounding)
  } else {
    out <- inbra(mitjanes, desvsd, rounding = rounding)
  }
  if(!is.null(y)){
    mitjanes <- ifelse(is.na(tapply(x, y, mean, na.rm = TRUE)), 
                       "-", 
                       round(tapply(x, y, mean, na.rm = TRUE), rounding))
    desvsd <- ifelse(is.na(tapply(x, y, sd, na.rm = TRUE)), 
                     "-", 
                     round(tapply(x, y, sd, na.rm = TRUE), rounding))
    if(!is.null(show.quantiles)){
      quants <- ifelse(is.na(tapply(x, y, quantile, probs = show.quantiles, na.rm = TRUE)), 
                       "-", 
                       lapply(tapply(x, y, quantile, probs = show.quantiles, na.rm = TRUE), round, rounding)) 
      quants <- sapply(quants, paste0, collapse = " - ")
      out <- inbra(mitjanes, desvsd, quants, between = "; ", rounding = rounding)
    } else {
      out <- inbra(mitjanes, desvsd, between = "; ", rounding = rounding)
    }
  }
  out <- t(out)
  colnames(out) <- levels(y)
  out
}
descr_var.factor <- function(x, y = NULL, rounding = getOption("rounding"), margin = 2, ...){
  
  freqs <- as.vector(table(x))
  percents <- as.vector(prop.table(table(x)) * 100)
  out <- as.matrix(inbra(freqs, percents, add = "%", rounding = rounding))

  if(!is.null(y)){
    freqs <- table(x, y)
    n_row <- nrow(freqs) 
    if(length(margin) != 2){
      percents <- prop.table(table(x, y), margin = margin) * 100
      df <- data.frame(as.vector(freqs), as.vector(percents))
    } else {
      percents1 <- prop.table(table(x, y), margin = margin[1]) * 100
      percents2 <- prop.table(table(x, y), margin = margin[2]) * 100
      df <- data.frame(as.vector(freqs), as.vector(percents1), as.vector(percents2))
    }
    
    out <- matrix(inbra(df, add = "%", between = ", ", rounding = rounding), nrow = n_row)
    colnames(out) <- levels(y)
  }
  
  rownames(out) <- levels(x)
  out
}

#' @export
alt_bind <- function(x, ...) UseMethod("alt_bind")

#' @export
alt_bind.default <- function(..., alternate = 2, sequence){
  dots <- list(...)
  if(! do.call(all, lapply(dots, is.matrix))) stop("All arguments in ... must be matrices")
  if(missing(sequence)) sequence <- rep(1, length(dots))
  if(length(sequence) != length(dots)) stop("sequence and ... must have same length")
  
  if(alternate == 2){
    if(1 != sum(! duplicated(sapply(dots, nrow)))) stop("Rows do not match.")
    if(1 != sum(! duplicated(mapply(SIMPLIFY = FALSE, FUN = function(x, y) ncol(x)/y, dots, sequence)))) stop("Rows do not match using this sequence.")
    
    dim_names <- dimnames(dots[[1]])
    args_list <- do.call(c, mapply(SIMPLIFY = FALSE, FUN = function(x, y){
      if(x > 1){
        lapply(c(1:(x - 1), 0), function(k){
          y[, 1:ncol(y) %% x == k, drop = FALSE]
        })
      } else if(x == 1){
        list(y)
      } else stop("Invalid sequence.")
    }, sequence, dots))
    
    pre_out <- do.call(rbind, args = args_list)
    out <- matrix(pre_out, nrow = nrow(dots[[1]]))
    if(length(dim_names) > 1){
      if(!is.null(dim_names[[1]])) rownames(out) <- rownames(dots[[1]])
      if(!any(sapply(lapply(dots, colnames), is.null))){
        colnames(out) <- as.vector(do.call(alt_bind, 
                                         c(lapply(lapply(dots, colnames), t), 
                                           list(alternate = alternate, 
                                                sequence = sequence))))
      }
    }
    return(out)
  } else if(alternate == 1){
    if(1 != sum(! duplicated(sapply(dots, ncol)))) stop("Columns do not match.")
    if(1 != sum(! duplicated(mapply(SIMPLIFY = FALSE, FUN = function(x, y) nrow(x)/y, dots, sequence)))) stop("Rows do not match using this sequence.")
    
    return(t(do.call(alt_bind, c(lapply(dots, t), list(alternate = 2, sequence = sequence)))))
  } else {
    stop("alternate argument must be either 1 or 2 for alternating rows or columns respectively.")
  }
}

#' @export
alt_bind.bivarTable <- function(..., sequence, label.first.row = FALSE){
  dots <- list(...)
  names_dots <- names(dots)
  if(missing(sequence)) sequence <- rep(1, length(dots))
  taula <- do.call(alt_bind, c(lapply(dots, getElement, name = "table"), list(sequence = sequence, alternate = 1)))
  names_taula <- rownames(taula)
  if(!is.null(names_dots)){
    rownames(taula) <- rep(rep(names_dots, times = sequence), length.out = nrow(taula))
  }
  if(!label.first.row){
    row_labels <- dots[[1]]$table
    row_labels[TRUE, TRUE] <- ""
    taula_nova <- alt_bind(row_labels, taula, sequence = c(1, length(dots)), alternate = 1)
  } else {
    taula_nova <- taula
    rownames(taula_nova)[1:nrow(taula_nova) %% length(dots) == 1] <- rownames(dots[[1]]$table)
  }
  
  out <- dots[[1]]
  out$table <- taula_nova
  class(out) <- c("bivarTable", class(out))
  return(out)
}


# require("multcomp")
# compareLevels <- function(model, linfct, ...){
#   groups <- model$model[[2]]
#   comp <- combn(levels(groups), 2, FUN = paste0, collapse = ":")
#   if(missing(linfct)){
#     compMAT <- t(combn(seq_along(levels(groups)), 2, FUN = function(x){
#       vect <- numeric(length(levels(groups)))
#       vect[x] <- c(1, -1)
#       vect
#     }))
#     rownames(compMAT) <- comp
#     colnames(compMAT) <- levels(groups)
#   }
#   glht(model, linfct = compMAT)
# }