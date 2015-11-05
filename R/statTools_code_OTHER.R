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
#' After the first element, puts everything sorrounded by brackets.
#' 
#' @details
#' When \code{before} is a data frame, it returns a vector of strings where the first column from the data frame is outside the brackets
#' and all the others inside separated by the symbol given in \code{between}.
#' When \code{before} is a vector and \code{...} are also vectors of the same length, a data frame is created and the same that above is performed.
#' 
#' @param before Defaults to NULL. Vector with the values written before the bracket starts. It can also be a data frame (see Details).
#' @param ... Other vectors to be displayed between brackets.
#' @param between Defaults to "-". String to separate values inside the brackets. 
#' @param add Defaults to NULL. A vector of the strings to add in every term inside the brackets. If it has length 1, it is replicated for all elements.
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
#' @seealso \code{\link{bivarTable}}
#' @export
inbra <- function(before = NULL, ..., between = "-", add = NULL, rounding = getOption("rounding")){
  extra <- list(...)
  before2 <- before
  if(!is.null(before)) before <- as.data.frame(before)

  if(!(is.data.frame(before2) | is.matrix(before2)) & length(extra)>0){
    for(i in 1:length(extra)){
      before <- cbind(before, extra[[i]])
    }
  }
  dins <- paste0(if(is.numeric(before[, 2])) round(before[, 2], rounding) else before[, 2], add[1])
  if(ncol(before) > 2){
    for(i in 3:ncol(before)){
      dins <- paste(dins, paste0(if(is.numeric(before[, i])) round(before[, i], rounding) else before[, i], if(length(add) > 1) add[i - 1] else add), sep = between)
    }
  }
  out <- paste(if(is.numeric(before[, 1])) round(before[, 1], rounding) else before[, 1], " (", dins,")", sep = "")
  attr(out, "data.frame") <- before
  return(out)
}

#' @export
unAsIs <- function(x) {
  if("AsIs" %in% class(x)){
    class(x) <- class(x)[-match("AsIs", class(x))]
    if("logical"%in%class(x)) x <- factor(x, levels = c(FALSE, TRUE), labels = 0:1)
  }
  return(x)
}

#' @export
nagelkerke <- function(model, rounding = getOption("rounding")){
  if(! "glm"%in%class(model)){
    warning("it's not a glm object")
    return("-")
  }
  n <- length(model$residuals)
  num <- 1-exp((model$deviance - model$null.deviance)/n)
  den <- 1-exp(- model$null.deviance/n)
  return(round(num/den, rounding))
}

#' @export
adjNagelkerke <- function(model, rounding = getOption("rounding")){
  if(! "glm"%in%class(model)){
    warning("it's not a glm object")
    return("-")
  }
  r2 <- nagelkerke(model, rounding = rounding)
  return(round(1-(1-r2)*(model$df.null)/(model$df.residual), rounding))
}

#' @export
GoF <- function(model){
  residus <- residuals(model, "pearson")
  pval.gf <- do.call(getOption("p.value.rounding")[[1]], c(list(pval = 1 - pchisq(sum(residus^2), length(residus) - length(model$coef))), getOption("p.value.rounding")[-1]))
  return(pval.gf)
}

#' @export
getPval <- function(model, vars = 2){
  sm <- summary(model)
  pval <- sm$coef[vars, 4, drop = FALSE]
  out <- do.call(getOption("p.value.rounding")[[1]], c(list(pval=pval), getOption("p.value.rounding")[-1]))
  out <- matrix(out, nrow = 1)
  return(out)
}

#' @export
getBetaSd <- function(model, vars = 2){
  sm <- summary(model)
  betasd <- sm$coef[vars, 1:2, drop = FALSE]
  out <- inbra(betasd)
  out <- matrix(out, nrow = 1)
  return(out)
}

#' @export
getORCI <- function(model, vars = 2, between = ";"){
  if("glm" %in% class(model)){
    suppressMessages(ci <- exp(confint(object = model)[vars, , drop=F]))
    out <- inbra(before = cbind(exp(model$coef[vars]), ci), between = between)
    out <- matrix(out, nrow = 1)
    return(out)
  } else {return(rep("-", length(vars)))}
}

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
checkIf <- function(data, condition, select, drop = FALSE, showNA = FALSE, ...){
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
addNAlevel <- function(vector, toFactor = TRUE, after = length(factor(vector))){
  if(is.character(vector) | is.factor(vector)){
    vector2 <- ifelse(is.na(vector), "NA", as.character(vector))
    if(toFactor) vector2 <- factor(vector2, levels = append(levels(vector), values = "NA", after = after))
  }
  return(vector2)
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
    return(descomp(x + 1))
  }
  out <- c(mindif, x/mindif)
  if(rev) out <- rev(out)
  return(out)
}

# compareLevels <- function(model, linfct, ...){
#   require("multcomp")
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