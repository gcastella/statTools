# bivarTable HELP! --------------------------------------------------------------------------------------------------

# CALL:          bivarTable(X, 
#                           data = NULL,
#                           margin = getOption("margin"),
#                           rounding = getOption("rounding"),
#                           test = c("both", "parametric", "non-parametric", "none"),
#                           condense.binary.factors = getOption("condense.binary.factors"),
#                           drop.x = getOption("drop.x"), 
#                           drop.y = getOption("drop.y"),
#                           fit.model = NULL,
#                           outcome = getOption("outcome"),
#                           FUN.model = NULL, 
#                           ...)
#
# ARGUMENTS: 
#
# X                         F?rmula nom?s amb termes additius. Accepta AsIs class, punt (.) i restes.
#                           La part esquerra defineix les columnes (variable amb els grups), la dreta 
#                           les files (on veure difer?ncies entre grups).
# data                      D'on llegir les dades per interpretar les f?rmules.
# margin                    Valors 1 o 2. Perfils fila o columna a les taules de conting?ncia, 
#                           respectivament. 
# rounding                  Nombre de decimals a tenir en compte (els p-valors s'arrodoneixen amb un 
#                           altre criteri).
# test                      One of "both", "parametric", "non-parametric" or "none". Si volem que fagi uns 
#                           testos predeterminats en concret (veure options()$parametric.tests i 
#                           options()$non.parametric.tests).
# condense.binary.factors   Si transformar en una sola l?nia les variables bin?ries.
# drop.x                    Refactoritzar les variables fila si s?n factors per treure nivells sense observacions.            
# drop.y                    Refactoritzar la variable columna (grups) per treure nivells sense observacions. 
# fit.model                 Llista amb noms de les f?rmules a partir de les quals ajustar un model glm o lm
#                           segons s'escaigui. L'?s del punt (.) s'utilitza per referir-se de forma general 
#                           a la variable fila o columna sense haver d'especificar-ne el nom.
# outcome                   Valors 1 o 2, segons si pels models la variable considerada outcome seran les 
#                           que estan a les files o a les columnes, respectivament. 
# FUN.model                 Funcions extres que s'aplicaran als models, els outputs de les quals s'afegiran 
#                           com a noves columnes a la taula resultant. El primer par?metre de les quals 
#                           ha de ser el model. S'especifica com una llista on cada element s'anomena com un
#                           dels models especificats a fit.model i ser? un vector amb els noms de les funcions
#                           (entrats com a string) a aplicar en cadascun dels respectius models. Funcions 
#                           preparades: nagelkerke(), adjNagelkerke(), getPval(), getBetaSd(), getORCI(),
#                           verticalgetPval(), verticalgetBetaSd(), verticalgetORCI(), GoF().
# ...                       Arguments extres per les funcions del par?metre FUN.model. Per a especificar els
#                           arguments extres d'una funci? que s'aplica a un model, s'afegeix dintre dels ...
#                           un  argument amb el nom del model corresponent, i que ser? una llista on cada element
#                           tingui per nom la funci? de la qual es volen canviar els par?metres, i contingui una 
#                           altra llista amb els par?metres modificats (tipus el par?metre args de do.call()).
#
# OUTPUT:
#
# A list with class bivarTable, where the first argument is the table generated (a matrix) and the other arguments
# are input arguments returned as outputs in the list, such as margin, outcome, fit.model, FUN.model, test, drop.x,
# drop.y, condense.binary.factors, data.frame with the values from variables in the rows (named X), and the same for
# the columns (named y), and extra arguments in ..., with maybe, the arguments for calling the functions in FUN.model.
# 
# The function export(bivarTable output) opens an xlsx file created as a tempfile in the computer with a fancy table
# created via bivarTable function. ONLY WORKING WITH TEST="BOTH" RIGHT NOW (08/09/2015).

# bivarTable EXAMPLES -----------------------------------------------------------------------------------------------

# ##### cntrl + shift + c TO COMMENT/UNCOMMENT SELECTED CODE WITH RSTUDIO
#
# iris$prova <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 2)
# iris$prova2 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 3)
# iris$prova3 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 5)
# bvt <- bivarTable(X = Species ~ . + I(Sepal.Width>3) - Sepal.Length, 
#                   data = iris, 
#                   margin = 2, 
#                   outcome = 1, 
#                   drop.x = TRUE, 
#                   drop.y = TRUE, 
#                   test = "non-par", 
#                   condense.binary.factors = TRUE, 
#                   rounding = 3,
#                   fit.model = list(simple = ~ .,
#                                    adj = ~ . + Sepal.Length), 
#                   FUN.model = list(simple = c("getPval"), 
#                                    adj = c("getORCI", "getPval")),
#                   adj = list(getPval = list(vars = 4), 
#                              getORCI = list(vars = 4)))
# bvt
# bvt <- bivarTable(X = I(Sepal.Width>3) ~ . - Sepal.Length,
#                   data = iris, 
#                   margin = 2, 
#                   rounding = 3,
#                   test = "both", 
#                   condense.binary.factors = FALSE, 
#                   drop.x = TRUE, 
#                   drop.y = TRUE, 
#                   fit.model = list(simple = ~ ., 
#                                    adj = ~ . + Sepal.Length), 
#                   outcome = 2, 
#                   FUN.model = list(simple = c("getPval"), 
#                                    adj = c("verticalgetORCI", 
#                                            "verticalgetPval")))
# bvt
# export(bvt)


# SET NEW DEFAULT OPTIONS -------------------------------------------------

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


# GENERAL FUNCTIONS -------------------------------------------------------

#' @title
#' Rounding criterion for p-values
#' 
#' @description
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
#' pvalue(c(0.684736, 0.06543, 0.05146673, 0.05042, 0.049831, 0.04395, 0.0232, 0.0266, 0.0001235, 0.000000124527))
#' pvalue(c(0.684736, 0.06543, 0.05146673, 0.05042, 0.049831, 0.04395, 0.0232, 0.0066, 0.0001, 0.0001235, 0.000000124527), show.sig.stars = TRUE)
#' pvalue(c(0.684736, 0.16543, 0.10146673, 0.10042, 0.09949831, 0.0904395, 0.0910232, 0.090266, 0.04001235, 0.0002546, 0.000000124527), alpha = 0.1)
#' \dontrun{
#' # Use stars as a default option when calling pvalue() from another function such as bivarTable().
#' options("p.value.rounding" = list("pvalue", show.sig.stars = TRUE))
#' }
#' @seealso \code{\link{bivarTable}}
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

#' @title
#' Windows path to R string
#' 
#' @description
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

#' @title
#' Paste objects between brackets
#' 
#' @description
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
#' @param rounding Defaults to getOption("rounding"). Number of decimals used for rounding. See \code{digits} from \code{\link{round}}.
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
unAsIs <- function(x) {
  if("AsIs" %in% class(x)){
    class(x) <- class(x)[-match("AsIs", class(x))]
    if("logical"%in%class(x)) x <- factor(x, levels = c(FALSE, TRUE), labels = 0:1)
  }
  return(x)
}


aggregate2 <- function(formula , data = NULL, keep = NULL, na.action = na.pass, FUN = mean, ...){
  agg <- aggregate(formula, data, FUN = FUN, ..., simplify = T, na.action = na.action)
  if(!is.null(keep)){ 
    formula2 <- update(formula, as.formula(paste("cbind(", paste(keep, collapse = ", "), ")~.", sep = "")))
    agg2 <- aggregate(formula2, data = data, FUN = unique, simplify = T, na.action = na.action)
    agg <- cbind(agg2[(ncol(agg2) - length(keep) + 1):ncol(agg2)], agg)
  }
  return(agg)
}
bootstrap <- function(data, FUN, B = 100, which.indices = "indices", ...){
  argsfun <- c(list(data = data), list(...))
  replicate(B, expr = {
    argsfun[[which.indices]] <- sample(1:nrow(data), replace = T)
    do.call(FUN, args = argsfun)
  })
}
# va mal ...
gatherList <- function(dataList, values = NULL, useNames = FALSE, select = NULL, ...){
  
  if(!is.null(select)){
    dataList <- sapply(dataList, function(x){
      quins <- select
      if(is.character(select)) quins <- which(names(x) %in% select)
      subset(x, select = quins) 
    }, simplify = FALSE)
  }
  if(useNames){
    sameNames <- sapply(sapply(dataList, names, simplify = FALSE), sort)
    sameNames <- all(sapply(sapply(sameNames, unique), length) == 1)
    stopifnot(sameNames)
    orderNames <- names(dataList[[1]])
  }
  
  out <- dataList[[1]]
  if(useNames) out <- out[orderNames]      
  for(i in 2:length(dataList)){
    if(useNames){
      out <- rbind(out, dataList[[i]][orderNames])      
    } else {
      out <- rbind(out, dataList[[i]])
    }
  }
  
  if(!is.null(values) & !is.null(names(dataList))){
    aux2 <- data.frame(rep(names(dataList), sapply(dataList, nrow)))
    colnames(aux2) <- values
    out <- cbind(out, aux2)
  }
  
  return(out)
}

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
adjNagelkerke <- function(model, rounding = getOption("rounding")){
  if(! "glm"%in%class(model)){
    warning("it's not a glm object")
    return("-")
  }
  r2 <- nagelkerke(model, rounding = rounding)
  return(round(1-(1-r2)*(model$df.null)/(model$df.residual), rounding))
}
GoF <- function(model){
  residus <- residuals(model, "pearson")
  pval.gf <- do.call(getOption("p.value.rounding")[[1]], c(list(pval = 1 - pchisq(sum(residus^2), length(residus) - length(model$coef))), getOption("p.value.rounding")[-1]))
  return(pval.gf)
}
getPval <- function(model, vars = 2){
  sm <- summary(model)
  pval <- sm$coef[vars, 4, drop = FALSE]
  out <- do.call(getOption("p.value.rounding")[[1]], c(list(pval=pval), getOption("p.value.rounding")[-1]))
  out <- matrix(out, nrow = 1)
  return(out)
}
getBetaSd <- function(model, vars = 2){
  sm <- summary(model)
  betasd <- sm$coef[vars, 1:2, drop = FALSE]
  out <- inbra(betasd)
  out <- matrix(out, nrow = 1)
  return(out)
}
getORCI <- function(model, vars = 2, between = ";"){
  if("glm" %in% class(model)){
    suppressMessages(ci <- exp(confint(object = model)[vars, , drop=F]))
    out <- inbra(before = cbind(exp(model$coef[vars]), ci), between = between)
    out <- matrix(out, nrow = 1)
    return(out)
  } else {return(rep("-", length(vars)))}
}
verticalgetPval <- function(model, vars = grep(pattern = "invar", x = names(model$coef)), reference = 1){
  sm <- summary(model)
  pval <- sm$coef[vars, 4, drop = FALSE]
  out <- do.call(getOption("p.value.rounding")[[1]], c(list(pval=pval), getOption("p.value.rounding")[-1]))
  if(! (is.numeric(model$model[[2]]) & length(vars)==1)){
    out <- matrix(append(c("", out), "Ref.", reference), ncol = 1)
  }
  return(out)
}
verticalgetBetaSd <- function(model, vars = grep(pattern = "invar", x = names(model$coef)), reference = 1){
  sm <- summary(model)
  betasd <- sm$coef[vars, 1:2, drop = FALSE]
  out <- inbra(betasd)
  if(! (is.numeric(model$model[[2]]) & length(vars)==1)){
    out <- matrix(append(c("", out), "Ref.", reference), ncol = 1)
  }
  return(out)
}
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
addNAlevel <- function(vector, toFactor = TRUE, after = length(factor(vector))){
  if(is.character(vector) | is.factor(vector)){
    vector2 <- ifelse(is.na(vector), "NA", as.character(vector))
    if(toFactor) vector2 <- factor(vector2, levels = append(levels(vector), values = "NA", after = after))
  }
  return(vector2)
}
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
HRrow <- function(formula, data, when = c(1,2,5)){
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
toNumeric <- function(data, select){
  
  nl <- as.list(seq_along(along.with = data))
  names(nl) <- names(data)
  select <- eval(substitute(select), nl, parent.frame())
  for(i in names(data[select])){
    data[[i]] <- as.numeric(data[[i]])
  }
  return(data)
}


# PLOT FUNCTIONS ----------------------------------------------------------

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
barplot2 <- function(formula, data = NULL, factor.sd = 1, margin = 1, beside = TRUE, freq = FALSE, mean.par = NULL, sd.par = NULL, xlab = NULL, ylab = NULL, ...){
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


# BIVARTABLE FAMILY -------------------------------------------------------

bivarRow <- function(x, ...) UseMethod("bivarRow", object = if(is.data.frame(x)) x[[1]] else x)
bivarRow.default <- function(x, y, ...){
  stop("Only factor and numeric classes are supported")
}
bivarRow.factor <- function(x, y = NULL, data = NULL,
                            margin = getOption("margin"),
                            rounding = getOption("rounding"),
                            test = c("both", "parametric", "non-parametric", "none"),
                            condense.binary.factors = getOption("condense.binary.factors"),
                            drop.x = getOption("drop.x"), 
                            drop.y = getOption("drop.y"),
                            fit.model = NULL, 
                            outcome = getOption("outcome"),
                            FUN.model = NULL, ...){
  
  xname <- deparse(substitute(x))
  if(is.data.frame(x)){
    xname <- names(x)
    x <- x[[1]]
  } 
  if(drop.x) x <- factor(x)
  if(drop.y & !is.null(y)) y <- factor(y)
  lly <- length(levels(y))
  llx <- length(levels(x))
  
  tx <- as.vector(table(x))
  px <- as.vector(prop.table(table(x))*100)
  dots <- list(...)
  
  totals.n <- matrix(c("", inbra(tx, ifelse(is.na(px), "-", paste0(round(px, rounding), "%")))), ncol=1)
  rownames(totals.n) <- c(xname, levels(x))
  colnames(totals.n) <- paste0("Totals", " (n = ", sum(!is.na(y)),"; ", round(sum(!is.na(y))/length(y)*100, rounding), "%)")
  
  output <- totals.n
    
  if(!is.null(y)){
    n.ij <- table(x, y)
    percent.ij <- round(prop.table(n.ij, margin)*100, rounding)
    desc.ij <- rbind(rep("", lly), matrix(paste(n.ij, paste0("(", ifelse(is.na(percent.ij), "-", paste0(round(percent.ij, rounding), "%")),")")), nrow = llx))
    perc.desc <- round(prop.table(table(y)) * 100, rounding)
    colnames(desc.ij) <- paste0(levels(y), " (n = ", table(y),"; ", perc.desc, "%)")
    output <- cbind(output, desc.ij)
    
    test <- match.arg(test)
    nonparam <- getOption("non.parametric.tests")
    param <- getOption("parametric.tests")
    test.pvals <- tryCatch(switch(test, 
                                  "non-parametric" = c("non-parametric" = do.call(nonparam$factor[[1]], c(list(table(x, y)), nonparam$factor[-1]))$p.value),
                                  "parametric"     = c("parametric p-value" = do.call(param$factor[[1]], c(list(table(x, y)), param$factor[-1]))$p.value),
                                  "both"           = c("non-parametric p-value" = do.call(nonparam$factor[[1]], c(list(table(x, y)), nonparam$factor[-1]))$p.value, 
                                                      "parametric p-value" = do.call(param$factor[[1]], c(list(table(x, y)), param$factor[-1]))$p.value),
                                  "none"           = NA), 
                          error = function(e){
                            warning(paste0("Error for variable ", xname, ": ", as.character(e)))
                            if(test == "both"){
                              return(c(NA, NA))
                            } else {
                              return(NA)
                            }
                          }
    )
    pround <- getOption("p.value.rounding")
    test.pvals <- do.call(pround[[1]], c(list(test.pvals), pround[-1]))
    test.pvalsmat <- matrix(rep("", length(test.pvals)*nrow(output)), ncol = length(test.pvals), nrow = nrow(output))
    test.pvalsmat[1,] <- test.pvals
    colnames(test.pvalsmat) <- names(test.pvals)
    if(test == "none") test.pvalsmat <- NULL
    output <- cbind(output, test.pvalsmat)
    
    if(!is.null(fit.model)){ 
      pmod <- NULL
      mod <- list()
      
      if(outcome == 1){
        outvar <- if(llx > 2) as.numeric(x) else x
        invar <- y
      } else {
        outvar <- if(lly > 2) as.numeric(y) else y
        invar <- x
      }
      
      llinvar <- length(levels(invar))
      lloutvar <- length(levels(outvar))
      
      for(i in 1:length(fit.model)){
        if(lloutvar == 2){
          mod[[names(fit.model)[i]]] <- glm(outvar ~ invar, family = binomial, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(llinvar > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else if(lloutvar == 0){
          mod[[names(fit.model)[i]]] <- lm(outvar ~ invar, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(llinvar > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else {warning(paste(deparse(substitute(x)), "has only one level"))}
        
        names(pmod)[i] <- names(fit.model)[i]
        pm <- as.matrix(c(pmod[i], rep("", llx)))
        colnames(pm) <- paste(names(fit.model)[i], "p-value")
        output <- cbind(output, pm)
        
        if(!is.null(FUN.model[[names(fit.model)[i]]])){
          funs.mat <- NULL
          funcions <- FUN.model[[names(fit.model)[i]]]
          args.funcions <- dots[[names(fit.model)[i]]]
          for(j in 1:length(FUN.model[[names(fit.model)[i]]])){
            sortida <- do.call(funcions[j], c(list(mod[[i]]), args.funcions[[funcions[j]]]))
            if(is.vector(sortida)) sortida <- t(as.matrix(sortida))
            if(nrow(output) - nrow(sortida) < 1){
              # cutrada...
              sortida <- sortida[1:nrow(output), , drop = FALSE]
              auxmat <- NULL
            } else {
              auxmat <- matrix("", ncol = ncol(sortida), nrow = nrow(output) - nrow(sortida))
            }
            sortida <- rbind(sortida, auxmat)
            colnames(sortida) <- paste0(funcions[j], 1:ncol(sortida), " ", names(fit.model)[i])
            funs.mat <- cbind(funs.mat, sortida)
          }
          output <- cbind(output, funs.mat)
        }
      }
    }
  }
  if(condense.binary.factors & llx==2){
    rownames(output)[1] <- inbra(rownames(output)[1], rownames(output)[3])
    output[1, 1:(lly + 1)] <- output[3, 1:(lly + 1)]
    output <- output[1, , drop = FALSE]
  }
  return(output)
}
bivarRow.numeric <- function(x, y = NULL, data = NULL,
                             margin = getOption("margin"),
                             rounding = getOption("rounding"),
                             test = c("both", "parametric", "non-parametric", "none"),
                             condense.binary.factors = getOption("condense.binary.factors"),
                             drop.y = getOption("drop.y"),
                             fit.model = NULL, 
                             outcome = getOption("outcome"),
                             FUN.model = NULL, ...){
  
  xname <- deparse(substitute(x))
  if(is.data.frame(x)){
    xname <- names(x)
    x <- x[[1]]
  } 
  if(drop.y & !is.null(y)) y <- factor(y)
  lly <- length(levels(y))
  dots <- list(...)
  
  totals <- inbra(ifelse(is.na(mean(x, na.rm = T)), "-", round(mean(x, na.rm = T), rounding)),
                  ifelse(is.na(sd(x, na.rm = T)), "-", round(sd(x, na.rm = T), rounding)))
  names(totals) <- paste0("Totals", " (n = ", sum(!is.na(y)),"; ", round(sum(!is.na(y))/length(y)*100, rounding), "%)")
  output <- c(totals)
  
  if(!is.null(y)){
    mitjanes <- ifelse(is.na(tapply(x, y, mean, na.rm = T)), "-", round(tapply(x, y, mean, na.rm = T), rounding))
    desvsd <- ifelse(is.na(tapply(x, y, sd, na.rm = T)), "-", round(tapply(x, y, sd, na.rm = T), rounding))
    desc <- inbra(mitjanes, desvsd)
    perc.desc <- round(prop.table(table(y)) * 100, rounding)
    names(desc) <- paste0(levels(y), " (n = ", table(y),"; ", perc.desc, "%)")
    output <- c(output, desc)
    
    test <- match.arg(test)
    nonparam <- getOption("non.parametric.tests")
    param <- getOption("parametric.tests")
    
    if(lly==2) tt <- "numeric.binary" else tt <- "numeric.multi"
    test.pvals <- tryCatch(switch( test, 
                                   "non-parametric" = c("non-parametric" = do.call(nonparam[[tt]][[1]], c(list(x ~ y), nonparam[[tt]][-1]))$p.value),
                                   "parametric"     = c("parametric p-value" = do.call(param[[tt]][[1]], c(list(x ~ y), param[[tt]][-1]))$p.value),
                                   "both"           = c("non-parametric p-value" = do.call(nonparam[[tt]][[1]], c(list(x ~ y), nonparam[[tt]][-1]))$p.value, 
                                                        "parametric p-value" = do.call(param[[tt]][[1]], c(list(x ~ y), param[[tt]][-1]))$p.value),
                                   "none"           = NA), 
                           error = function(e){
                             warning(paste0("Error for variable ", xname, ": ", as.character(e)))
                             if(test == "both"){
                               return(c(NA, NA))
                             } else {
                               return(NA)
                             }
                           }
    )
    pround <- getOption("p.value.rounding")
    test.pvals <- do.call(pround[[1]], c(list(test.pvals), pround[-1]))
    if(test == "none") test.pvals <- NULL
    output <- c(output, test.pvals)
    
    if(!is.null(fit.model)){ 
      pmod <- NULL
      mod <- list()
      
      if(outcome == 1){
        outvar <- x
        invar <- y
      } else {
        outvar <- if(lly > 2) as.numeric(y) else y
        invar <- x
      }
      
      for(i in 1:length(fit.model)){
        if(length(levels(outvar)) == 2){
          mod[[names(fit.model)[i]]] <- glm(outvar ~ invar, family = binomial, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(length(levels(invar)) > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else if(length(levels(outvar)) == 0){
          mod[[names(fit.model)[i]]] <- lm(outvar ~ invar, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(length(levels(invar)) > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else {warning("Factor with only one level")}
        output <- c(output, pmod[i])
        names(output)[length(output)] <- paste(names(fit.model)[i], "p-value")
        
        if(!is.null(FUN.model[[names(fit.model)[i]]])){
          funs.mat <- NULL
          funcions <- FUN.model[[names(fit.model)[i]]]
          args.funcions <- dots[[names(fit.model)[i]]]
          for(j in 1:length(FUN.model[[names(fit.model)[i]]])){
            sortida <- do.call(funcions[j], c(list(mod[[i]]), args.funcions[[funcions[j]]]))
            sortida <- as.vector(sortida)
            names(sortida) <- paste0(funcions[j], 1:length(sortida), " ", names(fit.model)[i])
            funs.mat <- c(funs.mat, sortida)
          }
          output <- c(output, funs.mat)
        }
      }
    }
  }
  cols <- names(output)
  output <- matrix(output, nrow=1)
  colnames(output) <- cols
  rownames(output) <- xname
  return(output)
}

bivarTable <- function(X, ...) UseMethod("bivarTable")
bivarTable.default <- function(X, 
                               y = NULL, 
                               data = NULL,
                               margin = getOption("margin"),
                               rounding = getOption("rounding"),
                               test = c("both", "parametric", "non-parametric", "none"),
                               condense.binary.factors = getOption("condense.binary.factors"),
                               drop.x = getOption("drop.x"), 
                               drop.y = getOption("drop.y"),
                               fit.model = NULL,
                               outcome = getOption("outcome"),
                               FUN.model = NULL, ...){
  
  nmy <- if(!is.null(y)) deparse(substitute(y)) else ""
  nmX <- names(X)
  taula <- NULL
  
  if(is.list(y)){ 
    nmy <- names(y)
    y <- y[[1]]
  }
  
  for(i in 1:length(X)){
    novataula <- bivarRow(x = X[i], 
                          y = y, 
                          data = data,
                          margin = margin, 
                          rounding = rounding, 
                          test = test, 
                          condense.binary.factors = condense.binary.factors, 
                          drop.x = drop.x,
                          drop.y = drop.y,
                          fit.model = fit.model, 
                          outcome = outcome, 
                          FUN.model = FUN.model, ...)
    taula <- rbind(taula, novataula)
  }
  
  out <- list()
  out$table <- taula
  out$margin <- margin
  out$outcome <- outcome
  out$fit.model <- fit.model 
  out$FUN.model <- FUN.model 
  out$test <- match.arg(test)
  out$condense.binary.factors <- condense.binary.factors
  out$drop.x <- drop.x
  out$drop.y <- drop.y
  out$X <- X
  out$y <- data.frame(y)
  if(!is.null(y)) names(out$y) <- nmy
  out$other <- list(...)
  class(out) <- c("bivarTable", class(out))
  
  return(out)
}
bivarTable.formula <- function(X, 
                               data = NULL,
                               margin = getOption("margin"),
                               rounding = getOption("rounding"),
                               test = c("both", "parametric", "non-parametric", "none"),
                               condense.binary.factors = getOption("condense.binary.factors"),
                               drop.x = getOption("drop.x"), 
                               drop.y = getOption("drop.y"),
                               fit.model = NULL,
                               outcome = getOption("outcome"),
                               FUN.model = NULL, ...){
  
  resp <- attr(terms(X, data = data), "response") == 1
  df <- model.frame(X, data = data, na.action = na.pass)
  noms <- names(df)
  df <- as.data.frame(sapply(as.list(df), unAsIs, simplify = F))
  names(df) <- noms
  
  if(resp){
    y <- df[1]
    X <- df[-1][attr(terms(X, data = data), "term.labels")]
  } else {
    y <- NULL
    X <- df
  }
  bivarTable.default(X = X, 
                     y = y, 
                     data = data,
                     margin = margin, 
                     rounding = rounding, 
                     test = test, 
                     condense.binary.factors = condense.binary.factors, 
                     drop.x = drop.x,
                     drop.y = drop.y,
                     fit.model = fit.model, 
                     outcome = outcome, 
                     FUN.model = FUN.model, ...)
}

print.bivarTable <- function(X){
  outX <- X$table
  print.default(outX, quote = FALSE)
}

export <- function(X, ...) UseMethod("export")
export.default <- function(X, 
                   cols = colnames(X), 
                   rows = rownames(X), 
                   type = "xlsx",
                   file = tempfile(pattern = "exportDefault", fileext = paste0(".", type)), 
                   caption = NULL,
                   label = NULL, 
                   append = FALSE, ...){
  
  type <- rev(strsplit(file, "\\.")[[1]])[1]
  colnames(X) <- cols
  rownames(X) <- rows

  if(type == "csv"){
    write.table(x = X, 
                append = append,
                file = file, 
                row.names = TRUE, 
                sep = ";", 
                col.names = TRUE, 
                na = "")
  } else if(type == "tex") {
    print(xtable(x = X, caption = caption, label = label), 
          file = file, 
          include.rownames = TRUE, 
          include.colnames = TRUE, ...)
  } else if(type == "xlsx"){
    require(openxlsx)
    write.xlsx(X, file = file, row.names = TRUE, col.names = TRUE)
    openXL(file)
  } else stop("File extension is not valid")
  return(invisible(X))
}
export.bivarTable <- function(X, 
                              type = "xlsx",
                              file = tempfile(pattern = "exportBivarTable", fileext = paste0(".", type)), 
                              caption = NULL,
                              label = NULL, ...){
  type <- rev(strsplit(file, "\\.")[[1]])[1]
  taula <- X$table
  col.names <- colnames(taula)
  row.names <- rownames(taula)
  row.data <- X$X
  col.vect <- X$y[,1]
  nmy <- names(X$y)
  lly <- length(levels(col.vect))
  ntests <- if(X$test == "none") 0 else if(X$test == "both") 2 else 0
  model <- X$fit.model
  
  if(type == "csv"){
    export.default(taula, file = file, ...)
  } else if(type == "tex") {
    export.default(taula, file = file, ...)
  } else if(type == "xlsx"){
    require(openxlsx)
    
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 8)
    addWorksheet(wb = wb, sheetName = "BIVARTABLE")
    
    # styles
    row1Style <- createStyle(border = "BottomLeftRight", valign = "center", halign = "center", textDecoration = c("italic", "bold"))
    col1Style.vars <- createStyle(border = "Right", valign = "center", halign = "left", textDecoration = c("italic", "bold"))
    col1Style.levels <- createStyle(border = "Right", valign = "center", halign = "right", textDecoration = c("italic", "bold"))
    
    # first row
    mergeCells(wb = wb, sheet = 1, cols = 1, rows = 1:2)
    mergeCells(wb = wb, sheet = 1, cols = 2, rows = 1:2)
    writeData(wb = wb, sheet = 1, x = col.names[1], startCol = 2, startRow = 1, colNames = TRUE, rowNames = TRUE)
    vlineafter <- c(1, 2)

    if(!is.null(col.vect)){
      mergeCells(wb = wb, sheet = 1, cols = 3:(lly + 2), rows = 1)
      writeData(wb = wb, sheet = 1, x = nmy, startCol = 3, startRow = 1, colNames = TRUE, rowNames = TRUE)
      writeData(wb = wb, sheet = 1, x = row.names, startCol = 1, startRow = 3, colNames = TRUE, rowNames = TRUE)
      writeData(wb = wb, sheet = 1, x = t(as.matrix(col.names[2:(lly + 1)])), startCol = 3, startRow = 2, colNames = FALSE, rowNames = FALSE)
      vlineafter <- c(vlineafter, lly + 2)

      if(ntests==2){
        mergeCells(wb = wb, sheet = 1, cols = (3 + lly) : (4 + lly), rows = 1)
        writeData(wb = wb, sheet = 1, x = "p-values", startCol = 3 + lly, startRow = 1, colNames = FALSE, rowNames = FALSE)
        writeData(wb = wb, sheet = 1, x = t(as.matrix(gsub(pattern = " p-value", replacement = "", col.names[(2 + lly):(2 + lly + (X$test == "both"))]))), startCol = 3 + lly, startRow = 2, colNames = FALSE, rowNames = FALSE)
      } else if(ntests == 1){
        writeData(wb = wb, sheet = 1, x = "p-value", startCol = 3 + lly, startRow = 1, colNames = FALSE, rowNames = FALSE)
        writeData(wb = wb, sheet = 1, x = X$test, startCol = 3 + lly, startRow = 2, colNames = FALSE, rowNames = FALSE)
      }
      vlineafter <- c(vlineafter, 2+lly+ntests)
      
      if(!is.null(model)){
        for(i in 1:length(model)){
          nom <- paste0(names(model)[i], " ")
          quins <- grepl(pattern = nom, col.names)
          illa <- sum(diff(c(F, quins, F))!=0) <= 2
          if(illa & sum(quins)>1){
            col.names[quins] <- gsub(pattern = nom, replacement = "", col.names[quins])
            celes <- range(which(quins) + 1)
            mergeCells(wb = wb, sheet = 1, cols = celes, rows = 1)
            writeData(wb = wb, sheet = 1, startCol = celes[1], startRow = 1, x = nom, colNames = FALSE, rowNames = FALSE)
            writeData(wb = wb, sheet = 1, startCol = celes[1], startRow = 2, x = t(as.matrix(col.names[quins])), colNames = FALSE, rowNames = FALSE)
            vlineafter <- c(vlineafter, celes[2])
          } else if(illa & sum(quins)==1){
            col.names[quins] <- gsub(pattern = nom, replacement = "", col.names[quins])
            writeData(wb = wb, sheet = 1, startCol = which(quins) + 1, startRow = 1, x = nom, colNames = FALSE, rowNames = FALSE)
            writeData(wb = wb, sheet = 1, startCol = which(quins) + 1, startRow = 2, x = col.names[quins], colNames = FALSE, rowNames = FALSE)
            vlineafter <- c(vlineafter, which(quins) + 1)
          }
        }
      }
    }
    
    # writeData's
    writeData(wb = wb, sheet = 1, x = taula, startCol = 2, startRow = 3, colNames = FALSE, rowNames = FALSE)
    
    # add styles
    ## fer un for q vagi recorrent tota la matriu i vagi mirant si es un titol posali aquest estil, si es noseque posali tal altre, ...
    celes <- expand.grid(1:(2 + length(row.names)), 1:(1 + length(col.names)))
    
    addStyle(wb = wb, sheet = 1, rows = celes[[1]], cols = celes[[2]], style = createStyle(halign="right", valign="center"))
    addStyle(wb = wb, sheet = 1, rows = 1:(2 + length(row.names)), cols = rep(2, length(1:(2+length(row.names)))), style = createStyle(border="Right", halign="right", valign="center"))
    addStyle(wb = wb, sheet = 1, rows = 1:(2 + length(row.names)), cols = rep(2 + lly + (X$test != "none") + (X$test == "both"), length(1:(2 + length(row.names)))), style = createStyle(border = "Right", halign = "right", valign = "center"))
    addStyle(wb = wb, sheet = 1, rows = 1:(2 + length(row.names)), cols = rep(lly + 2, length(1:(2 + length(row.names)))), style = createStyle(border = "Right", halign = "right", valign = "center"))
    
    addStyle(wb = wb, sheet = 1, rows = 1:(length(row.names) + 2), cols = rep(1, length(1:(length(row.names) + 2))), style = col1Style.vars)
    rowlevels <- which(sapply(row.names, function(x){
      any(sapply(as.list(row.data), function(y){
        if(is.factor(y)){
          any(x %in% levels(y))
        } else FALSE
      }))
    })) + 2
    if(length(rowlevels)>0) addStyle(wb = wb, sheet = 1, rows = rowlevels, cols = rep(1, length(rowlevels)), style = col1Style.levels)
    
    addStyle(wb = wb, sheet = 1, rows = rep(1, length(col.names) + 1), cols = 1:(length(col.names) + 1), style = row1Style)
    addStyle(wb = wb, sheet = 1, rows = rep(2, length(col.names) + 1), cols = 1:(length(col.names) + 1), style = row1Style)
    addStyle(wb = wb, sheet = 1, rows = 1:2, cols = c(2, 2), style = createStyle(border = "LeftBottomRight", textDecoration = c("italic", "bold"), valign = "center", halign = "center"))
    addStyle(wb = wb, sheet = 1, rows = rep(1, length(3:(lly + 2))), cols = 3:(lly + 2), style = createStyle(border = "LeftBottomRight", textDecoration = c("italic", "bold"), valign = "center", halign = "center"))
    if(ntests!=0) addStyle(wb = wb, sheet = 1, rows = rep(1, length((lly + 3):(lly + 3 + (X$test == "both")))), cols = (lly + 3):(lly + 3 + (X$test == "both")), style = createStyle(border = "LeftBottomRight", textDecoration = c("italic", "bold"), valign = "center", halign = "center"))
    addStyle(wb = wb, sheet = 1, rows = c(2, 2), cols = c((lly + 2), length(col.names) + 1), style = createStyle(border = "BottomRight", textDecoration = c("italic", "bold"), valign = "center", halign = "center"))
    addStyle(wb = wb, sheet = 1, rows = 1:2, cols = c(1, 1), style = createStyle(border = "RightBottom"))
    addStyle(wb = wb, sheet = 1, rows = rep(2 + length(row.names), length(col.names) + 1), cols = 1:(length(col.names) + 1), style = createStyle(border = "Bottom", valign = "center", halign = "right"))
    addStyle(wb = wb, sheet = 1, rows = 2 + length(row.names), cols = 1, style = createStyle(border = "RightBottom", valign = "center", halign = if((length(row.names) + 2) %in% rowlevels) "right" else "left", textDecoration = c("bold", "italic")))
    addStyle(wb = wb, sheet = 1, rows = rep(2 + length(row.names), 4), cols = c(2, lly + 2, lly + 2 + ntests, length(col.names) + 1), style = createStyle(border = "RightBottom", valign = "center", halign = "right"))
    
    addStyle(wb = wb, sheet = 1, rows = 3:(length(row.names) + 1), cols = rep(length(col.names) + 1, length(3:(length(row.names) + 1))), style = createStyle(border = "Right", valign = "center", halign = "right"))
    
    setColWidths(wb, sheet = 1, cols = 1:(ncol(taula) + 1), widths = "auto", ignoreMergedCells = TRUE)
    saveWorkbook(wb, file, overwrite = TRUE)
    openXL(file)
  } else stop("File extension is not valid")  
}