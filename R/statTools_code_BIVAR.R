bivarRow <- function(x, ...) UseMethod("bivarRow", object = if(is.data.frame(x)) x[[1]] else x)

bivarRow.default <- function(x, y, ...){
  warning(paste0(typeof(x), " is not handled in bivarTable(), no output generated"))
  return(NULL)
}

bivarRow.factor <- function(x, y = NULL, data = NULL,
                            margin = getOption("margin"),
                            rounding = getOption("rounding"),
                            condense.binary.factors = getOption("condense.binary.factors"),
                            drop.x = getOption("drop.x"), 
                            drop.y = getOption("drop.y"),
                            test = c("none", "parametric", "non-parametric", "both"),
                            outcome = getOption("outcome"),
                            pvalues.model = getOption("pvalues.model"),
                            fit.model = NULL, 
                            FUN.model = NULL, ...){
  
  dots <- list(...)
  output <- list()
  
  xname <- deparse(substitute(x))
  if(is.data.frame(x)){
    xname <- names(x)
    x <- x[[1]]
  } 
  if(drop.x) x <- factor(x)
  if(drop.y & !is.null(y)) y <- factor(y)
  lly <- length(levels(y))
  llx <- length(levels(x))
  
  all_desc <- rbind("", descr_var(x = x, rounding = rounding, ...))
  rownames(all_desc) <- c(xname, levels(x))
  colnames(all_desc) <- paste0("All", 
                                 " (n = ", sum(!is.na(y)),
                                 "; ", 
                                 round(sum(!is.na(y)) / length(y) * 100, rounding), 
                                 "%)")
  output$all <- all_desc
    
  if(!is.null(y)){
    
    bygroup_desc <- rbind("", descr_var(x = x, y = y, rounding = rounding, margin = margin, ...))
    perc_desc <- round(prop.table(table(y)) * 100, rounding)
    colnames(bygroup_desc) <- paste0(levels(y), " (n = ", table(y),"; ", perc_desc, "%)")
    output$bygroup_desc <- bygroup_desc
    
    test <- match.arg(test)
    nonparam <- getOption("non.parametric.tests")
    param <- getOption("parametric.tests")
    test.pvals <- tryCatch(
      switch(test, 
        "non-parametric" = c("non-parametric" = do.call(nonparam$factor[[1]], c(list(table(x, y)), nonparam$factor[-1]))$p.value),
        "parametric"     = c("parametric p-value" = do.call(param$factor[[1]], c(list(table(x, y)), param$factor[-1]))$p.value),
        "both"           = c("non-parametric p-value" = do.call(nonparam$factor[[1]], c(list(table(x, y)), nonparam$factor[-1]))$p.value, 
                            "parametric p-value" = do.call(param$factor[[1]], c(list(table(x, y)), param$factor[-1]))$p.value),
        "none"           = NA
      ), 
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
    test.pvalsmat <- matrix(rep("", times = length(test.pvals) * nrow(output[[1]])), ncol = length(test.pvals), nrow = nrow(output[[1]]))
    test.pvalsmat[1,] <- test.pvals
    colnames(test.pvalsmat) <- names(test.pvals)
    if(test == "none") test.pvalsmat <- NULL
    output$tests <- test.pvalsmat
    
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
      
      for(i in seq_along(fit.model)){
        if(lloutvar == 2){
          mod[[names(fit.model)[i]]] <- glm(outvar ~ invar, family = binomial, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], 
                                               fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(llinvar > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else if(lloutvar == 0){
          mod[[names(fit.model)[i]]] <- lm(outvar ~ invar, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], 
                                               fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(llinvar > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else {
          warning(paste(deparse(substitute(x)), "has only one level"))
        }
        
        if(pvalues.model){
          names(pmod)[i] <- names(fit.model)[i]
          pm <- as.matrix(c(pmod[i], rep("", llx)))
          colnames(pm) <- paste(names(fit.model)[i], "drop1 p-value")
          output$pval_models <- pm
        }
        
        if(!is.null(FUN.model[[names(fit.model)[i]]])){
          funs.mat <- NULL
          funcions <- FUN.model[[names(fit.model)[i]]]
          args.funcions <- dots[[names(fit.model)[i]]]
          for(j in seq_along(FUN.model[[names(fit.model)[i]]])){
            sortida <- do.call(funcions[j], c(list(mod[[i]]), args.funcions[[funcions[j]]]))
            if(is.vector(sortida)) sortida <- t(as.matrix(sortida))
            if(nrow(output[[1]]) - nrow(sortida) < 1){
              sortida <- sortida[1:nrow(output[[1]]), , drop = FALSE]
              auxmat <- NULL
            } else {
              auxmat <- matrix("", ncol = ncol(sortida), nrow = nrow(output[[1]]) - nrow(sortida))
            }
            sortida <- rbind(sortida, auxmat)
            nom_j <- names(FUN.model[[names(fit.model)[i]]])[j]
            if(!is.null(nom_j)){
              colnames(sortida) <- paste0(nom_j, if(ncol(sortida) > 1) seq_along(sortida[1, ]) else NULL, " ", names(fit.model)[i])
            } else {
              colnames(sortida) <- paste0(funcions[j], 1:ncol(sortida), " ", names(fit.model)[i])
            }
            funs.mat <- cbind(funs.mat, sortida)
          }
          output$funs_model <- funs.mat
        }
      }
    }
  }
  output <- do.call(cbind, output)
  if(condense.binary.factors & llx == 2){
    rownames(output)[1] <- inbra(rownames(output)[1], rownames(output)[3])
    output[1, 1:(lly + 1)] <- output[3, 1:(lly + 1)]
    output <- output[1, , drop = FALSE]
  }
  return(output)
}
bivarRow.numeric <- function(x, y = NULL, data = NULL,
                             margin = getOption("margin"),
                             rounding = getOption("rounding"),
                             condense.binary.factors = getOption("condense.binary.factors"),
                             drop.y = getOption("drop.y"),
                             test = c("none", "parametric", "non-parametric", "both"),
                             outcome = getOption("outcome"),
                             pvalues.model = getOption("pvalues.model"),
                             fit.model = NULL, 
                             FUN.model = NULL, ...){
  
  dots <- list(...)
  output <- list()
  
  xname <- deparse(substitute(x))
  if(is.data.frame(x)){
    xname <- names(x)
    x <- x[[1]]
  } 
  if(drop.y & !is.null(y)) y <- factor(y)
  lly <- length(levels(y))
  
  all_desc <- descr_var(x = x, rounding = rounding, ...)
  rownames(all_desc) <- xname
  colnames(all_desc) <- paste0("All", 
                               " (n = ", sum(!is.na(y)),
                               "; ", 
                               round(sum(!is.na(y)) / length(y) * 100, rounding), 
                               "%)")
  output$all <- all_desc
  
  if(!is.null(y)){
    
    bygroup_desc <- descr_var(x = x, y = y, rounding = rounding, ...)
    perc_desc <- round(prop.table(table(y)) * 100, rounding)
    colnames(bygroup_desc) <- paste0(levels(y), " (n = ", table(y),"; ", perc_desc, "%)")
    output$bygroup_desc <- bygroup_desc
    
    test <- match.arg(test)
    nonparam <- getOption("non.parametric.tests")
    param <- getOption("parametric.tests")
    
    if(lly == 2) tt <- "numeric.binary" else tt <- "numeric.multi"
    test.pvals <- tryCatch(
      switch(test, 
        "non-parametric" =  c("non-parametric" = do.call(nonparam[[tt]][[1]], c(list(x ~ y), nonparam[[tt]][-1]))$p.value),
        "parametric"     =  c("parametric p-value" = do.call(param[[tt]][[1]], c(list(x ~ y), param[[tt]][-1]))$p.value),
        "both"           =  c("non-parametric p-value" = do.call(nonparam[[tt]][[1]], c(list(x ~ y), nonparam[[tt]][-1]))$p.value, 
                              "parametric p-value" = do.call(param[[tt]][[1]], c(list(x ~ y), param[[tt]][-1]))$p.value),
        "none"           =  NA), 
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
    output$tests <- t(test.pvals)
    
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
      
      for(i in seq_along(fit.model)){
        if(length(levels(outvar)) == 2){
          mod[[names(fit.model)[i]]] <- glm(outvar ~ invar, family = binomial, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], 
                                               fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(length(levels(invar)) > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else if(length(levels(outvar)) == 0){
          mod[[names(fit.model)[i]]] <- lm(outvar ~ invar, data = data)
          mod[[names(fit.model)[i]]] <- update(mod[[names(fit.model)[i]]], 
                                               fit.model[[names(fit.model)[i]]])
          pmod[names(fit.model)[i]] <- do.call(pround[[1]], c(list(if(length(levels(invar)) > 2) drop1(mod[[names(fit.model)[i]]], test = "Chi")[2, 5] else summary(mod[[names(fit.model)[i]]])$coef[2, 4]), pround[-1]))
        } else {
          warning("Factor with only one level")
        }
        
        if(pvalues.model){
          output$pvalue_model <- t(pmod[i])
          colnames(output$pvalue_model) <- paste(names(fit.model)[i], "drop1 p-value")
        }
        
        if(!is.null(FUN.model[[names(fit.model)[i]]])){
          funs.mat <- NULL
          funcions <- FUN.model[[names(fit.model)[i]]]
          args.funcions <- dots[[names(fit.model)[i]]]
          for(j in 1:length(FUN.model[[names(fit.model)[i]]])){
            sortida <- do.call(funcions[j], c(list(mod[[i]]), args.funcions[[funcions[j]]]))
            sortida <- as.vector(sortida)
            nom_j <- names(FUN.model[[names(fit.model)[i]]])[j]
            if(!is.null(nom_j)){
              names(sortida) <- paste0(nom_j, if(length(sortida) > 1) seq_along(sortida) else NULL, " ", names(fit.model)[i])
            } else {
              names(sortida) <- paste0(funcions[j], 1:length(sortida), " ", names(fit.model)[i])
            }
            funs.mat <- c(funs.mat, sortida)
          }
          output$funs_mat <- t(funs.mat)
        }
      }
    }
  }
  
  output <- do.call(cbind, output)
  return(output)
}

#' Assessing differences in groups 
#' 
#' This function creates a table resulting from an analysis comparing two or more groups .
#' 
#' @param X A data frame with variables in the rows (for .default) or a formula with additive terms (. and - are allowed too) and with 0 or 1 variable in the LHS.
#' LHS of the formula defines the grouping variable, and RHS where to assess differences (for .formula).
#' @param y Grouping variable in the default method.
#' @param data Data frame with the variables in \code{X}.
#' @param margin For factors, percentages are calculated. If \code{margin = 1} row percentages sum 100\%, 
#' if 2, column percentages sum 100\%. See \code{margin} from \code{prop.table}.
#' @param rounding Decimal places for all numeric values in the table. P-values are rounded with another criterion.
#' @param pvalue.model Logical. If TRUE a p-value from the drop1 methods are provided for each model fitted.
#' @param test One of "both", "parametric", "non-parametric" or "none". Defines the type of tests to perform in all variables in RHS.
#' See Details section.
#' @param condense.binary.factors Use only one single row for binary factors to avoid redundancy.
#' @param drop.x Perform \code{factor(x)} before all computations to delete unused levels in RHS variables.            
#' @param drop.y Perform \code{factor(y)} before all computations to delete unused levels the grouping variable.            
#' @param fit.model A NAMED list with formulas for fitting models to each row. These formulas are passed to \code{update.formula}
#' so \code{.} here refers to each RHS or LHS variable, depending on argument \code{output}. See Details.
#' @param outcome If 1, RHS are understood as the dependent variables when fitting models. 
#' If 2, LHS variable is understood as the dependent variable in each model fitted.
#' @param FUN.model A NAMED list. Each element has to be named like one of the elements in \code{fit.model}.
#' Each element in FUN.model will be a character vector with the names of functions to be executed in each model to return more particular outputs.
#' If the vector is named, these are used as column names in the final table (See Examples). The first argument of the function
#' has to be the model fitted. Other arguments can be changed in \code{...}. Examples of functions used for this are 
#' nagelkerke(), adjNagelkerke(), getPval(), getBetaSd(), getORCI(), verticalgetPval(), verticalgetBetaSd(), verticalgetORCI(), GoF().
#' @param ... Extra arguments for the functions in \code{FUN.model} or the descriptive functions \code{descr_var()}. To change arguments from the functions, add a list argument named 
#' as the model that the function is applied to, whose elements will be the lists with arguments to modify (passed to do.call), and the 
#' name of each list of arguments the function those arguments belong to (See Examples).
#'
#' @details
#' The grouping variable (LHS of the formula or y for .default) is set on the columns and the other variables (RHS of the formula or X data frame for .default).
#' Variables are read from argument data. Default options for some arguments can be changed using options(argument = value).
#' 
#' The tests performed in each case are defined in options()$parametric.tests and options()$non.parametric.tests. Changing them, the tests
#' performed will change, however, to get the p-value from the tests, the new tests must return a list with an element called p.value. 
#' The default tests are:
#' 
#' Quantitative row variable vs binary grouping variable: t-test (t.test) or Mann-Whitney test (wilcox.test).
#' 
#' Quantitative row variable vs non-binary grouping variable: ANOVA (oneway.test) or Kruskal-Wallis test (kruskal.test).
#' 
#' Qualitative row variable: Chi-squared test (chisq.tes) or Fisher test (fisher.test).
#' 
#' For fitting models, first decide whether the grouping variable is the dependent variable or not. If it is, set ouput to 2 otherwise to 1,
#' meaning that the dependent variable will be the one in the rows of the table (RHS of the formula). Formulas to fit the model must use the dot
#' in the RHS of the formula to refer to the independent variable (either grouping variable or the one in the rows).
#' when the response variable is a binary factor, a logistic regression model is fitted. When is a non-binary factor, the variable
#' is converted to numeric preserving the order and a linear regression model is fitted. When the response is a quantitative variable linear models are
#' fitted.
#' 
#' 
#'
#'
#' @return 
#' A list with class bivarTable, where the first argument is the table generated (a matrix) and the other arguments
#' are input arguments returned as outputs in the list, such as margin, outcome, fit.model, FUN.model, test, drop.x,
#' drop.y, condense.binary.factors, data.frame with the values from variables in the rows (named X), and the same for
#' the columns (named y), and extra arguments in ..., with maybe, the arguments for calling the functions in FUN.model.
#' 
#' @examples
#' iris$prova <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 2)
#' iris$prova2 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 3)
#' iris$prova3 <- cut(iris$Sepal.Width + iris$Sepal.Length, breaks = 5)
#' bvt <- bivarTable(X = Species ~ . + I(Sepal.Width>3) - Sepal.Length, 
#'                   data = iris, 
#'                   margin = 2, 
#'                   outcome = 1, 
#'                   drop.x = TRUE, 
#'                   drop.y = TRUE, 
#'                   test = "non-par", 
#'                   condense.binary.factors = TRUE, 
#'                   rounding = 3,
#'                   fit.model = list(simple = ~ .,
#'                                    adj = ~ . + Sepal.Length), 
#'                   FUN.model = list(simple = c("getPval"), 
#'                                    adj = c("OR (95% CI)" = "getORCI", 
#'                                            "p-value" = "getPval")),
#'                   adj = list(getPval = list(vars = 4), 
#'                              getORCI = list(vars = 4)))
#' bvt
#' bvt <- bivarTable(X = I(Sepal.Width>3) ~ . - Sepal.Length, data = iris,          ## STRUCTURE
#'                   rounding = 3, condense.binary.factors = FALSE,                 ## OPTIONS
#'                   drop.x = TRUE, drop.y = TRUE, margin = 2:1,                    ## OPTIONS
#'                   test = "both",                                                 ## TESTS
#'                   fit.model = list(simple = ~ .,                                 ## FIT MODEL WITH NO COVARIATES
#'                                    adj = ~ . + Sepal.Length),                    ## FIT ADJUSTED MODEL
#'                   outcome = 2,                                                   ## LHS AS DEPENDENT VARIABLE (COLUMNS)
#'                   FUN.model = list(simple = c("p-value" = "getPval"),            ## GET A P-VALUE FROM THE FIRST MODEL
#'                                    adj = c("OR (95% CI)" = "verticalgetORCI",    ## GET ALL OR's FROM THE 2nd MODEL
#'                                            "p-value" = "verticalgetPval")),      ## GET ALL P-VALUES FROM THE 2nd MODEL
#'                   show.quantiles = c(0, 0.5, 1))                                 ## QUANTILES: probs ARGUMENT
#'                                                                                  ## PASSED TO descr_var()
#' bvt
#' \dontrun{
#' export(bvt)                                                                      ## OPEN bvt IN EXCEL
#' }
#' 
#' @seealso \code{\link{export}}
#' @export
bivarTable <- function(X, ...) UseMethod("bivarTable")

#' @export
#' @describeIn bivarTable Default method for bivarTable
bivarTable.default <- function(X, 
                               y = NULL, 
                               data = NULL,
                               margin = getOption("margin"),
                               rounding = getOption("rounding"),
                               condense.binary.factors = getOption("condense.binary.factors"),
                               drop.x = getOption("drop.x"), 
                               drop.y = getOption("drop.y"),
                               test = c("none", "parametric", "non-parametric", "both"),
                               outcome = getOption("outcome"),
                               pvalues.model = getOption("pvalues.model"),
                               fit.model = NULL,
                               FUN.model = NULL, ...){
  stopifnot(is.data.frame(X))
  nmy <- if(!is.null(y)) deparse(substitute(y)) else ""
  nmX <- names(X)
  taula <- NULL
  
  if(is.list(y)){ 
    nmy <- names(y)
    y <- y[[1]]
  }
  
  taula_list <- lapply(seq_along(X), FUN = function(i){
    bivarRow(x = X[i], 
             y = y, 
             data = data,
             margin = margin, 
             rounding = rounding, 
             condense.binary.factors = condense.binary.factors, 
             drop.x = drop.x,
             drop.y = drop.y,
             test = test, 
             outcome = outcome, 
             pvalues.model = pvalues.model, 
             fit.model = fit.model, 
             FUN.model = FUN.model, ...)
  })
  taula <- do.call(rbind, taula_list)
  
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
  out$pvalues.model <- pvalues.model
  out$other <- list(...)
  class(out) <- c("bivarTable", class(out))
  
  return(out)
}


#' @export
#' @describeIn bivarTable Formula method for bivarTable
bivarTable.formula <- function(X, 
                               data = NULL,
                               margin = getOption("margin"),
                               rounding = getOption("rounding"),
                               condense.binary.factors = getOption("condense.binary.factors"),
                               drop.x = getOption("drop.x"), 
                               drop.y = getOption("drop.y"),
                               test = c("none", "parametric", "non-parametric", "both"),
                               outcome = getOption("outcome"),
                               pvalues.model = getOption("pvalues.model"),
                               fit.model = NULL,
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
                     outcome = outcome, 
                     pvalues.model = pvalues.model,
                     fit.model = fit.model, 
                     FUN.model = FUN.model, ...)
}


#' @export
print.bivarTable <- function(X, ...){
  outX <- X$table
  print.default(outX, quote = FALSE, ...)
}


#' Export a matrix to tex, csv or xlsx
#' 
#' Better visualisation of a matrix, or saving it in a file.
#' 
#' @param X Object to be exported (matrix or bivarTable object).
#' @param cols Names for the columns
#' @param rows Names for the rows
#' @param type Type of file to export to (extension without the dot).
#' @param file Name of the file. By default a temp file is created.
#' @param caption Caption passed to xtable in case of exporting to tex.
#' @param label Label when exporting to tex.
#' @param append Append or not in case of csv.
#' @param ... Further arguments passed to print.xtable
#' 
#' @details
#' When exporting to xlsx, the produced file is opened. Make sure openxlsx is working ok on your computer (Rtools and PATH issues).
#' 
#' @return Returns the argument X but invisible.
#' 
#' @seealso \code{\link{bivarTable}}
#' @export
export <- function(X, ...) UseMethod("export")

#' @export
#' @import xtable
#' @import openxlsx
#' @describeIn export Default method for export.
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

#' @export
#' @import openxlsx
#' @describeIn export For exporting a bivarTable object, with fancier style if type is xlsx.
export.bivarTable <- function(X, 
                              type = "xlsx",
                              file = tempfile(pattern = "exportBivarTable", fileext = paste0(".", type)), 
                              caption = NULL,
                              label = NULL,
                              ...){
  type <- rev(strsplit(file, "\\.")[[1]])[1]
  taula <- X$table
  names_cols <- colnames(taula)
  names_rows<- rownames(taula)
  rhs_data <- X$X
  grouped_by <- X$y[, 1]
  names_y <- names(X$y)
  lly <- nlevels(grouped_by)
  ntests <- if(X$test == "none") 0 else if(X$test == "both") 2 else 1
  model <- X$fit.model
  
  if(type == "csv"){
    export.default(taula, file = file, ...)
  } else if(type == "tex") {
    export.default(taula, file = file, ...)
  } else if(type == "xlsx"){
    
    # add linebreaks in some cols
    names_cols <- sapply(strsplit(split = " \\(n = ", names_cols), paste0, collapse = "\n(n = ")
    
    #create wb
    wb <- createWorkbook()
    modifyBaseFont(wb, fontSize = 8)
    addWorksheet(wb = wb, sheetName = "BIVARTABLE")
    
    # styles
    header_sty <- createStyle(border = "TopBottom", valign = "center", halign = "center", textDecoration = c("italic", "bold"))
    variables_sty <- createStyle(valign = "center", halign = "left", textDecoration = c("italic", "bold"))
    levels_sty <- createStyle(valign = "center", halign = "right", textDecoration = c("italic", "bold"))
    data_sty <- createStyle(valign = "center", halign = "right")
    bgcol_sty <- createStyle(bgFill = "#DBDBDB", fgFill = "#DBDBDB")
    
    # headers
    mergeCells(wb = wb, sheet = 1, cols = 1, rows = 1:2)
    mergeCells(wb = wb, sheet = 1, cols = 2, rows = 1:2)
    writeData(wb = wb, sheet = 1, x = names_cols[1], 
              startCol = 2, startRow = 1, 
              colNames = TRUE, rowNames = TRUE)
    
    if(!is.null(grouped_by)){
      mergeCells(wb = wb, sheet = 1, cols = 1:lly + 2, rows = 1)
      writeData(wb = wb, sheet = 1, x = names_y, 
                startCol = 3, startRow = 1, 
                colNames = TRUE, rowNames = TRUE)
      writeData(wb = wb, sheet = 1, x = names_rows, 
                startCol = 1, startRow = 3, 
                colNames = TRUE, rowNames = TRUE)
      writeData(wb = wb, sheet = 1, x = t(names_cols[1:lly + 1]), 
                startCol = 3, startRow = 2, 
                colNames = FALSE, rowNames = FALSE)
      
      if(ntests == 2){
        mergeCells(wb = wb, sheet = 1, cols = lly + 2 + 1:2, rows = 1)
        writeData(wb = wb, sheet = 1, x = "p-value", 
                  startCol = 3 + lly, startRow = 1, 
                  colNames = FALSE, rowNames = FALSE)
        writeData(wb = wb, sheet = 1, 
                  x = t(gsub(pattern = " p-value", replacement = "", names_cols[lly + 2 + 0:1])), 
                  startCol = lly + 2 + 1, startRow = 2, 
                  colNames = FALSE, rowNames = FALSE)
      } else if(ntests == 1){
        mergeCells(wb = wb, sheet = 1, cols = lly + 2 + 1, rows = 1:2)
        writeData(wb = wb, sheet = 1, 
                  x = paste(gsub(pattern = "non-", replacement = "non-\n", x = X$test), "\n p-value"), 
                  startCol = 3 + lly, startRow = 1, 
                  colNames = FALSE, rowNames = FALSE)
      }
      
      if(!is.null(model)){
        for(i in seq_along(model)){
          nom <- names(model)[i]
          quins <- grepl(pattern = nom, names_cols)
          illa <- sum(diff(c(F, quins, F)) != 0) >= 2
          if(illa & sum(quins) > 1){
            names_cols[quins] <- gsub(pattern = nom, replacement = "", names_cols[quins])
            celes <- range(which(quins) + 1)
            mergeCells(wb = wb, sheet = 1, cols = celes, rows = 1)
            writeData(wb = wb, sheet = 1, 
                      startCol = celes[1], startRow = 1, 
                      x = paste(nom, "model"), 
                      colNames = FALSE, rowNames = FALSE)
            writeData(wb = wb, sheet = 1, 
                      startCol = celes[1], startRow = 2, 
                      x = t(names_cols[quins]), 
                      colNames = FALSE, rowNames = FALSE)
          } else if(illa & sum(quins) == 1){
            names_cols[quins] <- gsub(pattern = paste0(nom, " "), replacement = paste0(nom, "\n"), names_cols[quins])
            names_cols[quins] <- gsub(pattern = paste0(" ", nom), replacement = paste0("\n", nom), names_cols[quins])
            writeData(wb = wb, sheet = 1, 
                      startCol = which(quins) + 1, startRow = 1, 
                      x = nom, 
                      colNames = FALSE, rowNames = FALSE)
            writeData(wb = wb, sheet = 1, 
                      startCol = which(quins) + 1, startRow = 2, 
                      x = names_cols[quins], 
                      colNames = FALSE, rowNames = FALSE)
          }
        }
      }
    }
    
    # data
    writeData(wb = wb, sheet = 1, 
              x = taula, 
              startCol = 2, startRow = 3, 
              colNames = FALSE, rowNames = FALSE)
    
    # styles
    addStyle(wb = wb, sheet = 1, 
             style = header_sty, 
             rows = 1:2, cols = 1:(length(names_cols) + 1), 
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = 1, 
             style = variables_sty, 
             rows = 2 + seq_along(names_rows), cols = 1, 
             gridExpand = TRUE, stack = TRUE)
    all_levels <- unlist(sapply(rhs_data, function(x){
      if(is.factor(x)) return(levels(x))
      NULL
    }))
    addStyle(wb = wb, sheet = 1, 
             style = levels_sty, 
             rows = 2 + seq_along(names_rows)[names_rows %in% all_levels], cols = 1, 
             gridExpand = TRUE, stack = FALSE)
    addStyle(wb = wb, sheet = 1, 
             style = data_sty, 
             rows = 2 + seq_along(names_rows), cols = 1 + seq_along(names_cols), 
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = 1, 
             style = createStyle(border = "Bottom"), 
             rows = 2 + length(names_rows), cols = 1:(length(names_cols) + 1), 
             gridExpand = TRUE, stack = TRUE)
    addStyle(wb = wb, sheet = 1, 
             style = bgcol_sty, 
             rows = 2 + which(seq_along(names_rows)%%2 == 0), cols = 1:(length(names_cols) + 1), 
             gridExpand = TRUE, stack = TRUE)
    
    setColWidths(wb, sheet = 1, cols = 1:(ncol(taula) + 1), widths = "auto", ignoreMergedCells = TRUE)
    saveWorkbook(wb, file, overwrite = TRUE)
    openXL(file)
  } else stop("File extension is not valid")  
}