bivarRow <- function(x, ...) UseMethod("bivarRow", object = if(is.data.frame(x)) x[[1]] else x)

bivarRow.default <- function(x, y, ...){
  warning("Only factor and numeric classes are supported")
  return(NULL)
}

bivarRow.factor <- function(x, y = NULL, data = NULL,
                            margin = getOption("margin"),
                            rounding = getOption("rounding"),
                            test = c("none", "parametric", "non-parametric", "both"),
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
                             test = c("none", "parametric", "non-parametric", "both"),
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

#' bivarTable 
#' 
#' Creating a table with the results of assessing differences among groups.
#' 
#' 
#' @param X Fórmula només amb termes additius. Accepta AsIs class, punt (.) i restes.
#' La part esquerra defineix les columnes (variable amb els grups), la dreta 
#' les files (on veure diferències entre grups).
#' @param data D'on llegir les dades per interpretar les fórmules.
#' @param margin Valors 1 o 2. Perfils fila o columna a les taules de contingència, 
#' respectivament. 
#' @param rounding Nombre de decimals a tenir en compte (els p-valors s'arrodoneixen amb un 
#' altre criteri).
#' @param test One of "both", "parametric", "non-parametric" or "none". Si volem que fagi uns 
#' testos predeterminats en concret (veure options()$parametric.tests i 
#' options()$non.parametric.tests).
#' @param condense.binary.factors Si transformar en una sola línia les variables binàries.
#' @param drop.x Refactoritzar les variables fila si són factors per treure nivells sense observacions.            
#' @param drop.y Refactoritzar la variable columna (grups) per treure nivells sense observacions. 
#' @param fit.model Llista amb noms de les fórmules a partir de les quals ajustar un model glm o lm
#' segons s'escaigui. L'ús del punt (.) s'utilitza per referir-se de forma general 
#' a la variable fila o columna sense haver d'especificar-ne el nom (veure update.formula).
#' @param outcome Valors 1 o 2, segons si pels models la variable considerada outcome seran les 
#' que estan a les files o a les columnes, respectivament. 
#' @param FUN.model Funcions extres que s'aplicaran als models, els outputs de les quals s'afegiran 
#' com a noves columnes a la taula resultant. El primer paràmetre de les quals 
#' ha de ser el model. S'especifica com una llista on cada element s'anomena com un
#' dels models especificats a fit.model i serà un vector amb els noms de les funcions
#' (entrats com a string) a aplicar en cadascun dels respectius models. Funcions 
#' preparades: nagelkerke(), adjNagelkerke(), getPval(), getBetaSd(), getORCI(),
#' verticalgetPval(), verticalgetBetaSd(), verticalgetORCI(), GoF().
#' @param ... Arguments extres per les funcions del paràmetre FUN.model. Per a especificar els
#' arguments extres d'una funció que s'aplica a un model, s'afegeix dintre dels ...
#' un  argument amb el nom del model corresponent, i que serà una llista on cada element
#' tingui per nom la funció de la qual es volen canviar els paràmetres, i contingui una 
#' altra llista amb els paràmetres modificats (tipus el paràmetre args de do.call()).
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
#'                                    adj = c("getORCI", "getPval")),
#'                   adj = list(getPval = list(vars = 4), 
#'                              getORCI = list(vars = 4)))
#' bvt
#' bvt <- bivarTable(X = I(Sepal.Width>3) ~ . - Sepal.Length,
#'                   data = iris, 
#'                   margin = 2, 
#'                   rounding = 3,
#'                   test = "both", 
#'                   condense.binary.factors = FALSE, 
#'                   drop.x = TRUE, 
#'                   drop.y = TRUE, 
#'                   fit.model = list(simple = ~ ., 
#'                                    adj = ~ . + Sepal.Length), 
#'                   outcome = 2, 
#'                   FUN.model = list(simple = c("getPval"), 
#'                                    adj = c("verticalgetORCI", 
#'                                            "verticalgetPval")))
#' bvt
#' \dontrun{
#' export(bvt)
#' }
#' 
#' @seealso \code{\link{export}}
#' @export
bivarTable <- function(X, ...) UseMethod("bivarTable")

#' @export
#' @describeIn bivarTable Calling separately a data frame with the variables in the rows and the grouping variable.
bivarTable.default <- function(X, 
                               y = NULL, 
                               data = NULL,
                               margin = getOption("margin"),
                               rounding = getOption("rounding"),
                               test = c("none", "parametric", "non-parametric", "both"),
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


#' @export
#' @describeIn bivarTable Calling bivarTable using a formula.
bivarTable.formula <- function(X, 
                               data = NULL,
                               margin = getOption("margin"),
                               rounding = getOption("rounding"),
                               test = c("none", "parametric", "non-parametric", "both"),
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
    names_cols <- sapply(strsplit(split = " \\(", names_cols), paste0, collapse = "\n(")
    
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