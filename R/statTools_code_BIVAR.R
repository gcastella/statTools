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




bivarRow <- function(x, ...) UseMethod("bivarRow", object = if(is.data.frame(x)) x[[1]] else x)

bivarRow.default <- function(x, y, ...){
  warning("Only factor and numeric classes are supported")
  return(NULL)
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