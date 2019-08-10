tf_name <- function(phrase, type = input$trans_func){
  switch(type,
         log10 = bquote(log[10] * '(' * .(phrase) * ')'),
         log1p = bquote(log[10] * '(' * .(phrase) + 1 * ')'),
         ln = bquote(ln * '(' * .(phrase) * ')'),
         sqrt = bquote(sqrt(.(phrase))),
         sq = bquote('(' * .(phrase) * ')'^{2}),
         sq4 = bquote(sqrt(.(phrase), 4)),
         phrase)
}

remove_nan_inf <- function(d){
  rmv_rows <- c()
  for( i in 1:ncol(d) ){
    if( class(d[, i]) != 'numeric' ){
      next
    }
    ind <- is.infinite(d[, i]) | is.nan(d[, i])
    rmv_rows <- c(rmv_rows, (1:nrow(d))[ind])
  }
  if( length(rmv_rows) == 0 ){
    return(d)
  }else{
    dd <- d[-rmv_rows, ]
    return(dd)
  }
}


splitsets <- function(input, prop=c(.50,.25,.25))  {  
  #prop - three proportions equaling 1: first number is training dataset, second is validation, third is test
  
  if (prop[1]+prop[2]+prop[3] != 1 | length(prop) != 3)  {
    stop("Check proportions")
  }
  
  res <- vector(mode="list", length=3)
  
  n.train <- round(nrow(input) * prop[1])
  n.val <- round(nrow(input) * prop[2])
  n.test <- (nrow(input)-n.train-n.val)
  
  choice <- input
  train <- input[input$CATCHID %in% sample(input$CATCHID,n.train),]
  choice <- input[!(input$CATCHID %in% train$CATCHID),]
  val <- input[input$CATCHID %in% sample(choice$CATCHID,n.val),]
  test <- input[!(input$CATCHID %in% train$CATCHID | input$CATCHID %in% val$CATCHID),]
  
  res[[1]] <- train
  res[[2]] <- val
  res[[3]] <- test
  
  names(res) <- c('train', 'val', 'test')
  return(res)
}

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  # txt <- captureOutput(results <- expr)
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- search()[ifelse(
    unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0){
    for (package in package.list) detach(package, character.only=TRUE)
  }  
}

indexDirAlign <- function(x, dir = TRUE){
  xrank <- rank(x, na.last = 'keep', ties.method = 'max')
  min.x <- min(xrank, na.rm = TRUE)
  max.x <- max(xrank, na.rm = TRUE)
  if(dir){
    y <- (xrank - min.x)  / (max.x - min.x)
  }else{
    y <- 1 - ((xrank - min.x)  / (max.x - min.x))
  }
  return(y)
}
