
splitTrainTest <- function(data, d, output_path, frac = NULL, index = NULL) {
  if (is.null(index)) {
    # TODO: add warning if frac NULL
    index = sample(1:nrow(data), frac*nrow(data))
  }

  train = data[index,] # Create the training data
  test = data[-index,] # Create the test data

  if (file.exists(paste0(output_path, "EXPLORE/", d, "_train.arff"))) {file.remove(paste0(output_path, "EXPLORE/", d, "_train.arff"))}
  if (file.exists(paste0(output_path, "EXPLORE/", d, "_test.arff"))) {file.remove(paste0(output_path, "EXPLORE/", d, "_test.arff"))}

  # Remove columns with no variation in feature from train set (remove from train and test)
  # TODO: remove columns with little variation (less than 5/10?)
  if (min(sapply(train, function(c) length(unique(c)))) == 1) {
    ParallelLogger::logInfo(paste0("Train set ", d, " had ", sum((sapply(train, function(c) length(unique(c))) == 1)), " features with no variation (1 unique value) that are removed"))

    test <- test[,!(sapply(train, function(c) length(unique(c))) == 1)]
    train <- train[,!(sapply(train, function(c) length(unique(c))) == 1)]
  }

  # Write to file
  # farff::writeARFF(train, paste0(output_path, "EXPLORE/", d, "_train.arff"))
  # farff::writeARFF(test, paste0(output_path, "EXPLORE/", d, "_test.arff"))

  return(list(train,test))
}

createModel <- function(method, train = NULL, data_path = NULL, test, data_name, output_path, models, i, bound, ...) {

  # Create model predictions
  if (method == 'lasso') {
    result <- run_lasso(method, train, test, data_name, output_path, models, i)
  } else if (method == 'randomforest') {
    result <- run_randomforest(method, train, test, data_name, output_path, models, i)
  } else if (method == 'ripper') {
    result <- run_ripper(method, train, test, data_name, output_path, models, i)
  } else if (method == 'explore') {
    result <- run_EXPLORE(method, data_path, train, test, data_name, output_path, models, i, start_rule_length = 1, end_rule_length = 4, constraint_accuracy = bound, parallel = "yes")
  }
  # else if (method == 'explore_specificity') {
  #   result <- run_EXPLORE(method, data_path, train, test, data_name, output_path, models, i, specificity = 0.9)
  # }
  # else if (method == 'explore_mandatoryfeature') {
  #   feature_include <- names(which.max(abs(models[method=="lasso" & iteration == i,4:(ncol(models)-1)])))
  #   result <- run_EXPLORE(method, data_path, train, test, data_name, output_path, models, i, feature_include = feature_include)
  # }

  return(result)
}




# https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r
#' @export
run_lasso <- function(method, train, test, data_name, output_path, models, i) {

  # Lambdas to try
  lambdas <- 10^seq(2, -3, by = -.1)

  # Normalize data
  colMean <- apply(train[,-which(names(train) == "class")], 2, mean)
  colSD <- apply(train[,-which(names(train) == "class")], 2, sd)

  train[,-which(names(train) == "class")] <- scale_data(train[,-which(names(train) == "class")], colMean, colSD)
  test[,-which(names(test) == "class")] <- scale_data(test[,-which(names(test) == "class")], colMean, colSD)

  # Setting alpha = 1 implements lasso regression
  model_lasso <- glmnet::cv.glmnet(x=data.matrix(train[, -which(names(train) == "class")]), y = train$class, alpha = 1, lambda = lambdas, standardize = FALSE, nfolds = 5, family = "binomial")
  # plot(model_lasso)

  # Save coefficients
  coef <- as.matrix(coef(model_lasso, s = "lambda.min"))
  var_names <- colnames(models)[3:(ncol(models)-1)]
  var_names[1] <- "(Intercept)"
  vars <- lapply(var_names, function(c) ifelse(c %in% rownames(coef), coef[row.names(coef) == c,1], NA))
  model <- c(list(method, i), as.list(vars), sum(sapply(vars, function(v) ifelse(v != 0, 1, 0))) - 1) # do not count intercept in model size

  coef <- coef[coef != 0,]
  ParallelLogger::logInfo(paste0(method, ": ", paste(names(coef), coef, sep = ":", collapse = ",")))

  pred_lasso <- predict(model_lasso, newx = data.matrix(test[, -which(names(test) == "class")]), type="class", s = "lambda.min")

  # Create accuracy bound from performance
  pred <- predict(model_lasso, newx = data.matrix(train[, -which(names(train) == "class")]), type="class", s = "lambda.min")
  eval <- evaluateModel(as.numeric(pred), train$class)
  bound <- eval$accuracy*0.9

  # Transform character to numeric
  pred_lasso <- as.numeric(pred_lasso)

  return(list(pred_lasso, model, bound))
}


#' @export
run_randomforest<- function(method, train, test, data_name, output_path, models, i) {

  model_randomforest <- randomForest::randomForest(factor(class) ~ ., data = train)

  # Save variables
  count_vars <- randomForest::varUsed(model_randomforest, by.tree = FALSE, count = TRUE)
  names(count_vars) <- colnames(train)[1:(ncol(train)-1)]

  vars <- lapply(colnames(models)[3:(ncol(models)-1)], function(c) ifelse(c %in% names(count_vars), count_vars[c], NA))
  model <- c(list(method, i), as.list(vars), sum(sapply(vars, function(v) ifelse(v > 0, 1, 0)), na.rm = TRUE))

  ParallelLogger::logInfo(paste0(method,": ", paste(paste0(names(count_vars), " ", count_vars), collapse = ",")))

  pred_randomforest <- predict(model_randomforest, test, type = "response")

  # Transform character to numeric
  pred_randomforest <- as.numeric(levels(pred_randomforest))[pred_randomforest]

  return(list(pred_randomforest, model))
}


#' @export
run_ripper <- function(method, train, test, data_name, output_path, models, i) {

  train <- as.data.frame(lapply(train,factor),  stringsAsFactors=FALSE)
  model_ripper <- RWeka::JRip(class ~ . , train)
  # print(model_ripper)

  # Save variables
  model_string <- model_ripper$classifier$toString()
  vars <- lapply(colnames(models)[3:(ncol(models)-1)], function(c) ifelse(stringr::str_detect(model_string, c), 1, 0))
  model <- c(list(method, i), as.list(vars), do.call(sum, vars))

  ParallelLogger::logInfo(paste0(method,": ", model_string))

  test <- as.data.frame(lapply(test,factor),  stringsAsFactors=FALSE)
  pred_ripper <- predict(model_ripper,test, type="class") # alternative: probability

  # Transform factor to numeric
  pred_ripper <- as.numeric(levels(pred_ripper))[pred_ripper]

  return(list(pred_ripper, model))
}



#' @export
run_EXPLORE <- function(method, data_path, train, test, data_name, output_path, models, i, feature_include = NULL, specificity = NULL, start_rule_length = 1, end_rule_length = 3, constraint_accuracy = 0.8, parallel = "yes") {

  # Create output folder for EXPLORE if not exists
  if (!file.exists(paste0(output_path, method, "/"))) {dir.create(paste0(output_path, method, "/"), recursive = TRUE)}

  # Create cutoff file based on column names
  # cutoff <- paste(colnames(test)[1:(ncol(test)-1)], collapse = ",")
  # write.table(cutoff,
  #             file=paste0(output_path, "/", method, "/", data_name ,"_train.cutoff"),
  #             quote = FALSE,
  #             col.names = FALSE,
  #             row.names = FALSE)

  # Insert mandatory included features
  if (!is.null(feature_include)) {
    feature_include <- paste0("'", feature_include, "'")
  }

  time_start <- Sys.time()

  # Option 2: pre-specified settings file with input data
  rule_string <- Explore::trainExplore(output_path = file.path(output_path, "explore/"), file_name = paste0(data_name, "_train_", i),
                        train_data = train, ClassFeature = "'class'", PositiveClass = 1, FeatureInclude = feature_include, Specificity = specificity,
                        StartRulelength = start_rule_length, EndRulelength = end_rule_length, Accuracy = constraint_accuracy, Parallel = parallel)

  time_end <- Sys.time()

  ParallelLogger::logInfo(paste0("Time needed to run EXPLORE ", difftime(time_end, time_start, units = "mins")))

  # Save variables
  vars <- stringr::str_match_all(rule_string, "'\\s*(.*?)\\s*'")[[1]]
  vars <- lapply(colnames(models)[3:(ncol(models)-1)], function(c) ifelse(c %in% vars[,2], 1, 0))
  model <- c(list(method, i), vars, do.call(sum, vars))
  # TODO: add threshold instead

  ParallelLogger::logInfo(paste0(method,": ", rule_string))

  # Make predictions using EXPLORE
  pred_explore <- Explore::predictExplore(model = rule_string, test_data = test)

  return(list(pred_explore, model))
}





