
splitTrainTest <- function(data, d, output_path, frac = NULL, index = NULL) {
  if (is.null(index)) {
    # TODO: add warning if frac NULL
    index = sample(1:nrow(data), frac*nrow(data))
  }

  train = data[index,] # Create the training data
  test = data[-index,] # Create the test data

  # if (file.exists(paste0(output_path, "explore/", d, "_train.arff"))) {file.remove(paste0(output_path, "explore/", d, "_train.arff"))}
  # if (file.exists(paste0(output_path, "explore/", d, "_test.arff"))) {file.remove(paste0(output_path, "explore/", d, "_test.arff"))}

  # Remove columns with no variation in feature from train set (remove from train and test)
  # TODO: remove columns with little variation (less than 5/10?)
  if (min(sapply(train, function(c) length(unique(c)))) == 1) {
    ParallelLogger::logInfo(paste0("Train set ", d, " had ", sum((sapply(train, function(c) length(unique(c))) == 1)), " features with no variation (1 unique value) that are removed"))

    test <- test[,!(sapply(train, function(c) length(unique(c))) == 1)]
    train <- train[,!(sapply(train, function(c) length(unique(c))) == 1)]
  }

  # Write to file
  # farff::writeARFF(train, paste0(output_path, "explore/", d, "_train.arff"))
  # farff::writeARFF(test, paste0(output_path, "explore/", d, "_test.arff"))

  return(list(train,test))
}

createModel <- function(method, train = NULL, data_path = NULL, test, data_name, output_path, models, i, explore_options, bound, ...) {

  # Create model predictions
  if (method == 'lasso') {
    result <- run_lasso(method, train, test, data_name, output_path, models, i)

  } else if (method == 'randomforest') {
    result <- run_randomforest(method, train, test, data_name, output_path, models, i)

  } else if (method == 'ripper') {
    result <- run_ripper(method, train, test, data_name, output_path, models, i)

  } else if (method == 'explore') {
    if (is.null(explore_options)) {
      result <- run_EXPLORE(method, data_path, train, test, data_name, output_path, models, i, o = 0, start_rule_length = 1, end_rule_length = 4, accuracy = NULL, specificity = NULL, parallel = "yes", maximize = "ACCURACY", sorted = "yes")

      #   feature_include <- names(which.max(abs(models[method=="lasso" & iteration == i,4:(ncol(models)-1)])))
      #   result <- run_EXPLORE(method, data_path, train, test, data_name, output_path, models, i, feature_include = feature_include)

    } else {
      pred_explore <- list()
      model <- list()
      methods_output <- list()
      explore_output <- list()

      # Preparations for settings
      if (any(explore_options$Sorted == TRUE)| any(explore_options$Constraint_Accuracy == "custom")) {
        # TODO: change here to Cyclops?

        model_lasso <- glmnet::cv.glmnet(x=data.matrix(train[, -which(names(train) == "class")]), y = train$class, alpha = 1, lambda = 10^seq(3, -2, by = -.1), maxit=10000000, standardize = TRUE, nfolds = 5, family = "binomial")
        # model_lasso <- glmnet::cv.glmnet(x=data.matrix(train[, -which(names(train) == "class")]), y = train$class, alpha = 1, lambda = 10^seq(2, -3, by = -.1), standardize = FALSE, nfolds = 5, family = "binomial")
        coef <- as.matrix(coef(model_lasso, s = "lambda.min")) # get importance
        coef <- rownames(coef)[order(-abs(coef))] # order from high to low
        coef <- coef[-which(coef == "(Intercept)")] # remove intercept
        train_sorted <- train[,c(coef,"class")] # sort data features by LASSO importance
      }

      if (any(explore_options$Constraint_Accuracy == "custom")) {
        if (is.null(bound)) {
          pred_lasso <- predict(model_lasso, newx = data.matrix(train[, -which(names(train) == "class")]), type="class", s = "lambda.min")
          eval <- evaluateModel(as.numeric(pred_lasso), train$class)
          explore_options$Constraint_Accuracy[explore_options$Constraint_Accuracy == "custom"] <- eval$accuracy*0.7
        } else {
          explore_options$Constraint_Accuracy[explore_options$Constraint_Accuracy == "custom"] <- bound*0.7
        }
      }

      # Run EXPLORE with all settings
      for (o in 1:nrow(explore_options)) { # o <- 1

        if (explore_options$Sorted[o]) {
          train_input <- train_sorted
        } else {
          train_input <- train
        }

        result_o <-run_EXPLORE(method, data_path, train_input, test, data_name, output_path, models, i, o,
                               start_rule_length = explore_options$StartRulelength[o], end_rule_length = explore_options$EndRulelength[o],
                               accuracy = explore_options$Constraint_Accuracy[o], specificity = explore_options$Constraint_Specificity[o],
                               parallel = explore_options$Parallel[o],
                               maximize = explore_options$Maximize[o], sorted = explore_options$Sorted[o])

        pred_explore <- append(pred_explore, result_o[[1]])
        model <- append(model, result_o[[2]])
        methods_output <- append(methods_output, result_o[[3]])
        explore_output <- append(explore_output, result_o[[4]])
      }

      result <- list(pred_explore, model, methods_output, explore_output)
    }
  }

  return(result)
}

# createModel
# 1) pred lasso probability (in evaluation part: translate to class)
# 2) model
# 3) eval_train
# 4) model specific: bound / explore details

# https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r
#' @export
run_lasso <- function(method, train, test, data_name, output_path, models, i) {

  # Normalize data
  colMean <- apply(train[,-which(names(train) == "class")], 2, mean)
  colSD <- apply(train[,-which(names(train) == "class")], 2, sd)

  train[,-which(names(train) == "class")] <- scale_data(train[,-which(names(train) == "class")], colMean, colSD)
  test[,-which(names(test) == "class")] <- scale_data(test[,-which(names(test) == "class")], colMean, colSD)

  # Setting alpha = 1 implements lasso regression
  # TODO: change here to helper function?

  model_lasso <- lasso_glm(data.matrix(train[, -which(names(train) == "class")]), train$class, return = "model")
  # model_lasso <- glmnet::cv.glmnet(x=data.matrix(train[, -which(names(train) == "class")]), y = train$class, alpha = 1, lambda = 10^seq(3, -2, by = -.1), maxit=10000000, standardize = FALSE, nfolds = 5, family = "binomial")
  # model_lasso <- glmnet::cv.glmnet(x=data.matrix(train[, -which(names(train) == "class")]), y = train$class, alpha = 1, lambda = 10^seq(2, -3, by = -.1), standardize = FALSE, nfolds = 5, family = "binomial", maxit=100000000)
  # plot(model_lasso)

  # Save coefficients
  coef <- as.matrix(coef(model_lasso, s = "lambda.min"))
  var_names <- colnames(models)[3:(ncol(models)-1)]
  var_names[1] <- "(Intercept)"
  vars <- lapply(var_names, function(c) ifelse(c %in% rownames(coef), coef[row.names(coef) == c,1], NA))
  model <- c(list(method, i), as.list(vars), sum(sapply(vars, function(v) ifelse(v != 0, 1, 0))) - 1) # do not count intercept in model size

  coef <- coef[coef != 0,]
  ParallelLogger::logInfo(paste0(method, ": ", paste(names(coef), coef, sep = ":", collapse = ",")))

  pred_lasso <- predict(model_lasso, newx = data.matrix(test[, -which(names(test) == "class")]), type="response", s="lambda.min")

  # Create accuracy bound from performance
  # pred <- predict(model_lasso, newx = data.matrix(train[, -which(names(train) == "class")]), type="class", s = "lambda.min")
  pred <- predict(model_lasso, newx = data.matrix(train[, -which(names(train) == "class")]), type="response", s = "lambda.min")
  eval_train <- evaluateModel(prob_to_class(as.numeric(pred), train$class), train$class)
  bound <- eval_train$accuracy

  # Transform character to numeric
  pred_lasso <- as.numeric(pred_lasso)

  return(list(list(pred_lasso), list(model), list(eval_train), list(bound)))
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

  eval_train <- evaluateModel(prob_to_class(pred_randomforest, train$class), train$class)

  return(list(list(pred_randomforest), list(model), list(eval_train)))
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
  pred_ripper <- predict(model_ripper,test, type="probability") # alternative: class

  # Transform factor to numeric
  pred_ripper <- as.numeric(levels(pred_ripper))[pred_ripper]
  eval_train <- evaluateModel(prob_to_class(pred_ripper, train$class), train$class)

  return(list(list(pred_ripper), list(model), list(eval_train)))
}


#' @export
run_EXPLORE <- function(method, data_path, train, test, data_name, output_path, models, i, o, feature_include = NULL, specificity = NULL, start_rule_length = 1, end_rule_length = 3, accuracy = 0.8, parallel = "yes", maximize = "ACCURACY", sorted = "yes") {

  # Insert mandatory included features
  if (!is.null(feature_include)) {
    feature_include <- paste0("'", feature_include, "'")
  }

  time_start <- Sys.time()

  # Option 2: pre-specified settings file with input data
  # TODO: test with R.Utils::withTimeout
  rule_string <- Explore::trainExplore(output_path = file.path(output_path, "explore/"), file_name = paste0(data_name, "_train_", o, "_", i),
                                       train_data = train, ClassFeature = "'class'", PositiveClass = 1, FeatureInclude = feature_include, Specificity = specificity,
                                       StartRulelength = start_rule_length, EndRulelength = end_rule_length, Accuracy = accuracy, Parallel = parallel,
                                       Maximize = maximize)
  time_end <- Sys.time()

  # Save variables
  vars <- stringr::str_match_all(rule_string, "'\\s*(.*?)\\s*'")[[1]]
  vars <- lapply(colnames(models)[3:(ncol(models)-1)], function(c) ifelse(c %in% vars[,2], 1, 0))
  model <- c(list(method, i), vars, do.call(sum, vars))
  # TODO: add threshold instead

  ParallelLogger::logInfo(paste0(method,": ", rule_string))

  # Make predictions using EXPLORE
  pred_explore <- Explore::predictExplore(model = rule_string, test_data = test) # TEST!

  # EXPLORE output
  if (!(rule_string == "") && !is.null(rule_string)) {
    explore_model <- rule_string
    time_explore <- difftime(time_end, time_start, units = "mins")

    pred_explore_train <- Explore::predictExplore(model = rule_string, test_data = train) # TRAIN!
    eval_train <- evaluateModel(pred_explore_train, train$class)
  } else if (rule_string == "") {
    explore_model <- "model not available"
    eval_train <- NA
    time_explore <- NA
  } else if (is.null(rule_string)) {
    explore_model <- "time exceeded"
    eval_train <- NA
    time_explore <- NA
  }

  explore_output_d_o_i <- list(Time = time_explore, Model = explore_model, Data = data_name, Iteration = i, Option = o)
  return(list(list(pred_explore), list(model), list(eval_train), list(explore_output_d_o_i)))
}

