#' SVM  Estimation
#' @export
svmEstimation <- function(train.data.set,
                                labels = c("Region"), categorical.var = c("SampleId"),
                                which.kernel = "RBF",  costs = c(10, 100, 1000, 10000), sigmas = 10^(-2:1), kCV = 10){

            theObject <- svmObj()
            cross.errors <- matrix(nrow = length(costs), ncol = 2)
            colnames(cross.errors) <- c("Error", "nSV")
            rownames(cross.errors) <- costs

            cross.errors.radial <- matrix(nrow = length(sigmas), ncol = length(costs))
            colnames(cross.errors.radial) <- costs
            rownames(cross.errors.radial) <- sigmas

            nSV.radial <- matrix(nrow = length(sigmas), ncol = length(costs))
            colnames(nSV.radial) <- costs
            rownames(nSV.radial) <- sigmas

            #kCV <- nrow(train.data.set) # the number of samples for cross-validation, nrow(train.data.set) means LOOCV

            train.data <- train.data.set[, !names(train.data.set) %in% categorical.var]

            for (myC in costs){
              error <- 0
              switch(which.kernel,
                     linear = {
                       model <-  ksvm(Region~ .,data = train.data, kernel = 'polydot', C = myC, cross = kCV)
                       cross.errors[toString(myC), "Error"] <- cross(model) * 100
                       cross.errors[toString(myC), "nSV"] <- nSV(model) * 100},
                     quadratic = {
                       for (g in sigmas) {
                         quad <- polydot(degree = 2, scale = 1, offset = 1)
                         model <-  ksvm(Region~ .,data = train.data, kernel = quad, sigma = g, C = myC, cross = kCV)
                         cross.errors.radial[toString(g), toString(myC)] <- cross(model) * 100
                         nSV.radial[toString(g), toString(myC)] <- nSV(model)}},
                     RBF = {
                       for (g in sigmas){
                         model <- ksvm(Region~ .,data = train.data, C = myC, sigma = g, cross = kCV)
                         cross.errors.radial[toString(g), toString(myC)] <- cross(model) * 100
                         nSV.radial[toString(g), toString(myC)] <- nSV(model)}},
                     stop("Enter one of 'linear', 'quadratic', 'RBF'"))}
            if(which.kernel == "linear")
              theObject@cross.errors <- cross.errors
            else{
              theObject@cross.errors <- cross.errors.radial
              theObject@nSV <- nSV.radial}
            return(theObject)}

#' SVM prediction
#' @export
svmPrediction <- function(train.data.set, test.data.set, labels = c("Region"), categorical.var = c("SampleId"),  which.kernel = "RBF", cost = 10, sigma = 1){
            theObject <- svmObj()
            theObject@cost <- cost
            theObject@sigma <- sigma

            train.data <- splitData(train.data.set, labels, categorical.var)
            train.class <- train.data.set[, names(train.data.set) %in% labels]

            test.data <- splitData(test.data.set, labels, categorical.var)
            test.class <- test.data.set[, names(test.data.set) %in% labels]

            # svm.model <- svm(Region ~., as.data.frame(train.data[, -1]), type = "C-classification", gamma = 0.1, cost = 10, kernel = "radial", scale = FALSE, probability=TRUE)
            switch(which.kernel,
                   linear    = {model <- svm(train.data, train.class, type = "C-classification", cost = theObject@cost, kernel = "linear", scale = FALSE)},
                   quadratic = {model <- svm(train.data, train.class, type="C-classification", cost = theObject@cost, gamma = theObject@sigma, kernel = "polynomial", degree = 2, coef0 = 1, scale = FALSE)},
                   RBF       = {model <- svm(train.data, train.class, type = "C-classification", gamma = theObject@sigma, cost = theObject@cost, kernel = "radial", scale = FALSE)},
                   stop("Enter one of 'linear', 'quadratic', 'RBF'")
            )

            theObject@model <- model
            # Training set
            prediction <- predict(model, train.data)
            theObject@metrics.train <- setClassMetrics(theObject@metrics.train, prediction, train.class)

            # Test set
            prediction <- predict(model, newdata = test.data)
            theObject@metrics.test <- setClassMetrics(theObject@metrics.test, prediction, test.class)

            return(theObject)
          }
