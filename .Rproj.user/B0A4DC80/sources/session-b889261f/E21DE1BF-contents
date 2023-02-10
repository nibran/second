# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


ChurnProbabilityCust <- function(custdata, ID){
  if (ID %in% custdata[,CustomerId]){
    model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
                 data = custdata,
                 family = "binomial")

    custdata$churnprob <-predict(model, custdata, type = "response")

    CustomerChurnProb <- custdata[ CustomerId == ID, list(CustomerId,churnprob)]
    return(CustomerChurnProb[,churnprob])
  }else{stop("ERROR the Customer does not exist")

  }}




