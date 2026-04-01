#' @title Probabilistic Sharpe Ratio
#'
#' @description
#' Given a predefined benchmark Sharpe ratio,the observed Sharpe Ratio
#' can be expressed in probabilistic terms known as the Probabilistic
#' Sharpe Ratio. PSR provides an adjusted estimate of SR, by removing the inflationary effect caused by short series with skewed and/or fat-tailed returns and
#' is defined as the probability of the observed sharpe ratio being higher than the reference sharpe ratio.
#'
#'
#'
#' @aliases ProbSharpeRatio
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of the returns input
#' @param Rf the risk free rate
#' @param refSR a single value or a vector when R is multicolumn. It defines the reference Sharpe Ratio and should be in the same periodicity as the returns (non-annualized).
#' @param p the confidence level
#' @param weights (if R is multicolumn and the underlying assets form a portfolio) the portfolio weights
#' @param n (if R is NULL) the track record length of the returns
#' @param sr (if R is NULL) the sharpe ratio of the returns
#' @param sk (if R is NULL) the skewness of the returns
#' @param kr (if R is NULL) the kurtosis of the returns
#' @param ignore_skewness If TRUE, it ignores the effects of skewness in the calculations
#' @param ignore_kurtosis If TRUE, it ignores the effects of kurtosis in the calculations
#' @author Tasos Grivas <tasos@@openriskcalculator.com>, Pulkit Mehrotra
#' @return A list containing the below
#' \itemize{  \item The probability that the observed Sharpe Ratio is higher than the reference one
#'  \item The p-level confidence interval of the Sharpe Ratio}
#' @references Marcos Lopez de Prado. 2018. Advances in Financial Machine Learning (1st ed.). Wiley Publishing.
#'
#'
#' @examples
#'
#' data(edhec)
#' ProbSharpeRatio(edhec[, 1], refSR = 0.23)
#' ProbSharpeRatio(refSR = 1 / 12^0.5, Rf = 0, p = 0.95, sr = 2 / 12^0.5, sk = -0.72, kr = 5.78, n = 59)
#'
#' ### Higher moments are data intensive, kurtosis shouldn't be used for short timeseries
#' ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
#' ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), ignore_skewness = FALSE, ignore_kurtosis = TRUE)
#' ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), ignore_skewness = TRUE, ignore_kurtosis = TRUE)
#'
#' ProbSharpeRatio(edhec[, 1:2],
#'   refSR = 0.26, weights = c(0.5, 0.5),
#'   ignore_skewness = FALSE, ignore_kurtosis = FALSE
#' )
#'
#' @export

ProbSharpeRatio <-
  function(R = NULL, Rf = 0, refSR, p = 0.95, weights = NULL, n = NULL, sr = NULL, sk = NULL, kr = NULL, ignore_skewness = FALSE, ignore_kurtosis = TRUE) {
    num_of_cols <- 1
    column_names <- NULL
    # Error handling if R is not NULL
    if (!is.null(R)) {
      x <- checkData(R)
      num_of_cols <- ncol(x)
      n <- nrow(x)
      # Checking if the weights are provided or not
      if (!is.null(weights)) {
        if (length(weights) != num_of_cols) {
          stop("number of items in weights is not equal to the number of columns in R")
        } else {
          # A potfolio is constructed by applying the weights
          x <- Return.portfolio(R, weights)
          num_of_cols <- 1
        }
      }

      sr <- SharpeRatio(x, Rf, p, "StdDev")
      sk <- skewness(x)
      kr <- kurtosis(x, method = "moment")
      column_names <- colnames(x)
    } else {
      if (is.null(sr) | is.null(sk) | is.null(kr) | is.null(n)) {
        stop("You must either pass R or the Sharpe ratio, Skewness, Kurtosis,n etc")
      }
      num_of_cols <- length(sr)
    }

    # Broadcast scalar refSR to vector, or validate vector length
    if (length(refSR) == 1 && num_of_cols > 1) {
      refSR <- rep(refSR, num_of_cols)
    } else if (length(refSR) != num_of_cols) {
      stop("Reference Sharpe Ratio should be a single value or given for each series")
    }

    if (!is.null(dim(Rf))) {
      Rf <- checkData(Rf)
    }

    # Flatten inputs to numeric vectors to prevent matrix dimension dropping bugs
    sr <- as.numeric(sr)
    sk <- as.numeric(sk)
    kr <- as.numeric(kr)
    refSR <- as.numeric(refSR)

    # If the Reference Sharpe Ratio is greater than the Obserred Sharpe Ratio an error is displayed
    index_of_higher_tr <- which(refSR > sr)
    if (length(index_of_higher_tr) != 0) {
      if (length(index_of_higher_tr) == num_of_cols) {
        stop("The reference Sharpe Ratio greater than the Observed Sharpe ratio for all the timeseries")
      }
      if (is.null(column_names)) dropped_names <- index_of_higher_tr else dropped_names <- column_names[index_of_higher_tr]
      warning(paste("The Reference Sharpe Ratio greater than the Observed Sharpe Ratio for the returns of: ", paste(dropped_names, collapse = ", "), "\n"))
      sr <- sr[-index_of_higher_tr]
      refSR <- refSR[-index_of_higher_tr]
      sk <- sk[-index_of_higher_tr]
      kr <- kr[-index_of_higher_tr]
      if (!is.null(column_names)) column_names <- column_names[-index_of_higher_tr]
    }
    num_of_cols <- length(sr)

    if (ignore_skewness) sk <- rep(0, num_of_cols)
    if (ignore_kurtosis) kr <- rep(3, num_of_cols)

    sr_prob <- pnorm(((sr - refSR) * ((n - 1)^(0.5))) / (1 - sr * sk + (sr^2) * (kr - 1) / 4)^(0.5))

    # Reassemble 1xN matrix for sr_prob
    sr_prob_mat <- matrix(sr_prob, nrow = 1)
    if (!is.null(column_names)) {
      colnames(sr_prob_mat) <- paste(column_names, "(SR >", round(refSR, 2), ")")
    } else {
      colnames(sr_prob_mat) <- paste("Series", 1:num_of_cols, "(SR >", round(refSR, 2), ")")
    }
    rownames(sr_prob_mat) <- paste("Probabilistic Sharpe Ratio(p=", round(p * 100, 1), "%):")

    conf_interval <- qnorm(p) * sqrt((1 - sr * sk + (sr^2) * (kr - 1) / 4) / (n - 1))

    # Reassemble clean Nx3 dataframe for confidence interval
    sr_confidence_interval <- data.frame(
      "Lower Bound" = round(sr - conf_interval, 4),
      "Sharpe Ratio" = round(sr, 4),
      "Upper Bound" = round(sr + conf_interval, 4),
      check.names = FALSE
    )
    if (!is.null(column_names)) rownames(sr_confidence_interval) <- column_names

    result <- list()
    result$sr_prob <- sr_prob_mat
    result$sr_confidence_interval <- sr_confidence_interval
    return(result)
  }
