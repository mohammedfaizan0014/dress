#' wage4
#'
#' @description
#'  Wage and other data for a group of 3000 male workers in the Mid-Atlantic region.
#'
#' @format A data frame with 3000 observations and 4 variables:
#' \describe{
#'   \item{age}{Age of worker}
#'   \item{education}{A factor with levels 1. < HS Grad 2. HS Grad 3. Some College 4. College Grad and 5. Advanced Degree indicating education level}
#'   \item{jobclass}{A factor with levels 1. Industrial and 2. Information indicating type of job}
#'   \item{wage}{Workers raw wage}
#'   ...
#' }
#'
#' @source
#' Data was manually assembled by Steve Miller, of Inquidia Consulting (formerly Open BI). From the March 2011 Supplement to Current Population Survey data.
#' \url{'https://www.re3data.org/repository/r3d100011860'}
#'
#' @references
#' James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013) An Introduction to Statistical Learning with applications in R, https://www.statlearning.com, Springer-Verlag, New York
#'
#' @seealso \code{\link[ISLR]{Wage}}
#' @examples
#' head(wage4)
"wage4"
