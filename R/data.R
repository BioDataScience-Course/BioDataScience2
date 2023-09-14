#' Rice Dataset Commeo and Osmancik
#'
#' @description
#' A total of 3810 images of rice grains were taken for two varieties (Cammeo
#' and Osmancik). The images are then processed and feature were extracted in a
#' table. Seven morphological features were obtained for each grain of rice.
#'
#' @format A data frame with 8 variables and 3810 observations:
#' \describe{
#'   \item{\code{area}}{The number of pixels within the boundaries of the
#'     rice grain.}
#'   \item{\code{perimeter}}{The perimeter of the rice grain.}
#'   \item{\code{major_axis_length}}{The longest line that can be drawn on the
#'     rice  grain.}
#'   \item{\code{minor_axis_length}}{The shortest line that can be drawn on the
#'     rice  grain.}
#'   \item{\code{eccentricity}}{It measures how round the ellipse, which has
#'     the same moments as the rice grain.}
#'   \item{\code{convex_area}}{The pixel count of the smallest convex hull of
#'     the region formed by the rice grain.}
#'   \item{\code{extent}}{the ratio of the region formed by the rice grain to
#'     the bounding box pixels.}
#'   \item{\code{class}}{A **factor** with two levels: `"Cammeo"`, and
#'     `"Osmancik"`.}
#' }
#' @source {Cinar, I. and Koklu, M. (2019). Classification of Rice Varieties
#'   Using Artificial Intelligence Methods. International Journal of Intelligent
#'   Systems and Applications in Engineering, vol.7, no.3 (Sep. 2019),
#'   pp.188-194. doi:10.18201/ijisae.2019355381}
"rice"
