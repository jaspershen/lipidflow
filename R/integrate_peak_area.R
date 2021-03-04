# # x = c(835.872, 836.887, 837.906, 838.923, 839.936, 840.950, 859.363)
# # y = c(29696.63, 34761.17, 44937.46, 52899.88, 46161.51, 37265.93, 20317.86)
# # 
# # integrate_under_curve_area <-
# #   function(x,
# #            y,
# #            method = c("linear", "spline"),
# #            fit_loess = FALSE) {
# #     if (length(unique(x)) < 2) {
# #       return(NA)
# #     }
# #     
# #   }
# # 
# # 
# # 
# auc = function (x,
#                 y,
#                 from = min(x, na.rm = TRUE),
#                 to = max(x, na.rm = TRUE),
#                 type = c("linear", "spline"),
#                 absolutearea = FALSE,
#                 subdivisions = 100,
#                 ...) {
#   type <- match.arg(type)
#   stopifnot(length(x) == length(y))
#   stopifnot(!is.na(from))
#   if (length(unique(x)) < 2)
#     return(NA)
#   if (type == "linear") {
#     if (absolutearea == FALSE) {
#       values <- approx(x, y, xout = sort(unique(c(from,
#                                                   to, x[x > from &
#                                                           x < to]))), ...)
#       res <- 0.5 * sum(diff(values$x) * (values$y[-1] +
#                                            values$y[-length(values$y)]))
#     } else {
#       o <- order(x)
#       ox <- x[o]
#       oy <- y[o]
#       idx <- which(diff(oy >= 0) != 0)
#       newx <-
#         c(x, x[idx] - oy[idx] * (x[idx + 1] - x[idx]) / (y[idx +
#                                                              1] - y[idx]))
#       newy <- c(y, rep(0, length(idx)))
#       values <- approx(newx, newy, xout = sort(unique(c(
#         from,
#         to, newx[newx > from &
#                    newx < to]
#       ))), ...)
#       res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) +
#                                            abs(values$y[-length(values$y)])))
#     }
#   } else {
#     if (absolutearea)
#       myfunction <- function(z) {
#         abs(splinefun(x, y, method = "natural")(z))
#       }
#     else
#       myfunction <- splinefun(x, y, method = "natural")
#     res <- integrate(
#       myfunction,
#       lower = from,
#       upper = to,
#       subdivisions = subdivisions
#     )$value
#   }
#   res
# }
# # 
# # 
# # 
# # get data
# x = xy$x
# y = xy$y
# plot(x, y, type = "b")
# 
# # using sintegral in Bolstad2
# require(Bolstad2)
# temp = sintegral(x, y)
# temp$int
# 
# sintegral(x, y)$int
# 
# # using auc in MESS
# require(MESS)
# auc(x, y, type = 'spline')
# 
# # using integrate.xy in sfsmisc
# require(sfsmisc)
# integrate.xy(x, y)
