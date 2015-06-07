# Modify plotMeans so that we can customize cex.axis
# plotMeans is originally from package Rcmdr
plotMeans.mod <- function (response, factor1, factor2, error.bars = c("se", "sd",
                                                                      "conf.int", "none"), level = 0.95, xlab = deparse(substitute(factor1)),
                           ylab = paste("mean of", deparse(substitute(response))), legend.lab = deparse(substitute(factor2)),
                           main = "Plot of Means", pch = 1:n.levs.2, lty = 1:n.levs.2,
                           col = palette(), cex.axis=1, ...)
{
  if (!is.numeric(response))
    stop("Argument response must be numeric.")
  xlab
  ylab
  legend.lab
  error.bars <- match.arg(error.bars)
  if (missing(factor2)) {
    if (!is.factor(factor1))
      stop("Argument factor1 must be a factor.")
    valid <- complete.cases(factor1, response)
    factor1 <- factor1[valid]
    response <- response[valid]
    means <- tapply(response, factor1, mean)
    sds <- tapply(response, factor1, sd)
    ns <- tapply(response, factor1, length)
    if (error.bars == "se")
      sds <- sds/sqrt(ns)
    if (error.bars == "conf.int")
      sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
      sds/sqrt(ns)
    sds[is.na(sds)] <- 0
    yrange <- if (error.bars != "none")
      c(min(means - sds, na.rm = TRUE), max(means + sds,
                                            na.rm = TRUE))
    else range(means, na.rm = TRUE)
    levs <- levels(factor1)
    n.levs <- length(levs)
    plot(c(1, n.levs), yrange, type = "n", xlab = xlab, ylab = ylab,
         axes = FALSE, main = main, ...)
    points(1:n.levs, means, type = "b", pch = 16, cex = 2)
    box()
    axis(2)
    axis(1, at = 1:n.levs, labels = levs, cex.axis=cex.axis)
    if (error.bars != "none")
      arrows(1:n.levs, means - sds, 1:n.levs, means + sds,
             angle = 90, lty = 2, code = 3, length = 0.125)
  }
  else {
    if (!(is.factor(factor1) | is.factor(factor2)))
      stop("Arguments factor1 and factor2 must be factors.")
    valid <- complete.cases(factor1, factor2, response)
    factor1 <- factor1[valid]
    factor2 <- factor2[valid]
    response <- response[valid]
    means <- tapply(response, list(factor1, factor2), mean)
    sds <- tapply(response, list(factor1, factor2), sd)
    ns <- tapply(response, list(factor1, factor2), length)
    if (error.bars == "se")
      sds <- sds/sqrt(ns)
    if (error.bars == "conf.int")
      sds <- qt((1 - level)/2, df = ns - 1, lower.tail = FALSE) *
      sds/sqrt(ns)
    sds[is.na(sds)] <- 0
    yrange <- if (error.bars != "none")
      c(min(means - sds, na.rm = TRUE), max(means + sds,
                                            na.rm = TRUE))
    else range(means, na.rm = TRUE)
    levs.1 <- levels(factor1)
    levs.2 <- levels(factor2)
    n.levs.1 <- length(levs.1)
    n.levs.2 <- length(levs.2)
    if (length(pch) == 1)
      pch <- rep(pch, n.levs.2)
    if (length(col) == 1)
      col <- rep(col, n.levs.2)
    if (length(lty) == 1)
      lty <- rep(lty, n.levs.2)
    if (n.levs.2 > length(col))
      stop(sprintf("Number of groups for factor2, %d, exceeds number of distinct colours, %d."),
           n.levs.2, length(col))
    plot(c(1, n.levs.1 * 1.4), yrange, type = "n", xlab = xlab,
         ylab = ylab, axes = FALSE, main = main, ...)
    box()
    axis(2)
    axis(1, at = 1:n.levs.1, labels = levs.1, cex.axis=cex.axis)
    for (i in 1:n.levs.2) {
      points(1:n.levs.1, means[, i], type = "b", pch = pch[i],
             cex = 2, col = col[i], lty = lty[i])
      if (error.bars != "none")
        arrows(1:n.levs.1, means[, i] - sds[, i], 1:n.levs.1,
               means[, i] + sds[, i], angle = 90, code = 3,
               col = col[i], lty = lty[i], length = 0.125)
    }
    x.posn <- n.levs.1 * 1.1
    y.posn <- sum(c(0.1, 0.9) * par("usr")[c(3, 4)])
    text(x.posn, y.posn, legend.lab, adj = c(0, -0.5))
    legend(x.posn, y.posn, levs.2, pch = pch, col = col,
           lty = lty)
  }
  invisible(NULL)
}