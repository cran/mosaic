utils::globalVariables(c('pair','lwr','upr','fitted','.resid',
                         '.std.resid', '.cooksd', '.fitted', 
                         'lower', 'upper',
                         'fcoef', 'density', 'probability',
                         '.hat', 'grid.arrange',  'estimate','se')) 

#' @importFrom ggplot2 fortify
#' @importFrom stats qqnorm
# #' @importFrom broom augment
#' 
NA

#' Generic plotting
#' 
#' Generic function plotting for R objects.  Currently plots exist for 
#' `data.frame`s, `lm`s, (including `glm`s).
#' 
#' @rdname mplot
#' @param object an R object from which a plot will be constructed.
#' @param data_text text representation of the data set.  In typical use cases, the default value should suffice.
#' @export

mplot <- function(object, ...) {
  if (inherits(object, "data.frame")) {
    return(mPlot(object, ..., data_text = rlang::expr_deparse(substitute(object)))) # substitute(object))) 
  }
  
  UseMethod("mplot")
}

#' @rdname mplot
#' @export
mplot.default <- function(object, ...) {
  message("mplot() doesn't know how to handle this kind of input.")
  message('use methods("mplot") to see a list of available methods.')
}

#' @rdname mplot
#' @param data a data frame containing the variables that might be used in the plot.
# Note that for maps, the data frame must contain coordinates of the polygons 
# comprising the map and a variable for determining which coordinates are part
# of the same region.  See \code{\link{sp2df}} for one way to create such
# a data frame.  Typically \code{\link{merge}} will be used to combine the map
# data with some auxiliary data to be displayed as fill color on the map, although
# this is not necessary if all one wants is a map.
#' @param format,default default type of plot to create; one of 
#' `"scatter"`,
#' `"jitter"`,
#' `"boxplot"`,
#' `"violin"`,
#' `"histogram"`,
#' `"density"`,
#' `"frequency polygon"`,
#' or
# \code{"xyplot"}. 
# or
#' `"map"`.  
#' Unique prefixes suffice.
#' @param system which graphics system to use (initially) for plotting (\pkg{ggplot2} 
#'   or \pkg{lattice}).  A check box will allow on the fly change of plotting system.
#' @param show a logical, if `TRUE`, the code will be displayed each time the plot is 
#'   changed.
#' @return Nothing.  Just for side effects. 
#' @param which a numeric vector used to select from 7 potential plots
#' @param ask if TRUE, each plot will be displayed separately after the user 
#' responds to a prompt.
#' @param multiplot if TRUE and `ask == FALSE`, all plots will be 
#' displayed together.
#' @param title title for plot
#' @param ... additional arguments.  If `object` is an `lm`, subsets
#' of these arguments are passed to `gridExtra::grid.arrange` and to the 
#' \pkg{lattice} plotting routines; in particular,
#' `nrow` and `ncol` can be used to control the number of rows
#' and columns used.
#' @param id.nudge a numeric used to increase (>1) or decrease (<1) the amount that observation labels are 
#' nudged.  Use a negative value to nudge down instead of up.
#' @param id.n Number of id labels to display.
#' @param id.size Size of id labels.
#' @param id.color Color of id labels.
#' @param add.smooth A logicial indicating whether a LOESS smooth should be added 
#'   (where this makes sense to do).
#'   Currently ignored for lattice plots.
#' @param span A positive number indicating the amount of smoothing. 
#'   A larger number indicates more smoothing. See [`stats::loess()`] for details.
#'   Currently ignored for lattice plots.
#' @param smooth.color,smooth.size,smooth.alpha Color, size, and alpha used for 
#'   LOESS curve.  Currently ignored for lattice plots.
#' @details
#' The method for models (lm and glm) is still a work in progress, but should be usable for 
#' relatively simple models.  When the results for a logistic regression model created with
#' [glm()] are satisfactory will depend on the format and structure of the data
#' used to fit the model.
#' 
#' Due to a bug in RStudio 1.3, the method for data frames may not display the controls
#' consistently.  We have found that executing this code usually fixes the problem:
#' 
#' ```
#' library(manipulate) 
#' manipulate(plot(A), A = slider(1, 10))
#' ```
#' 
#' 
#' @examples
#' lm( width ~ length * sex, data = KidsFeet) |>
#'   mplot(which = 1:3, id.n = 5)
#' lm( width ~ length * sex, data = KidsFeet) |>
#'   mplot(smooth.color = "blue", smooth.size = 1.2, smooth.alpha = 0.3, id.size = 3)
#' lm(width ~ length * sex, data = KidsFeet) |>
#'   mplot(rows = 2:3, which = 7)
# #' @importFrom ggrepel geom_text_repel
#' @export

mplot.lm <- 
  function(
    object, 
    which = c(1:3, 7), 
    system = c("ggplot2", "lattice", "base"),
    ask = FALSE, 
    multiplot = "package:gridExtra" %in% search(),
    par.settings = theme.mosaic(),
    level = .95,
    title = paste("model: ", deparse(object$call), "\n"),
    rows = TRUE,
    id.n = 3L,
    id.size = 5,
    id.color = "red",
    id.nudge = 1,
    add.smooth = TRUE, 
    smooth.color = "red",
    smooth.alpha = 0.6,
    smooth.size = 0.7,
    span = 3/4, 
    ...) {
  
  system <- match.arg(system)
  check_installed('ggrepel')
  
  geom_smooth_or_not <-  
    if (add.smooth)
      geom_line(stat = "smooth", method = "loess", span = span, 
                alpha = smooth.alpha, color = smooth.color, linewidth = smooth.size) 
    else
      geom_blank() 
  
  dots <- list(...)
  if ("col" %in% names(dots)) {
    dots$col <- dots$col[1]
  }
  
  if (multiplot && ! "package:gridExtra" %in% search()) {
    message("multiplot = TRUE only works when 'gridExtra' is loaded.")
    message("    I'm setting multiplot = FALSE and continuing.")
    multiplot <- FALSE
  }
  
  if (system == "base") {
    return(plot( object, which = intersect(which, 1:6)))
  }
 
  rlang::check_installed('broom') 
  fdata <- broom::augment(object) 
  fdata <- 
    fdata |> 
    mutate(
      .row = 1L:nrow(fdata)
    )
 
  # broom::augment() does always supply .resid :-/
  
  if (is.null(fdata[[".resid"]])) {
    fdata <- fdata |> mutate(.resid = resid(object))
  }
  
  fdata_clean <- fdata |> filter(!is.na(.std.resid))
  
  removed_idx <- which(fdata$.hat >= 1)
  if (any(c(2, 3, 5, 6) %in% which) && length(removed_idx)) {
    warning("Observations with leverage 1 not plotted: ", 
            paste(removed_idx, collapse = ", "),
            call. = FALSE)
  }
    
#  fdata <- cbind(fdata, row = 1:nrow(fdata))
  
  if (!inherits(object, "lm")) 
    stop("use only with \"lm\" objects")
  if (!is.numeric(which) || any(which < 1) || any(which > 7)) 
    stop("'which' must be in 1:7")
  isGlm <- inherits(object, "glm")
  show <- rep(FALSE, 7)
  show[which] <- TRUE
  
  ylab23 <- if (isGlm) 
    "Std. deviance resid."
  else "Standardized residuals"
  
  
  # residuals vs fitted
  g1 <- ggplot(fdata, aes(.fitted, .resid)) +
    geom_point()  +
    geom_smooth_or_not +
    geom_hline(linetype = 2, linewidth = .2, yintercept = 0) +
    ggrepel::geom_text_repel(
      data = fdata |> arrange(-abs(.std.resid)) |> head(id.n),
      aes(label = .row), 
      color = id.color,
      segment.color = id.color,
      size = id.size) + 
    scale_x_continuous("Fitted Value") +
    scale_y_continuous("Residual") +
    labs(title = "Residuals vs Fitted")
  
  l1 <- do.call(xyplot, 
                c(list( .std.resid ~ .fitted, data = fdata,
                        type = c("p","smooth"),
                        panel = function(x,y,...) {
                          panel.abline(h = 0, linetype = 2, lwd = .5) 
                          panel.xyplot(x,y,...)
                        },
                        main = "Residuals vs Fitted",
                        xlab = "Fitted Value",
                        ylab = "Residual",
                        par.settings = par.settings),
                  dots)
  )
  
  # normal qq
  # remove NAs and NaNs before computing quantiles
  
  a <- quantile(fdata$.std.resid, c(0.25, 0.75), na.rm = TRUE)
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  QN <- 
    as.data.frame(qqnorm(fdata$.std.resid, plot.it = FALSE)) |> 
    mutate(.row = 1:nrow(fdata))
  g2 <- ggplot(fdata_clean, aes(sample = .std.resid)) +
    stat_qq() +
    geom_abline(slope = slope, intercept = int, linetype = "dashed") +
    ggrepel::geom_text_repel(
      inherit.aes = FALSE,
      data = QN |> arrange(-abs(y)) |> head(id.n),
      aes(y = y,  x = x, label = .row),
      color = id.color,
      segment.color = id.color,
      size = id.size) + 
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Standardized Residuals") +
    labs(title = "Normal Q-Q")
  
  l2 <- do.call(qqmath, 
                c(list( ~ .std.resid, data = fdata_clean, 
                        panel = function(x,...) {
                          panel.abline(a = int, b = slope)
                          panel.qqmath(x,...)
                        },
                        main = "Normal Q-Q",
                        xlab = "Theoretical Quantiles",
                        ylab = ylab23,
                        par.settings = par.settings),
                  dots)
  )
  
  # scale-location
  
  g3 <- ggplot(fdata_clean, aes(.fitted, sqrt(abs(.std.resid)))) +
    geom_point() +
    geom_smooth_or_not +
    ggrepel::geom_text_repel(
      data = fdata_clean |> arrange(-abs(.std.resid)) |> head(id.n),
      aes(label = .row), 
      color = id.color,
      segment.color = id.color,
      size = id.size) + 
    scale_x_continuous("Fitted Values") +
    scale_y_continuous(as.expression(
      substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))) )) +
    labs(title = "Scale-Location")
  
  l3 <- do.call(xyplot, 
                c(list( sqrt(abs(.std.resid)) ~ .fitted, data = fdata_clean,
                        type = c("p","smooth"),
                        main = "Scale-Location",
                        xlab = "Fitted Value",
                        ylab = as.expression(
                          substitute(sqrt(abs(YL)), list(YL = as.name(ylab23)))
                        ),
                        par.settings = par.settings),
                  dots)  
  )
  
  # cook's distance
  g4 <-  
    ggplot(data = fdata, aes(.row, .cooksd, ymin = 0, ymax = .cooksd)) +
    geom_point() + 
    geom_linerange() +
    scale_x_continuous("Observation Number", limits = c(0, NA)) +
    scale_y_continuous("Cook's distance") +
    labs(title = "Cook's Distance") 
  if (id.n > 0L) {
    g4 <- g4 + 
      ggrepel::geom_text_repel(
        data = fdata |> arrange(-abs(.cooksd)) |> head(id.n),
        aes(x = .row, y = .cooksd, label = .row),
        color = id.color,
        segment.color = id.color,
        size = id.size) 
  }
  
  l4 <- do.call( xyplot,
                 c(list( .cooksd ~ .row, data = fdata, 
                         type = c("p","h"),
                         main = "Cook's Distance",
                         xlab = "Observation number",
                         ylab = "Cook's distance",
                         par.settings = par.settings),
                   dots)  
  )
  
  # residuals vs leverage
  g5 <- 
    ggplot(fdata_clean, aes(x = .hat, y = .std.resid)) +
    geom_point() +
    geom_smooth_or_not +
    ggrepel::geom_text_repel(
      data = fdata_clean |> arrange(-abs(.std.resid)) |> head(id.n),
      aes(label = .row), 
      color = id.color,
      segment.color = id.color,
      size = id.size) + 
    geom_hline(linetype = 2, linewidth = .2, yintercept = 0) +
    labs(title = "Residuals vs Leverage",
         x     = "Leverage",
         y     = "Standardized Residual") +
    lims(x = c(0, NA))
  
  l5 <- do.call( xyplot, 
                 c(list( .std.resid ~ .hat, data = fdata_clean,
                         type = c('p','smooth'),
                         panel = function(x,y,...) {
                           panel.abline( h = 0, lty = 2, lwd = .5)
                           panel.xyplot( x, y, ...)
                         },
                         main = "Residuals vs Leverage",
                         xlab = "Leverage",
                         ylab = "Standardized Residuals",
                         par.settings = par.settings),
                   dots)
  )
  
  # cooksd vs leverage
  g6 <- ggplot(fdata_clean, aes(.hat, .cooksd)) +
    geom_point() +
    geom_smooth_or_not +
    ggrepel::geom_text_repel(
      data = fdata_clean |> arrange(-abs(.std.resid)) |> head(id.n),
      aes(label = .row), 
      color = id.color,
      segment.color = id.color,
      size = id.size) + 
    scale_x_continuous("Leverage") +
    scale_y_continuous("Cook's distance") +
    labs(title = "Cook's dist vs Leverage")
  
  l6 <- do.call(xyplot,
                c(list( .cooksd ~ .hat, data = fdata_clean,
                        type = c("p", "smooth"),
                        main = "Cook's dist vs Leverage",
                        xlab = "Leverage",
                        ylab = "Cook's distance",
                        par.settings = par.settings),
                  dots)
  )

  g7 <- mplot(summary(object), level = level, rows = rows, ..., system = "ggplot2")
  
  l7 <- mplot(summary(object), level = level, rows = rows, ..., system = "lattice")
  
  plots <- if (system == "ggplot2") {
    list(g1, g2, g3, g4, g5, g6, g7)
  } else {
    lapply( list(l1, l2, l3, l4, l5, l6, l7), 
            function(x) update(x, par.settings = par.settings))
  }
  
  plots <- plots[which] 
  
  if (ask) {
    for (p in plots) {
      readline("Hit <RETURN> for next plot")
      print(p)
    }
  } 
  if (multiplot) {
    rlang::check_installed('gridExtra')
    dots <- list(...)
    nn <- intersect( 
      union(names(formals(gridExtra::arrangeGrob)), names(formals(grid.layout))),
      names(dots) 
    )
    dots <- dots[ nn ]
    return(do.call(gridExtra::grid.arrange, c(plots, dots)))
    result <-  do.call(
      gridExtra::arrangeGrob, 
      c(plots, dots) # , c(list(main = title), dots))
    )
    plot(result)
    return(result)
  }

# Question: should a single plot be returned as is or in a list of length 1?
if (length(plots) == 1) {
  return(plots[[1]])
}
  
  return(plots)
}

#' @rdname mplot
#' @examples
#' \dontrun{
#' mplot( HELPrct )
#' mplot( HELPrct, "histogram" )
#' }
#' @export

mplot.data.frame <- 
  function(
    object, format, default = format, 
    system = c("ggformula", "ggplot2", "lattice"),  
    show = FALSE, 
    data_text = rlang::expr_deparse(substitute(object)),
    # data_text = substitute(object),
    title = "", ...
  ) {
  print(data_text)
  return(
    mPlot(object, format = format, default = default, system = system, 
        show = show, title = title, data_text = data_text, ...)
  )
}  
#   plotTypes <- c('scatter', 'jitter', 'boxplot', 'violin', 'histogram', 
#                  'density', 'frequency polygon', 'xyplot')
#   if (missing(default) & missing(format)) {
#     choice <- 
#       menu(title = "Choose a plot type.",
#            choices = c(
#              "1-variable (histogram, density plot, etc.)",
#              "2-variable (scatter, boxplot, etc.)" 
#            )
#       )
#     default <- c("histogram", "scatter") [choice]
#   }
#   default <- match.arg(default, plotTypes)
#   system <- match.arg(system)
# 
#   dataName <- substitute(object)
#   if (default == "xyplot") 
#     default <- "scatter"
#   if (default %in% c("scatter", "jitter", "boxplot", "violin")) {
#     return(
#       mScatter(lazy_data, default = default, system = system, show = show, title = title)
#     )
#   }
# #   if (default == "map") {
# #     return(eval(parse(
# #       text = paste("mMap(", dataName, 
# #                    ", default = default, system = system, show = show, title = title)"))
# #     ))
# #   }
#   return(eval(parse(
#     text = paste("mUniplot(", dataName, 
#                  ", default = default, system = system, show = show, title = title)"))
#   ))
# }


#' Extract data from R objects
#' 
#' @rdname fortify
#' @param level confidence level
#' @param ... additional arguments
#' @export

fortify.summary.lm <- function(model, data = NULL, level = 0.95, ...) {
  E <- as.data.frame(coef(model, level = level))
  # grab only part of the third name that comes before space
  statName <- strsplit(names(E)[3], split = " ")[[1]][1]
  names(E) <- c("estimate", "se", "stat", "pval")
  # add coefficient names to data frame
  E$coef <- row.names(E)
  E$statName <- statName
  E$lower <- confint(model, level = level, ...)[,1]
  E$upper <- confint(model, level = level, ...)[,2]
  E$level <- level
  return(E)
}

#' @rdname fortify
#' @export

fortify.summary.glm <- function(model, data = NULL, level = 0.95, ...) {
  E <- as.data.frame(coef(model, level = level))
  # grab only part of the third name that comes before space
  statName <- strsplit(names(E)[3], split = " ")[[1]][1]
  names(E) <- c("estimate", "se", "stat", "pval")
  # add coefficient names to data frame
  E$coef <- row.names(E)
  E$statName <- statName
  E <- mutate(E, 
              lower = estimate + qnorm((1-level)/2) * se,
              upper = estimate + qnorm(1-(1-level)/2) * se,
              level = level)
  return(E)
}

#' @rdname confint
#' @param object and R object
#' @param parm a vector of parameters
#' @param level a confidence level
#' @examples
#' lm(width ~ length * sex, data = KidsFeet) |>
#'   summary() |>
#'   confint()
#' @export

confint.summary.lm <- function (object, parm, level = 0.95, ...)  {
  cf <- coef(object)[, 1]
  pnames <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qt(a, object$df[2])
  pct <- paste( format(100*a, digits = 3, trim = TRUE, scientific = FALSE), "%" )
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                             pct))
  ses <- sqrt(diag(vcov(object)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}

#' @rdname mplot
#' @param level a confidence level
#' @param par.settings \pkg{lattice} theme settings 
#' @param rows rows to show.  This may be a numeric vector, 
#' `TRUE` (for all rows), or a character vector of row names.
#' @examples
#' lm(width ~ length * sex, data = KidsFeet) |>
#'   summary() |>
#'   mplot()
#'   
#' lm(width ~ length * sex, data = KidsFeet) |>
#'   summary() |>
#'   mplot(rows = c("sex", "length"))
#'   
#' lm(width ~ length * sex, data = KidsFeet) |>
#'   summary() |>
#'   mplot(rows = TRUE)
#' @export
 
mplot.summary.lm <- function(object, 
                             system = c("ggplot2", "lattice"),
                             level = 0.95,
                             par.settings = trellis.par.get(),
                             rows = TRUE,
                             ...){
  system <- match.arg(system)
  fdata <- fortify(object, level = level) |> 
    mutate(signif = pval < (1-level),
           fcoef = factor(coef, levels = coef)
           )
  row.names(fdata) <- fdata$coef
  fdata <- fdata[rows, ]
  fdata <- fdata[nrow(fdata):1, ]
  
  g <- ggplot(data = fdata,
              aes(x = fcoef, y = estimate, 
                  ymin = lower, ymax = upper, 
                  color = signif)) + # (pval < (1-level)/2))) + 
    geom_pointrange(size = 1.2) + 
    geom_hline(yintercept = 0, color = "red", alpha = .5, linetype = 2) + 
    labs(x = "coefficient", title = paste0(format(100*level), "% confidence intervals") ) +
    theme(legend.position = "none") +
    coord_flip()
  
  cols <- rep( par.settings$superpose.line$col, length.out = 2)
  cols <- cols[2 - fdata$signif]
  
  l <- xyplot( fcoef ~ estimate + lower + upper,
               data = fdata,
               fdata = fdata,
               xlab = "estimate",
               ylab = "coefficient",
               main = paste0(format(100 * level), "% confidence intervals"),
               ...,
               panel = function(x, y, fdata, ...) {
                 dots <- list(...)
                 if ("col" %in% names(dots)) {
                   dots$col <- rep(dots$col, length.out = 2) [2 - fdata$signif]
                 } 
                 dots <- .updateList(
                   list(lwd = 2, alpha = 0.6, cex = 1.4, col = cols),
                   dots
                 )
                 dots[["type"]] <- NULL
                                     
                 panel.abline(v = 0, col = "red", alpha = .5, lty = 2) 
                 do.call( panel.points,
                          c( list (x = fdata$estimate, y = y), 
                             dots )
                 )
                 do.call( panel.segments, 
                          c( list(y0 = y, y1 = y, x0 = fdata$lower, 
                                  x1 = fdata$upper),
                             dots )
                 )
               }
  )
  
  if (system == "ggplot2") {
    return(g)
  } else { 
    return(l)
  }
}


#' @export
 
mplot.summary.glm <- mplot.summary.lm

#' @rdname fortify
#' @param model an R object
#' @param data original data set, if needed
#' @param order one of `"pval"`, `"diff"`, or `"asis"` determining the 
#'   order of the `pair` factor, which determines the order in which the differences
#'   are displayed on the plot.
#' @export
 
# fortify.TukeyHSD <- function(model, data, ...) {
#   nms <- names(model)
#   l <- length(model)
#   plotData <- do.call( 
#     rbind, 
#     lapply(seq_len(l), function(i) {
#       res <- transform(as.data.frame(model[[i]]), 
#                        var = nms[[i]], 
#                        pair = row.names(model[[i]]) ) 
#     } ) 
#   )
#   names(plotData) <- c("diff", "lwr", "upr", "pval", "var", "pair")
#   return(plotData)
# }  

fortify.TukeyHSD <- function(model, data, order = c("asis", "pval", "difference"), ...) {
  order <- match.arg(order)
  nms <- names(model)
  l <- length(model)
  plotData <- do.call( 
    rbind, 
    lapply(seq_len(l), function(i) {
      res <- transform(as.data.frame(model[[i]]), 
                       var = nms[[i]], 
                       pair = row.names(model[[i]]) ) 
    } ) 
  )
  names(plotData) <- c("diff", "lwr", "upr", "pval", "var", "pair")
  plotData <-
    plotData |>
    mutate(pair = 
             switch(order,
                    "asis" = reorder(pair, 1:nrow(plotData)),
                    "pval" = reorder(pair, pval),
                    "difference" = reorder(pair, diff),
                    )
    )
  
  return(plotData)
}  

#' @rdname mplot
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param order one of `"pval"`, `"diff"`, or `"asis"` determining the 
#'   order of the `pair` factor, which determines the order in which the differences
#'   are displayed on the plot.
#' @examples
#' lm(age ~ substance, data = HELPrct) |>
#'   TukeyHSD() |>
#'   mplot()
#' lm(age ~ substance, data = HELPrct) |>
#'   TukeyHSD() |>
#'   mplot(system = "lattice")
#' @export

mplot.TukeyHSD <- function(object, system = c("ggplot2", "lattice"), 
                           ylab = "", xlab = "difference in means", 
                           title = paste0(attr(object, "conf.level") * 100, "% family-wise confidence level"),
                           par.settings = trellis.par.get(),
                           order = c("asis", "pval", "difference"),
                           # which = 1L:2L,
                           ...) {
  system <- match.arg(system)
  order <- match.arg(order) 
  fdata <- fortify(object, order = order)
 
  # res <- list()
  
  if (system == "ggplot2") {
    # if (1 %in% which) {
      p1 <-
        ggplot( data = fdata,
                aes(x = diff, color = log10(pval), y = factor(pair, levels = rev(levels(pair)))) ) +
        geom_point(size = 2) +
        geom_segment(aes(x = lwr, xend = upr, y = pair, yend = pair) ) +
        geom_vline( xintercept = 0, color = "red", linetype = 2, alpha = .5 ) + 
        facet_grid( var ~ ., scales = "free_y") +
        labs(x = xlab, y = ylab, title = title) 
    #   res <- c(res, list(p1))
    # }
    # if (2 %in% which) {
    #   p2 <- 
    #     ggplot( data = fdata,
    #             aes(x = diff, color = log10(pval), y = factor(pair, levels = rev(levels(pair)))) ) +
    #     geom_point(size = 2) +
    #     geom_segment(aes(x = lwr, xend = upr, y = pair, yend = pair) ) +
    #     geom_vline( xintercept = 0, color = "red", linetype = 2, alpha = .5 ) + 
    #     facet_grid( var ~ ., scales = "free_y") +
    #     labs(x = xlab, y = ylab, title = title) 
    #   res <- c(res, list(p2))
    # }
  return(p1)
  }

  cols <- par.settings$superpose.line$col[1 + 
            as.numeric( sign(fdata$lwr) * sign(fdata$upr) < 0)]
  
  xyplot( factor(pair, levels = rev(levels(pair))) ~ diff + lwr + upr | var, data = fdata, 
          panel = function(x,y,subscripts,...) {
            n <- length(x)
            m <- round(n/3)
            panel.abline(v = 0, col = "red", lty = 2, alpha = .5)
            panel.segments(x0 = x[(m+1):(2*m)], x1 = x[(2*m+1):(3*m)], y0 = y, y1 = y, col = cols[subscripts])
            panel.xyplot(x[1:m], y, cex = 1.4, pch = 16, col = cols[subscripts])
          },
          scales = list( y = list(relation = "free", rot = 30) ),
          xlab = xlab,
          ylab = ylab,
          main = title,
          ...
  )
}

