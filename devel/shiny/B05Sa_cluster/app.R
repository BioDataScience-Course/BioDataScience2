learndown::learndownShinyVersion("0.0.9000")
conf <- BioDataScience::config()

library(shiny)
library(learndown)
library(BioDataScience2)

# add news functions ----
## This function move to a package

dissimilarity <- function(data, formula = ~ ., subset = NULL,
                          method = "euclidean", scale = FALSE, rownames.col = "rowname",
                          transpose = FALSE, fun = NULL, ...) {
  # TODO: get more meaningful warnings and errors by replacing fun by actual
  # name of the function
  if (is.null(fun)) {# Default function depends on the chosen method
    if (method %in% c("maximum", "binary", "minkowski")) {
      fun <- stats::dist
    } else {
      fun <- vegan::vegdist # Note: euclidean/manhattan/canberra in both, but
      # we prioritize vegdist, and canberra is not calculated the same in dist!
    }
  }
  # We accept only formulas with right-hand side => length must be two
  if (length(formula) == 3)
    stop("The formula cannot have a left-hand term")

  # With matrices, we don't use rownames.col: rownames are already correctly set
  if (!is.matrix(data)) {# row names may be in a column (usual for tibbles)
    data <- as.data.frame(data)
    if (rownames.col %in% names(data)) {
      rownames(data) <- data[[rownames.col]]
      data[[rownames.col]] <- NULL
    } else {# rownames.col is NOT used
      rownames.col <- NULL
    }
    if (as.character(formula[2] != ".")) {
      # Subset the columns
      data <- model.frame(formula, data = data, subset = subset)
    } else if (!is.null(subset)) {
      data <- data[subset, ]
    }
  } else {# A matrix
    rownames.col <- NULL
    if (as.character(formula[2] != ".")) {
      # Subset the columns (and possibly the rows)
      if (is.null(subset)) {
        data <- data[, all.vars(formula)]
      } else {
        data <- data[subset, all.vars(formula)]
      }
    }
  }

  if (isTRUE(transpose))
    data <- t(data)

  # Arguments method =/metric = and stand = not always there
  if (!is.null(as.list(args(fun))$metric)) {# metric = instead of method =
    dst <- fun(data, metric = method, stand = scale, ...)
  } else if (isTRUE(scale)) {
    if (is.null(as.list(args(fun))$stand)) {# fun has no stand = argument
      data <- scale(data)
      dst <- fun(data, method = method, ...)
    } else {# We don't standardise ourself because there may be also qualitative
      # or binary data (like for cluster::daisy, for instance)
      dst <- fun(data, method = method, stand = scale, ...)
    }
  } else {# Just method = and scale = FALSE
    dst <- fun(data, method = method, ...)
  }
  attr(dst, "call") <- match.call()
  # Depending if it is a dist or dissimilarity object, the method is stored in
  # method or in Metric, but we use metric in our own version to avoid a clash
  # with the method item in cluster()/hclust() further on (hclust change it
  # into dist.method, but it is better to have the right name right now)
  attr(dst, "metric") <- method
  # dist or dissimilarity object use Labels, but we use labels everywhere else
  # including in cluster()/hclust()
  # So, we make sure labels is present (in hclust, it is labels anyway!)
  attr(dst, "labels") <- rownames(data)
  # Default values for Diag and Upper set to FALSE
  if (is.null(attr(dst, "Diag"))) attr(dst, "Diag") <- FALSE
  if (is.null(attr(dst, "Upper"))) attr(dst, "Upper") <- FALSE
  # Keep info about how raw data were transformed
  attr(dst, "rownames.col") <- rownames.col
  attr(dst, "transpose") <- transpose
  attr(dst, "scale") <- scale
  class(dst) <- unique(c("dissimilarity", class(dst)))
  dst
}

as.dissimilarity <- function(x, ...)
  UseMethod("as.dissimilarity")
as_dissimilarity <- as.dissimilarity # Synonym

as.dissimilarity.matrix <- function(x, ...) {
  dst <- as.dist(x, ...)
  attr(dst, "call") <- match.call()
  attr(dst, "metric") <- attr(dst, "method") # Make sur metric is used
  class(dst) <- unique(c("dissimilarity", class(dst)))
  dst
}

# We want to print only the first few rows and columns
print.dissimilarity <- function(x, digits.d = 3L, rownames.lab = "labels",
                                ...) {
  mat <- as.matrix(x)
  mat <- format(round(mat, digits.d))
  diag(mat) <- ""
  mat[upper.tri(mat)] <- ""
  class(mat) <- c("dst", "matrix")
  tbl <- tibble::as_tibble(mat)
  #tbl <- tibble::add_column(tbl, {{rownames.lab}} = rownames(mat), .before = 1)
  # I prefer this
  tbl <- dplyr::bind_cols(
    as_tibble_col(rownames(mat), column_name = rownames.lab), tbl)
  tbl <- tbl[, -ncol(tbl)]
  more_info <- ""
  if (isTRUE(attr(x, "scale"))) {
    if (isTRUE(attr(x, "transpose"))) {
      more_info <- " (transposed then scaled data)"
    } else {# Only scaled
      more_info <- " (scaled data)"
    }
  } else {
    if (isTRUE(attr(x, "transpose")))
      more_info <- " (transposed data)"
  }
  cat("Dissimilarity matrix with metric: ", attr(x, "metric"),
      more_info, "\n", sep = "")
  print(tbl)
  invisible(x)
}

labels.dissimilarity <- function(object, ...) {
  labs <- object$labels
  if (is.null(labs)) object$Labels
}

nobs.dissimilarity <- function(object, ...)
  attr(object, "Size")

# TODO: `[` by first transforming into a matrix with as.matrix()

autoplot.dissimilarity <- function(object, order = TRUE, show.labels = TRUE,
                                   lab.size = NULL, gradient = list(low = "red", mid = "white", high = "blue"),
                                   ...) {
  factoextra::fviz_dist(object, order = order, show_labels = show.labels,
                        lab_size = lab.size, gradient = gradient)
}

chart.dissimilarity <- function(data, ...,
                                type = NULL, env = parent.frame())
  autoplot(data, type = type, ...)

# cluster object (inheriting from hclust)
cluster <- function(x, ...)
  UseMethod("cluster")

cluster.default <- function(x, ...)
  stop("No method for object of class ", class(x)[1])

# Cluster uses hclust() by default, ... but it looks first for a faster
# implementation in either {fastcluster} or {flashClust} before falling back
# to the {stats} version.
# The functions cluster::agnes() and cluster::diana() should be compatible too,
# as well as any function that returns an object convertible into hclust
# by as.hclust() (but not tested yet)
# Also, a version where the raw data are provided and the disimilarity matrix
# is internally calculated should be also provided (see cluster::agnes)
# See also {ape} for phylogenetic trees methods
cluster.dist <- function(x, method = "complete", fun = NULL, ...) {
  if (is.null(fun)) {
    # We try fastcluster, then flashClust, then stats
    fun <- try(fastcluster::hclust, silent = TRUE)
    if (inherits(fun, "try-error"))
      fun <- try(flashClust::hclust, silent = TRUE)
    if (inherits(fun, "try-error"))
      fun <- try(stats::hclust, silent = TRUE)
  }
  clst <- fun(x, method = method, ...)
  clst <- as.hclust(clst)
  clst$call <- match.call()
  # hclust has to give a different name to the distance metric: dist.method
  # but we use metric. Again, keep both for maximum compatibility
  clst$metric <- clst$dist.method
  # If the original data were scaled or transposed, get the info also
  clst$rownames.col <- attr(x, "rownames.col")
  clst$scale <- attr(x, "scale")
  clst$transpose <- attr(x, "transpose")
  class(clst) <- unique(c("cluster", class(clst)))
  clst
}

# A couple of useful methods for our cluster object
# str() method is gathered from a dendrogram object
str.cluster <- function(object, max.level = NA, digits.d = 3L, ...)
  str(as.dendrogram(object), max.level = max.level, digits.d = digits.d, ...)

labels.cluster <- function(object, ...)
  object$labels

nobs.cluster <- function(object, ...)
  length(object$order)

# Other methods by first transforming into dendrogram: rev, reorder, order, [[

# cutree() is an explicit name, but it does not follow the rule of using
# known methods... and here, it really something that predict() is made for,
# except it cannot handle newdata =, but that argument is not in its definition
predict.cluster <- function(object, k = NULL, h = NULL, ...)
  cutree(object, k = k, h = h)

# There is no broom::glance() or broom::tidy() yet (what to put in it?),
# but broom:augment() should be nice = add the clusters as .fitted in the tibble
library(broom)
augment.cluster <- function(x, data, k = NULL, h = NULL, ...) {
  # Should we transpose the data (note: this is against augment() rules, but...)
  if (isTRUE(x$transpose)) {
    # We first have to make sure rownames are correct before the transposition
    if (!is.matrix(data) && !is.null(data[[x$rownames.col]])) {
      rownames(data) <- data[[x$rownames.col]]
      data[[x$rownames.col]] <- NULL
    }
    data <- t(data)
    msg <- "transposed data"
  } else {
    msg <- "data"
  }
  data <- as_tibble(data)

  # Get clusters
  clst <- predict(x, k = k, h = h, ...)
  if (nrow(data) != length(clst)) {
    stop("Different number of items in ", msg, " (",nrow(data) ,
         ") and in the clusters (", length(clst), ")")
  }
  tibble::add_column(data, .fitted = clst)
}

# Instead of the default plot.hclust(), we prefer the plot.dendrogram() version
# that allows for more and better variations of the dendrogram (horizontal or
# circular), see http://www.sthda.com/english/wiki
# /beautiful-dendrogram-visualizations-in-r-5-must-known-methods
# -unsupervised-machine-learning
plot.cluster <- function(x, y, hang = -1, check = TRUE, type = "vertical",
                         lab = "Height", ...) {
  type <- match.arg(type[1], c("vertical", "horizontal", "circular"))
  # type == "circular" is special because we need to transform as ape::phylo
  if (type == "circular") {
    if (!missing(hang))
      warning("'hang' is not used with a circular dendrogram")
    phylo <- ape::as.phylo(x)
    plot(phylo, type = "fan", font = 1, ...)
  } else {# Use plot.dendrogram() instead
    # We first convert into dendrogram objet, then we plot it
    # (better that plot.hclust())
    dendro <- as.dendrogram(x, hang = hang, check = check)
    if (type == "horizontal") {
      plot(dendro, horiz = TRUE, xlab = lab, ...)
    } else {
      plot(dendro, horiz = FALSE, ylab = lab, ...) # note: label different axe
    }
  }
}

# This is to draw circles in a plot (where to cut in a circular dendrogram)
# TODO: should be nice to do similar function for other symbols too in SciViews
circle <- function(x = 0, y = 0, d = 1, col = 0, lwd = 1, lty = 1, ...)
  symbols(x = x, y = y, circles = d / 2, fg = col, lwd = lwd, lty = lty,
          inches = FALSE, add = TRUE, ...)

# TODO: make sure the dendrogram is correct with different ggplot themes
autoplot.cluster <- function(object, type = "vertical", circ.text.size = 3,
                             theme = theme_sciviews(), xlab = "", ylab = "Height", ...) {
  if (is.null(type))
    type <- "vertical"
  type <- match.arg(type[1], c("vertical", "horizontal", "circular"))

  # Create the dendrogram
  ddata <- ggdendro::dendro_data(object, type = "rectangle")
  dendro <- ggplot(ggdendro::segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme + xlab(xlab) + ylab(ylab)

  if (type == "circular") {
    # Get labels (need one more to avoid last = first!)
    label_df <- tibble::tibble(labels = c(labels(object)[object$order], ""))
    xmax <- nobs(object) + 1
    label_df$id <- 1:xmax
    angle <-  360 * (label_df$id - 0.5) / xmax
    # Left or right?
    label_df$hjust <- ifelse(angle < 270 & angle > 90, 1, 0)
    # Angle for more readable text
    label_df$angle <- ifelse(angle < 270 & angle > 90, angle + 180, angle)

    # Make the dendrogram circular
    dendro <- dendro +
      scale_x_reverse() +
      scale_y_reverse() +
      coord_polar(start = pi/2) +
      geom_text(data = label_df,
                aes(x = id, y = -0.02, label = labels, hjust = hjust),
                size = circ.text.size, angle = label_df$angle, inherit.aes = FALSE) +
      theme(panel.border = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks.y = element_blank()) +
      ylab("")

  } else if (type == "vertical") {# Vertical dendrogram
    dendro <- dendro +
      scale_x_continuous(breaks = seq_along(ddata$labels$label),
                         labels = ddata$labels$label) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
      theme(panel.border = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(angle = 90, hjust = 0.5))

  } else {# Horizontal dendrogram
    dendro <- dendro +
      scale_x_continuous(breaks = seq_along(ddata$labels$label),
                         labels = ddata$labels$label, position = "top") +
      scale_y_reverse(expand = expansion(mult = c(0.05, 0))) +
      coord_flip() +
      theme(panel.border = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank())
  }
  dendro
}

chart.cluster <- function(data, ...,
                          type = NULL, env = parent.frame())
  autoplot(data, type = type, ...)


# data ----
penguins <- read("penguins", package = "palmerpenguins")

penguins %>.%
  # filter(., sex == "male") %>.%
  select(., species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>.%
  drop_na(.) -> peng

peng %>.%
  select(., -species) -> peng_red

# specific Function ----

score_cah <- function(x, reference = peng$species) {
  tab <- table(reference, x)
  max_gr <- apply(tab, 1, which.max)

  if(length(unique(max_gr)) < 3)
    res <- "Votre CAH ne permet pas de retrouver les 3 groupes. Un ou plusieurs groupes sont confondus."

  if(length(unique(max_gr)) == 3) {
    tot <- rowSums(tab[, unique(max.col(tab))]) / rowSums(tab)
    res <- paste0("Votre CAH permet de discerner 3 groupes avec une précision de ", (100*sum(tot)/nlevels(reference)), "%.")
  }
  res
}

# UI -----

ui <- fluidPage(
  learndownShiny("Regroupement d'espèces de manchôt avec la classification hiérarchique ascendante."),

  sidebarLayout(
    sidebarPanel(
      p("Vous avez à disposition 342 manchôts mâles de 3 espèces différentes. Trouvez les meilleurs paramètres afin d'obtenir la plus haute similitude entre votre CAH et les observations de terrain."),
      selectInput("method_dist", "Indice de distance", choices = c("euclidian", "bray", "canberra", "manhattan")),
      selectInput("scale", "Standardisation", choices = c(FALSE, TRUE)),
      selectInput("method_clust", "Méthode de CAH", choices = c("complete", "single","average", "ward.D2")),
      hr(),
      submitQuitButtons()
    ),

    mainPanel(
      plotOutput("dendrogram"),
      # fluidRow(
      #   column(width = 6,
      #          plotOutput("dendro")
      #         ),
      #   column(width = 6,
      #         plotOutput("dendro")
      #          )
      # ),
      tableOutput("tab_res"),
      hr(),
      textOutput("scores_res")
    )
  )
)


server <- function(input, output, session) {

  cah <- reactive({
    peng_dist <- dissimilarity(data = peng_red, scale = as.logical(input$scale), method = input$method_dist)
    peng_clust <- cluster(peng_dist, method = input$method_clust)
    peng_clust
  })

  output$dendrogram <- renderPlot({
    cah <- cah()
    chart(cah) +
      ylab("hauteur")
  })

  output$tab_res <- renderTable({
    cah <- cah()

    table(peng$species, predict(cah, k = 3)) %>.%
      as.data.frame(.) %>.%
      pivot_wider(. , names_from = Var2, values_from = Freq) %>.%
      rename(., "Especes" = Var1)
  })

  output$scores_res <- renderText({
    cah <- cah()
    score_cah(predict(cah, k = 3), reference = peng$species)
  })


  # trackEvents(session, input, output,
  #   sign_in.fun = BioDataScience::sign_in, config = conf)
  # trackSubmit(session, input, output, max_score = 4, solution =
  #   list(asym = asym_init, lrc = lrc_init, c0 = c0_init, m = m_init),
  #   comment = "y(x) = Asym * e^(- b2 * b3^x)",
  #   message.success = "Correct, c'est le meilleur modèle.",
  #   message.error = "Incorrect, un modèle mieux ajusté existe.")
  # trackQuit(session, input, output, delay = 20)
}

shinyApp(ui, server)
