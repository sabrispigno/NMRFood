# --------------------- 1) READ + FILTER + NORMALIZE --------------------- #
get_nmr_data <- function(file, sheet = "datidef",
                         ppm_col = "ppm", ppm_min = 0, ppm_max = 200,
                         normalize = TRUE) {
  df <- read_xlsx(file, sheet = sheet) %>%
    filter(.data[[ppm_col]] >= ppm_min, .data[[ppm_col]] < ppm_max)
  
  if (normalize) {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    sig_cols <- setdiff(num_cols, ppm_col)
    if (length(sig_cols)) {
      sums <- colSums(df[sig_cols], na.rm = TRUE)
      safe_div <- ifelse(sums == 0, 1, sums)
      df[sig_cols] <- sweep(df[sig_cols], 2, safe_div, "/")
    }
  }
  # put ppm first
  df[, c(ppm_col, setdiff(names(df), ppm_col))]
}

# --------------------- 2) ADD MOLECULAR CLASSES --------------------- #
add_molecular_classes <- function(df, ppm_col = "ppm",
                                  out_col = "MolecularClasses") {
  stopifnot(ppm_col %in% names(df))
  breaks <- c(-Inf, 45, 60, 90, 110, 145, 160, Inf)
  labels <- c("Alkyl C", "Methoxyl N-Alkyl-C", "O-alkyl-C",
              "Di-O-alkyl C", "H- C- sub. aromatic C",
              "O- sub. aromatic  C", "Carbonyl C")
  df[[out_col]] <- cut(df[[ppm_col]], breaks = breaks, labels = labels,
                       right = TRUE, include.lowest = TRUE)
  df
}

# --------------------- 3) SUM BY CLASSES (+ OPTIONAL REORDER) --------------------- #
sum_by_classes <- function(df, class_col = "MolecularClasses") {
  stopifnot(class_col %in% names(df))
  df[[class_col]] <- factor(df[[class_col]], levels = unique(df[[class_col]]))
  
  out <- df %>%
    group_by(.data[[class_col]]) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop")
  
  # reorder sample columns by magnitude of one selected class (descending)
  if (!is.null(order_by_class) && order_by_class %in% out[[class_col]]) {
    sample_cols <- setdiff(names(out), class_col)
    ridx <- which(out[[class_col]] == order_by_class)
    ord  <- order(-as.numeric(out[ridx, sample_cols]))
    out  <- cbind(out[1], out[sample_cols[ord]])
    names(out)[1] <- class_col
  }
  out
}

# --------------------- 4a) FIG. 1 — SPECTRA GRID --------------------- #
plot_spectra_grid <- function(data,
                              ppm_col = "ppm",
                              exclude_cols = c("MolecularClasses"),
                              ncol = 3,
                              save_prefix = NULL) {
  stopifnot(ppm_col %in% names(data))
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  sig_cols <- setdiff(num_cols, c(ppm_col, exclude_cols))
  if (!length(sig_cols)) stop("No signal columns to plot.")
  
  # common y limits
  y_min <- min(as.matrix(data[sig_cols]), na.rm = TRUE)
  y_max <- max(as.matrix(data[sig_cols]), na.rm = TRUE)
  
  plots <- lapply(sig_cols, function(col) {
    ggplot(data, aes(x = .data[[ppm_col]], y = .data[[col]])) +
      geom_line(linewidth = 0.1) +
      annotate("text",
               x = mean(range(data[[ppm_col]], na.rm = TRUE)),
               y = y_max, label = col, size = 1.28) +
      xlab("") + ylab("") +
      scale_x_reverse() +
      coord_cartesian(ylim = c(y_min, y_max)) +
      theme_classic() +
      theme(axis.title = element_blank(),
            axis.text  = element_blank(),
            axis.line  = element_blank(),
            axis.ticks = element_blank())
  })
  names(plots) <- sig_cols
  
  # optional multi-file export
  if (!is.null(save_prefix)) {
    # PDF
    pdf(paste0(save_prefix, "_grid_plot.pdf"))
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
    # PNG
    png(paste0(save_prefix, "_grid_plot.png"),
        width = 16, height = 22, units = "cm", res = 400)
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
    # SVG
    svglite::svglite(paste0(save_prefix, "_grid_plot.svg"), width = 16)
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
  }
  
  invisible(plots)
}

# --------------------- 4b) FIG. 2 — STACKED PERCENT BARS --------------------- #
plot_class_stacks <- function(summed_data,
                              class_col = "MolecularClasses",
                              colors = c("red3", "#808080", "#000000",
                                         "#FFA500", "#FFFF00", "lightgreen", "brown"),
                              ncol = 3,
                              save_prefix = NULL) {
  stopifnot(class_col %in% names(summed_data))
  cols <- setdiff(names(summed_data), class_col)
  if (!length(cols)) stop("No columns to plot.")
  
  if (length(colors) != nrow(summed_data)) {
    warning("Length of 'colors' does not match number of classes; using ggplot default palette.")
    colors <- NULL
  }
  
  plots <- lapply(cols, function(col) {
    p <- ggplot(summed_data, aes(x = "", y = .data[[col]], fill = .data[[class_col]])) +
      geom_bar(stat = "identity", color = "black", linewidth = 0.3,
               position = "fill", width = 0.3) +
      xlab(col) + ylab(NULL) +
      scale_y_continuous(labels = scales::percent) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.line    = element_blank(),
            axis.ticks   = element_blank(),
            legend.box   = element_blank())
    if (!is.null(colors)) p <- p + scale_fill_manual(values = colors, name = class_col)
    p
  })
  names(plots) <- cols
  
  if (!is.null(save_prefix)) {
    # PDF
    pdf(paste0(save_prefix, "_histogrid.pdf"))
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
    # PNG
    png(paste0(save_prefix, "_histogrid.png"),
        width = 16, height = 20, units = "cm", res = 300)
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
    # SVG
    svglite::svglite(paste0(save_prefix, "_histogrid.svg"), width = 16)
    do.call(grid.arrange, c(plots, ncol = ncol))
    dev.off()
  }
  
  invisible(plots)
}

