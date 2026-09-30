# Plot style retained from archived R/hipotesis.r. The estimates come from the final
# TSFS mediator equation, never from data/factor_scores_final.rds.
generate_tsfs_final_plots <- function(
  model_M, tsfs, output_dir, use_hc3_M,
  file_names = c(
    johnson_neyman = "TSFS_Johnson_Neyman_ESI_PSMLS_PEB.png",
    interaction = "TSFS_Interaction_ESI_PSMLS_PEB.png"
  )
) {
  stopifnot(inherits(model_M, "lm"), is.data.frame(tsfs))
  stopifnot(all(c("ESI_c", "PSMLS_c", "PEB_B") %in% names(tsfs)))
  stopifnot(all(c("johnson_neyman", "interaction") %in% names(file_names)))

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  psmls_sd <- stats::sd(tsfs$PSMLS_c)
  robust_option <- if (use_hc3_M) "HC3" else FALSE

  # Preserve the original interactions::sim_slopes Johnson-Neyman design.
  jn <- interactions::sim_slopes(
    model_M,
    pred = ESI_c,
    modx = PSMLS_c,
    johnson_neyman = TRUE,
    jnplot = TRUE,
    robust = robust_option
  )
  if (is.null(jn$jnplot)) stop("Plot Johnson-Neyman tidak dihasilkan.")
  jn$jnplot <- jn$jnplot + ggplot2::labs(
    x = "PSMLS", y = "Slope of ESI"
  )

  jn_file <- file.path(output_dir, file_names[["johnson_neyman"]])
  grDevices::png(jn_file, width = 6.5, height = 5.5, units = "in", res = 600)
  tryCatch(
    print(jn$jnplot),
    finally = grDevices::dev.off()
  )

  # Same dimensions, palette, line types, axes, titles, and theme as the
  # original R/hipotesis.r plot. Moderator levels now use the final TSFS SD.
  axis_lim <- c(-2.0, 1.5)
  interaction_plot <- interactions::interact_plot(
    model_M,
    pred = ESI_c,
    modx = PSMLS_c,
    modx.values = c(-psmls_sd, 0, psmls_sd),
    modx.labels = c("Low (-1 SD)", "Mean", "High (+1 SD)"),
    interval = FALSE,
    plot.points = FALSE,
    vary.lty = TRUE,
    line.thickness = 0.75,
    colors = c("#009E73", "#0072B2", "#D55E00"),
    x.label = NULL,
    y.label = NULL,
    legend.main = "PSMLS"
  ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed", linewidth = 0.4,
      color = "#4D4D4D"
    ) +
    ggplot2::geom_hline(
      yintercept = 0, linewidth = 0.4, color = "#4D4D4D"
    ) +
    ggplot2::coord_cartesian(xlim = axis_lim, ylim = axis_lim) +
    ggplot2::scale_x_continuous(
      limits = axis_lim, breaks = seq(-2.0, 1.5, by = 0.5),
      labels = scales::label_number(accuracy = 0.1), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = axis_lim, breaks = seq(-2.0, 1.5, by = 0.5),
      labels = scales::label_number(accuracy = 0.1), expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Interaction of ESI and PSMLS on PEB",
      subtitle = "Predicted Pro-Environmental Behavior",
      x = "Environmental Self-Identity (ESI)",
      y = "Predicted PEB"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.30, color = "#D0D0D0"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        fill = NA, linewidth = 0.8, color = "#3A9BC5"
      ),
      plot.title = ggplot2::element_text(
        face = "bold", size = 12, color = "#20255F", hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10, color = "#30384A", hjust = 0,
        margin = ggplot2::margin(b = 8)
      ),
      axis.title.x = ggplot2::element_text(
        face = "bold", size = 10, color = "#20255F",
        margin = ggplot2::margin(t = 10)
      ),
      axis.title.y = ggplot2::element_text(
        face = "bold", size = 10, color = "#20255F",
        margin = ggplot2::margin(r = 8)
      ),
      axis.text = ggplot2::element_text(size = 8.5, color = "#20255F"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box.just = "center",
      legend.title = ggplot2::element_text(
        face = "bold", size = 8, color = "#20255F"
      ),
      legend.text = ggplot2::element_text(size = 8, color = "#20255F"),
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )

  interaction_file <- file.path(output_dir, file_names[["interaction"]])
  ggplot2::ggsave(
    filename = interaction_file,
    plot = interaction_plot,
    width = 6.5,
    height = 5.5,
    units = "in",
    dpi = 600,
    bg = "white"
  )

  invisible(c(johnson_neyman = jn_file, interaction = interaction_file))
}

# Gambar model untuk naskah. Seluruh angka diambil dari model dan hasil bootstrap
# pada run yang sama agar gambar tidak menyimpan koefisien dari analisis lama.
buat_gambar_model_7 <- function(model_M, model_Y, simple_slopes,
                                bootstrap_summary, output_file,
                                use_hc3_M = TRUE) {
  stopifnot(inherits(model_M, "lm"), inherits(model_Y, "lm"))
  stopifnot(all(c("PSMLS_Level", "Slope_ESI_to_PEB", "p") %in%
                  names(simple_slopes)))

  fmt <- function(x, digits = 3L) {
    value <- sprintf(paste0("%.", digits, "f"), x)
    value <- sub("^0\\.", ".", value)
    sub("^-0\\.", "−.", value)
  }
  fmt_p <- function(p) {
    if (p < .001) "< .001" else paste("=", fmt(p))
  }
  fmt_ci <- function(low, high) paste0("[", fmt(low), ", ", fmt(high), "]")

  m <- stats::coef(model_M)
  y <- stats::coef(model_Y)
  m_vcov <- if (use_hc3_M) sandwich::vcovHC(model_M, type = "HC3") else
    stats::vcov(model_M)
  m_p <- 2 * stats::pt(abs(m / sqrt(diag(m_vcov))),
                       df = stats::df.residual(model_M), lower.tail = FALSE)
  y_p <- summary(model_Y)$coefficients[, "Pr(>|t|)"]
  stars <- function(p) if (p < .001) "***" else ""

  slope_names <- c("Low_minus_1SD", "Mean", "High_plus_1SD")
  slopes <- simple_slopes[match(slope_names, simple_slopes$PSMLS_Level), ]
  if (anyNA(slopes$Slope_ESI_to_PEB)) {
    stop("Baris simple slopes untuk low, mean, dan high tidak lengkap.")
  }
  slope_text <- paste0(
    vapply(seq_len(3L), function(i) {
      paste0(
        fmt(slopes$Slope_ESI_to_PEB[i]),
        if (slopes$p[i] < .001) "***" else
          paste0(" (p ", fmt_p(slopes$p[i]), ")")
      )
    }, character(1)),
    collapse = ", "
  )

  note_boot <- "Bootstrap belum dijalankan; efek tidak langsung tidak ditampilkan."
  if (nrow(bootstrap_summary)) {
    mean_row <- bootstrap_summary[
      bootstrap_summary$Effect == "Indirect Effect - Mean PSMLS", ]
    index_row <- bootstrap_summary[
      bootstrap_summary$Effect == "Index of Moderated Mediation", ]
    if (nrow(mean_row) != 1L || nrow(index_row) != 1L) {
      stop("Ringkasan bootstrap tidak memuat efek mean dan index.")
    }
    note_boot <- paste0(
      "Indirect effect at mean PSMLS: ", fmt(mean_row$Estimate),
      ", 95% bootstrap CI ", fmt_ci(mean_row$Boot_LLCI, mean_row$Boot_ULCI),
      " (H6); moderated mediation index: ", fmt(index_row$Estimate),
      ", CI ", fmt_ci(index_row$Boot_LLCI, index_row$Boot_ULCI), " (H7)."
    )
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(output_file, width = 10.5, height = 5.2,
                 units = "in", res = 450, type = "cairo", bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  line <- function(x, y, arrow_end = FALSE, width = 1.35) {
    grid::grid.lines(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"),
      arrow = if (arrow_end) grid::arrow(type = "closed", length =
                                          grid::unit(.105, "in")) else NULL,
      gp = grid::gpar(col = "black", fill = "black", lwd = width)
    )
  }
  box <- function(x, y, w, h) {
    grid::grid.roundrect(
      x = x, y = y, width = w, height = h,
      r = grid::unit(.012, "snpc"),
      gp = grid::gpar(col = "black", fill = "white", lwd = 1.35)
    )
  }
  label <- function(text, x, y, size = 13, face = "plain", just = "centre") {
    grid::grid.text(
      text, x = x, y = y, just = just,
      gp = grid::gpar(fontfamily = "serif", fontsize = size,
                      fontface = face, col = "black", lineheight = 1.08)
    )
  }

  # Jalur lebih dulu digambar agar ujung panah tertutup rapi oleh kotak.
  line(c(.215, .375), c(.675, .675), arrow_end = TRUE)
  line(c(.625, .775), c(.675, .675), arrow_end = TRUE)
  line(c(.120, .120), c(.805, .895))
  line(c(.120, .875), c(.895, .895))
  line(c(.875, .875), c(.895, .805), arrow_end = TRUE)
  line(c(.335, .335), c(.430, .675), arrow_end = TRUE)

  box(.120, .675, .190, .260)
  box(.500, .675, .250, .260)
  box(.875, .675, .200, .260)
  box(.320, .340, .205, .180)

  label("Environmental\nSelf-Identity\n(ESI)", .120, .675, 13.2)
  label("Pro-Environmental\nBehavior\n(PEB)", .500, .702, 13.2)
  label(paste0("R² = ", fmt(summary(model_M)$r.squared)),
        .500, .586, 12.4, "italic")
  label("Psychological\nWell-Being\n(PWB)", .875, .702, 13.2)
  label(paste0("R² = ", fmt(summary(model_Y)$r.squared)),
        .875, .586, 12.4, "italic")
  label("Perceived Social\nMedia Literacy\n(PSMLS)", .320, .340, 12.6)

  label(paste0("a₁ = ", fmt(m[["ESI_c"]]), stars(m_p[["ESI_c"]])),
        .293, .757, 12.1, "italic")
  label("(H1)", .293, .722, 11.3)
  label(paste0("b = ", fmt(y[["PEB_R_Y"]]), stars(y_p[["PEB_R_Y"]])),
        .703, .757, 12.1, "italic")
  label("(H2)", .703, .722, 11.3)
  label(paste0("c′ = ", fmt(y[["ESI_R_Y"]]), stars(y_p[["ESI_R_Y"]])),
        .500, .962, 12.1, "italic")
  label("(H3)", .500, .930, 11.3)
  label(paste0("a₃ = ", fmt(m[["ESI_c:PSMLS_c"]]),
               stars(m_p[["ESI_c:PSMLS_c"]])),
        .257, .530, 12.1, "italic")
  label("(ESI × PSMLS; H4)", .257, .492, 10.7)

  line(c(.025, .975), c(.215, .215), width = .75)
  label("Note. Coefficients are unstandardized; a₁ is evaluated at mean PSMLS.",
        .028, .179, 9.5, just = "left")
  label(paste0("Conditional ESI–PEB slopes at low, mean, and high PSMLS: ",
               slope_text, " (H5)."),
        .028, .137, 9.5, just = "left")
  label(note_boot, .028, .095, 9.5, just = "left")
  label(paste0("The PSMLS main effect on PEB is omitted (B = ",
               fmt(m[["PSMLS_c"]]), ", p ", fmt_p(m_p[["PSMLS_c"]]),
               "). *** p < .001."),
        .028, .053, 9.5, just = "left")

  invisible(output_file)
}
