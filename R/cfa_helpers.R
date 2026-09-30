# ==============================================================================
# CFA HELPERS - ANECO
# ==============================================================================
# Fungsi khusus output CFA ke console/TXT:
# - Membaca pemetaan item dan bunyi item
# - MODEL INFO
# - FACTOR STRUCTURE
# - MODEL FIT
# - FACTOR LOADINGS + bunyi item
# - FACTOR CORRELATIONS
# - MODIFICATION INDICES + bunyi item
# - RELIABILITY
# - print_cfa() sebagai output CFA lengkap
#
# Ketergantungan:
# - R/helpers.R harus di-source lebih dulu
# - get_rel_df() berasal dari R/helpers.R
#
# Urutan output print_cfa():
# MODEL INFO
# FACTOR STRUCTURE
# MODEL FIT
# FACTOR LOADINGS
# FACTOR CORRELATIONS
# MODIFICATION INDICES
# RELIABILITY
# ==============================================================================


# ==============================================================================
# 1. PACKAGES & DEPENDENCY CHECK
# ==============================================================================

library(lavaan)

if (!exists("get_rel_df", mode = "function")) {
  stop(
    paste0(
      "Fungsi get_rel_df() tidak ditemukan. ",
      "Jalankan source('R/helpers.R') sebelum source('R/cfa_helpers.R')."
    )
  )
}


# ==============================================================================
# 2. BACA FILE PEMETAAN ITEM
# ==============================================================================

item_mapping_file <- "data/pemetaan_item.txt"

if (!file.exists(item_mapping_file)) {

  stop(
    paste0(
      "File pemetaan item tidak ditemukan: ",
      item_mapping_file
    )
  )
}


item_txt <- readLines(
  item_mapping_file,
  encoding = "UTF-8",
  warn = FALSE
)


# Format yang dicari:
# (Indeks Javascript: 12) -> bunyi item

item_lines <- grep(
  "\\(Indeks Javascript: [0-9]+\\) ->",
  item_txt,
  value = TRUE
)


if (length(item_lines) == 0) {

  stop(
    "Format pemetaan item pada file TXT tidak berhasil dikenali."
  )
}


# Ambil indeks Javascript

item_index <- as.integer(
  sub(
    ".*Indeks Javascript: ([0-9]+)\\).*",
    "\\1",
    item_lines
  )
)


# Ambil bunyi item

item_text <- sub(
  "^.*\\) -> ",
  "",
  item_lines
)


text_by_index <- setNames(
  item_text,
  as.character(item_index)
)


# ==============================================================================
# 3. PEMETAAN INDEKS ITEM
# ==============================================================================

# ------------------------------------------------------------------------------
# Environmental Identity
# ------------------------------------------------------------------------------

eid_idx <- c(
  12:21,
  23:30
)


# ------------------------------------------------------------------------------
# Environmental Self-Identity
# ------------------------------------------------------------------------------

esi_idx <- 31:36


# ------------------------------------------------------------------------------
# Psychological Well-Being
# ------------------------------------------------------------------------------

pwb_idx <- c(
  37:46,
  48:56,
  58:71,
  73:81
)


# ------------------------------------------------------------------------------
# Perceived Social Media Literacy
# ------------------------------------------------------------------------------

psml_idx <- c(
  82:91,
  93:96
)


# ------------------------------------------------------------------------------
# Pro-Environmental Behavior
# ------------------------------------------------------------------------------

peb_idx <- c(
  97:110,
  112:116
)


# ==============================================================================
# 4. NAMA ITEM PWB
# ==============================================================================
# CATATAN:
# Nomor PWB_1 sampai PWB_42 mengikuti URUTAN LOKAL Google Form.
# Item pada Google Form sudah dikelompokkan per dimensi PWB.
#
# Dimensi lokal:
# 1-7   = Autonomy
# 8-14  = Environmental Mastery
# 15-21 = Personal Growth
# 22-28 = Positive Relations
# 29-35 = Purpose in Life
# 36-42 = Self-Acceptance
#
# Suffix _R menunjukkan item yang sudah di-reverse-score pada sheet Scoring.
# ==============================================================================

pwb_names <- c(

  # ---------------------------------------------------------------------------
  # Autonomy: PWB 1-7
  # Reverse lokal: 3, 4, 6
  # ---------------------------------------------------------------------------

  "PWB_1",
  "PWB_2",
  "PWB_3_R",
  "PWB_4_R",
  "PWB_5",
  "PWB_6_R",
  "PWB_7",


  # ---------------------------------------------------------------------------
  # Environmental Mastery: PWB 8-14
  # Reverse lokal: 9, 10, 12, 13
  # ---------------------------------------------------------------------------

  "PWB_8",
  "PWB_9_R",
  "PWB_10_R",
  "PWB_11",
  "PWB_12_R",
  "PWB_13_R",
  "PWB_14",


  # ---------------------------------------------------------------------------
  # Personal Growth: PWB 15-21
  # Reverse lokal: 15, 17, 19, 21
  # ---------------------------------------------------------------------------

  "PWB_15_R",
  "PWB_16",
  "PWB_17_R",
  "PWB_18",
  "PWB_19_R",
  "PWB_20",
  "PWB_21_R",


  # ---------------------------------------------------------------------------
  # Positive Relations: PWB 22-28
  # Reverse lokal: 23, 24, 27
  # ---------------------------------------------------------------------------

  "PWB_22",
  "PWB_23_R",
  "PWB_24_R",
  "PWB_25",
  "PWB_26",
  "PWB_27_R",
  "PWB_28",


  # ---------------------------------------------------------------------------
  # Purpose in Life: PWB 29-35
  # Reverse lokal: 29, 31, 32, 35
  # ---------------------------------------------------------------------------

  "PWB_29_R",
  "PWB_30",
  "PWB_31_R",
  "PWB_32_R",
  "PWB_33",
  "PWB_34",
  "PWB_35_R",


  # ---------------------------------------------------------------------------
  # Self-Acceptance: PWB 36-42
  # Reverse lokal: 38, 40, 41
  # ---------------------------------------------------------------------------

  "PWB_36",
  "PWB_37",
  "PWB_38_R",
  "PWB_39",
  "PWB_40_R",
  "PWB_41_R",
  "PWB_42"
)

# ==============================================================================
# 5. LOOKUP BUNYI ITEM
# ==============================================================================

item_labels <- c(

  # EID
  setNames(
    unname(
      text_by_index[
        as.character(eid_idx)
      ]
    ),
    paste0(
      "EID_",
      1:18
    )
  ),

  # ESI
  setNames(
    unname(
      text_by_index[
        as.character(esi_idx)
      ]
    ),
    paste0(
      "ESI_",
      1:6
    )
  ),

  # PSML
  setNames(
    unname(
      text_by_index[
        as.character(psml_idx)
      ]
    ),
    paste0(
      "PSMLS_",
      1:14
    )
  ),

  # PEB
  setNames(
    unname(
      text_by_index[
        as.character(peb_idx)
      ]
    ),
    paste0(
      "PEB_",
      1:19
    )
  ),

  # PWB
  setNames(
    unname(
      text_by_index[
        as.character(pwb_idx)
      ]
    ),
    pwb_names
  )
)


# ==============================================================================
# 5A. DAFTAR ITEM LENGKAP PER ALAT UKUR
# ==============================================================================
# Digunakan untuk menentukan item yang dipertahankan/dihapus secara otomatis.
# Berlaku untuk seluruh alat ukur yang sudah dipetakan di file ini.

instrument_items <- list(

  EID = paste0(
    "EID_",
    1:18
  ),

  ESI = paste0(
    "ESI_",
    1:6
  ),

  PSMLS = paste0(
    "PSMLS_",
    1:14
  ),

  PEB = paste0(
    "PEB_",
    1:19
  ),

  PWB = pwb_names
)


# ==============================================================================
# 5B. AMBIL ITEM YANG DIGUNAKAN DALAM MODEL
# ==============================================================================

get_model_items <- function(fit) {

  observed_names <- lavNames(
    fit,
    type = "ov"
  )

  known_items <- unique(
    unlist(
      instrument_items,
      use.names = FALSE
    )
  )

  observed_names[
    observed_names %in% known_items
  ]
}


# ==============================================================================
# 5C. AMBIL ITEM YANG DIHAPUS SECARA OTOMATIS
# ==============================================================================

get_removed_items <- function(fit) {

  items_kept <- get_model_items(
    fit
  )

  if (
    length(items_kept) == 0
  ) {

    return(
      character(0)
    )
  }


  instruments_used <- names(
    instrument_items
  )[
    vapply(
      instrument_items,
      function(x) {
        any(
          x %in% items_kept
        )
      },
      logical(1)
    )
  ]


  all_items <- unique(
    unlist(
      instrument_items[
        instruments_used
      ],
      use.names = FALSE
    )
  )


  setdiff(
    all_items,
    items_kept
  )
}


# ==============================================================================
# 6. FORMAT P-VALUE
# ==============================================================================

format_p <- function(p) {

  if (is.na(p)) {
    return("NA")
  }

  if (p < .001) {
    return("<.001")
  }

  sprintf(
    "%.3f",
    p
  )
}


# ==============================================================================
# 7. PEMBULATAN DATA FRAME
# ==============================================================================

round_numeric <- function(
  x,
  digits = 3
) {

  numeric_columns <- vapply(
    x,
    is.numeric,
    logical(1)
  )

  x[numeric_columns] <- lapply(
    x[numeric_columns],
    round,
    digits = digits
  )

  x
}


# ==============================================================================
# 8. BERSIHKAN TEKS UNTUK CONSOLE
# ==============================================================================

clean_console_text <- function(x) {

  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x) ||
    x == ""
  ) {

    return("-")
  }


  x <- as.character(x)


  # Em dash
  x <- gsub(
    "\u2014",
    " - ",
    x,
    fixed = TRUE
  )


  # En dash
  x <- gsub(
    "\u2013",
    "-",
    x,
    fixed = TRUE
  )


  # Non-breaking space
  x <- gsub(
    "\u00A0",
    " ",
    x,
    fixed = TRUE
  )


  # Rapikan spasi
  x <- gsub(
    "[[:space:]]+",
    " ",
    x
  )


  trimws(x)
}


# ==============================================================================
# 9. AMBIL BUNYI ITEM
# ==============================================================================

get_item_wording <- function(item) {

  if (
    is.null(item) ||
    is.na(item) ||
    item == ""
  ) {

    return("-")
  }


  if (
    item %in%
    names(item_labels)
  ) {

    return(
      clean_console_text(
        unname(
          item_labels[item]
        )
      )
    )
  }


  # Jika lhs/rhs adalah nama faktor latent
  # maka tampilkan sebagai nama faktor

  paste0(
    "[Factor: ",
    item,
    "]"
  )
}


# ==============================================================================
# 10. HEADER INSTRUMEN
# ==============================================================================

print_section <- function(title) {

  cat(
    "\n\n"
  )

  cat(
    strrep(
      "=",
      118
    ),
    "\n"
  )

  cat(
    title,
    "\n"
  )

  cat(
    strrep(
      "=",
      118
    ),
    "\n"
  )
}


# ==============================================================================
# 11. MODEL INFO
# ==============================================================================

print_model_info <- function(
  fit,
  model_name = NULL,
  data = NULL,
  items_removed = character(0)
) {

  options_fit <- lavInspect(
    fit,
    "options"
  )

  # ---------------------------------------------------------------------------
  # Estimator
  # ---------------------------------------------------------------------------

  estimator <- NULL

  if (
    !is.null(options_fit$estimator.orig) &&
    length(options_fit$estimator.orig) > 0 &&
    !is.na(options_fit$estimator.orig)
  ) {

    estimator <- as.character(
      options_fit$estimator.orig
    )

  } else {

    estimator <- as.character(
      options_fit$estimator
    )

    # lavaan sering menampilkan "ML" pada lavInspect()
    # walaupun model dijalankan dengan estimator = "MLR".
    robust_test <- paste(
      options_fit$test,
      collapse = " "
    )

    robust_se <- paste(
      options_fit$se,
      collapse = " "
    )

    if (
      identical(estimator, "ML") &&
      (
        grepl(
          "yuan|satorra|scaled|robust",
          robust_test,
          ignore.case = TRUE
        ) ||
        grepl(
          "robust",
          robust_se,
          ignore.case = TRUE
        )
      )
    ) {

      estimator <- "MLR"
    }
  }


  # ---------------------------------------------------------------------------
  # N
  # ---------------------------------------------------------------------------

  n_used <- lavInspect(
    fit,
    "nobs"
  )

  if (
    length(n_used) > 1
  ) {

    n_used <- sum(
      unlist(n_used)
    )
  }

  n_total <- if (
    !is.null(data)
  ) {

    nrow(data)

  } else {

    n_used
  }


  # ---------------------------------------------------------------------------
  # Struktur model
  # ---------------------------------------------------------------------------

  observed_names <- lavNames(
    fit,
    type = "ov"
  )

  latent_names <- lavNames(
    fit,
    type = "lv"
  )

  n_observed <- length(
    observed_names
  )

  n_latent <- length(
    latent_names
  )

  n_par <- lavInspect(
    fit,
    "npar"
  )

  df_model <- fitMeasures(
    fit,
    "df"
  )

  std_lv <- if (
    !is.null(options_fit$std.lv)
  ) {

    options_fit$std.lv

  } else {

    NA
  }

  converged <- lavInspect(
    fit,
    "converged"
  )


  # ---------------------------------------------------------------------------
  # Residual covariance yang benar-benar dibebaskan dalam model
  # Hanya ditampilkan jika memang ada.
  # ---------------------------------------------------------------------------

  pt <- parTable(
    fit
  )

  residual_pairs <- pt[
    pt$op == "~~" &
      pt$lhs %in% observed_names &
      pt$rhs %in% observed_names &
      pt$lhs != pt$rhs &
      pt$free > 0,
    c(
      "lhs",
      "rhs"
    ),
    drop = FALSE
  ]

  if (
    nrow(residual_pairs) > 0
  ) {

    residual_pairs$pair <- paste(
      residual_pairs$lhs,
      "~~",
      residual_pairs$rhs
    )
  }


  # ---------------------------------------------------------------------------
  # Cetak MODEL INFO
  # ---------------------------------------------------------------------------

  cat(
    "
MODEL INFO

"
  )


  if (
    !is.null(model_name)
  ) {

    cat(
      sprintf(
        "%-18s : %s
",
        "Model",
        model_name
      )
    )
  }


  cat(
    sprintf(
      "%-18s : %s
",
      "Estimator",
      estimator
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "N Total",
      n_total
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "N Used",
      n_used
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "Observed Variables",
      n_observed
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "Latent Factors",
      n_latent
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "Factor Names",
      paste(
        latent_names,
        collapse = ", "
      )
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "Parameters",
      n_par
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "Degrees of Freedom",
      df_model
    )
  )


  cat(
    sprintf(
      "%-18s : std.lv = %s
",
      "Standardization",
      std_lv
    )
  )


  cat(
    sprintf(
      "%-18s : %s
",
      "Converged",
      converged
    )
  )


  if (
    length(items_removed) == 0
  ) {

    cat(
      sprintf(
        "%-18s : 0
",
        "Items Removed"
      )
    )

  } else {

    cat(
      sprintf(
        "%-18s : %d | %s
",
        "Items Removed",
        length(items_removed),
        paste(
          items_removed,
          collapse = ", "
        )
      )
    )
  }


  # Residual pairs hanya muncul apabila memang masuk ke model.
  if (
    nrow(residual_pairs) > 0
  ) {

    cat(
      sprintf(
        "%-18s : %d
",
        "Residual Pairs",
        nrow(residual_pairs)
      )
    )

    for (
      i in seq_len(
        nrow(residual_pairs)
      )
    ) {

      cat(
        sprintf(
          "%-18s   %s
",
          "",
          residual_pairs$pair[i]
        )
      )
    }
  }


  invisible(
    list(
      n_total = n_total,
      n_used = n_used,
      observed_variables = n_observed,
      latent_factors = n_latent,
      factor_names = latent_names,
      parameters = n_par,
      df = df_model,
      estimator = estimator,
      residual_pairs = residual_pairs
    )
  )
}


# ==============================================================================
# 11A. FACTOR STRUCTURE
# ==============================================================================

print_factor_structure <- function(fit) {

  pe <- parameterEstimates(
    fit
  )

  latent_names <- lavNames(
    fit,
    type = "lv"
  )

  loading_tbl <- pe[
    pe$op == "=~",
    c(
      "lhs",
      "rhs"
    ),
    drop = FALSE
  ]


  cat(
    "
FACTOR STRUCTURE

"
  )


  for (
    factor_name in latent_names
  ) {

    indicators <- loading_tbl$rhs[
      loading_tbl$lhs == factor_name
    ]

    if (
      length(indicators) == 0
    ) {
      next
    }


    cat(
      sprintf(
        "%-15s : %d items
",
        factor_name,
        length(indicators)
      )
    )


    wrapped <- strwrap(
      paste(
        indicators,
        collapse = ", "
      ),
      width = 92
    )


    for (
      line in wrapped
    ) {

      cat(
        sprintf(
          "%-17s %s
",
          "",
          line
        )
      )
    }


    cat(
      "
"
    )
  }


  invisible(
    loading_tbl
  )
}


# ==============================================================================
# 12. MODEL FIT
# ==============================================================================

print_fit_compact <- function(fit) {

  fit_names <- c(
    "chisq.scaled",
    "df.scaled",
    "pvalue.scaled",
    "cfi.robust",
    "tli.robust",
    "rmsea.robust",
    "rmsea.ci.lower.robust",
    "rmsea.ci.upper.robust",
    "srmr",
    "aic",
    "bic"
  )


  all_fit <- fitMeasures(
    fit
  )


  available <- intersect(
    fit_names,
    names(all_fit)
  )


  values <- fitMeasures(
    fit,
    available
  )


  tbl <- data.frame(
    Index = names(values),
    Value = as.numeric(values),
    stringsAsFactors = FALSE
  )


  cat(
    "\nMODEL FIT\n\n"
  )


  cat(
    sprintf(
      "%-30s %12s\n",
      "Index",
      "Value"
    )
  )


  cat(
    strrep(
      "-",
      44
    ),
    "\n"
  )


  for (
    i in seq_len(
      nrow(tbl)
    )
  ) {

    value <- if (
      grepl(
        "pvalue",
        tbl$Index[i]
      )
    ) {

      format_p(
        tbl$Value[i]
      )

    } else {

      sprintf(
        "%.3f",
        tbl$Value[i]
      )
    }


    cat(
      sprintf(
        "%-30s %12s\n",
        tbl$Index[i],
        value
      )
    )
  }
}


# ==============================================================================
# 13. FACTOR LOADINGS + BUNYI ITEM
# ==============================================================================

print_factor_loadings <- function(
  fit,
  wording_width = 62
) {

  pe <- parameterEstimates(
    fit,
    standardized = TRUE
  )


  tbl <- pe[
    pe$op == "=~",
    c(
      "lhs",
      "rhs",
      "est",
      "se",
      "pvalue",
      "std.all"
    )
  ]


  names(tbl) <- c(
    "Factor",
    "Item",
    "Estimate",
    "SE",
    "p",
    "Std.Loading"
  )


  tbl$Item_Wording <- unname(
    item_labels[
      tbl$Item
    ]
  )


  cat(
    "\nFACTOR LOADINGS\n\n"
  )


  cat(
    sprintf(
      "%-11s %-10s %-64s %8s %8s %9s %8s\n",
      "Factor",
      "Item",
      "Bunyi Item",
      "Est.",
      "SE",
      "p",
      "Std."
    )
  )


  cat(
    strrep(
      "-",
      123
    ),
    "\n"
  )


  for (
    i in seq_len(
      nrow(tbl)
    )
  ) {

    wording <- clean_console_text(
      tbl$Item_Wording[i]
    )


    wrapped <- strwrap(
      wording,
      width = wording_width
    )


    p_text <- format_p(
      tbl$p[i]
    )


    cat(
      sprintf(
        "%-11s %-10s %-64s %8.3f %8.3f %9s %8.3f\n",
        tbl$Factor[i],
        tbl$Item[i],
        wrapped[1],
        tbl$Estimate[i],
        tbl$SE[i],
        p_text,
        tbl$Std.Loading[i]
      )
    )


    if (
      length(wrapped) > 1
    ) {

      for (
        j in 2:length(wrapped)
      ) {

        cat(
          sprintf(
            "%-11s %-10s %-64s\n",
            "",
            "",
            wrapped[j]
          )
        )
      }
    }


    cat(
      strrep(
        "-",
        123
      ),
      "\n"
    )
  }


  invisible(
    tbl
  )
}


# ==============================================================================
# 14. FACTOR CORRELATIONS
# ==============================================================================

print_factor_correlations <- function(fit) {

  pe <- parameterEstimates(
    fit,
    standardized = TRUE
  )


  latent <- lavNames(
    fit,
    type = "lv"
  )


  tbl <- pe[
    pe$op == "~~" &
      pe$lhs %in% latent &
      pe$rhs %in% latent &
      pe$lhs != pe$rhs,
    c(
      "lhs",
      "rhs",
      "est",
      "se",
      "pvalue",
      "std.all"
    )
  ]


  if (
    nrow(tbl) == 0
  ) {

    return(
      invisible(NULL)
    )
  }


  names(tbl) <- c(
    "Factor_1",
    "Factor_2",
    "Covariance",
    "SE",
    "p",
    "Correlation"
  )


  tbl$Covariance <- round(
    tbl$Covariance,
    3
  )

  tbl$SE <- round(
    tbl$SE,
    3
  )

  tbl$Correlation <- round(
    tbl$Correlation,
    3
  )


  tbl$p <- vapply(
    tbl$p,
    format_p,
    character(1)
  )


  cat(
    "\nFACTOR CORRELATIONS\n\n"
  )


  cat(
    sprintf(
      "%-15s %-15s %12s %9s %9s %12s\n",
      "Factor 1",
      "Factor 2",
      "Covariance",
      "SE",
      "p",
      "Correlation"
    )
  )


  cat(
    strrep(
      "-",
      78
    ),
    "\n"
  )


  for (
    i in seq_len(
      nrow(tbl)
    )
  ) {

    cat(
      sprintf(
        "%-15s %-15s %12.3f %9.3f %9s %12.3f\n",
        tbl$Factor_1[i],
        tbl$Factor_2[i],
        tbl$Covariance[i],
        tbl$SE[i],
        tbl$p[i],
        tbl$Correlation[i]
      )
    )
  }
}


# ==============================================================================
# 15. RELIABILITY
# ==============================================================================

print_reliability <- function(fit) {

  # Perhitungan reliability berasal dari get_rel_df() di R/helpers.R.
  # Fungsi ini hanya mengatur format output TXT/console.

  cat(
    "\nRELIABILITY\n\n"
  )


  rel <- get_rel_df(
    fit
  )


  rel <- round_numeric(
    rel,
    3
  )


  print(
    rel,
    row.names = FALSE
  )
}


# ==============================================================================
# 16. MODIFICATION INDICES
# ==============================================================================

# ==============================================================================
# MODIFICATION INDICES
# ==============================================================================

print_modification_indices <- function(
  fit,
  mi_cutoff = 10,
  top_n = 15,
  wording_width = 68
) {

  mi <- modificationIndices(
    fit,
    sort. = TRUE
  )

  # Filter berdasarkan cutoff
  mi <- mi[
    mi$mi >= mi_cutoff,
    ,
    drop = FALSE
  ]

  if (nrow(mi) == 0) {

    cat("\nMODIFICATION INDICES\n\n")

    cat(
      "Tidak ada Modification Index >= ",
      mi_cutoff,
      ".\n",
      sep = ""
    )

    return(
      invisible(NULL)
    )
  }

  # Ambil Top N
  if (
    !is.null(top_n) &&
    nrow(mi) > top_n
  ) {

    mi <- mi[
      seq_len(top_n),
      ,
      drop = FALSE
    ]
  }


  # ============================================================================
  # BUAT PARAMETER
  # ============================================================================

  mi$Parameter <- paste(
    mi$lhs,
    mi$op,
    mi$rhs
  )


  # ============================================================================
  # AMBIL BUNYI ITEM
  # ============================================================================

  mi$LHS_Wording <- vapply(
    mi$lhs,
    get_item_wording,
    character(1)
  )

  mi$RHS_Wording <- vapply(
    mi$rhs,
    get_item_wording,
    character(1)
  )


  # ============================================================================
  # HEADER
  # ============================================================================

  cat("\nMODIFICATION INDICES\n\n")

  cat(
    "Ditampilkan ",
    nrow(mi),
    " parameter dengan MI >= ",
    mi_cutoff,
    ".\n",
    sep = ""
  )

  cat(
    "MI digunakan sebagai diagnostik; modifikasi tetap harus didukung teori.\n\n"
  )


  cat(
    sprintf(
      "%-6s %-72s %-24s %10s %10s\n",
      "Rank",
      "Bunyi Item",
      "Parameter",
      "MI",
      "EPC"
    )
  )

  cat(
    strrep(
      "-",
      126
    ),
    "\n"
  )


  # ============================================================================
  # CETAK SETIAP MI
  # ============================================================================

  for (
    i in seq_len(
      nrow(mi)
    )
  ) {

    # --------------------------------------------------------------------------
    # Label LHS dan RHS
    # --------------------------------------------------------------------------

    lhs_label <- paste0(
      mi$lhs[i],
      ": ",
      mi$LHS_Wording[i]
    )

    rhs_label <- paste0(
      mi$rhs[i],
      ": ",
      mi$RHS_Wording[i]
    )


    # --------------------------------------------------------------------------
    # Wrap wording
    # --------------------------------------------------------------------------

    lhs_wrap <- strwrap(
      lhs_label,
      width = wording_width
    )

    rhs_wrap <- strwrap(
      rhs_label,
      width = wording_width
    )


    # --------------------------------------------------------------------------
    # Gabungkan dua item menjadi satu blok
    # --------------------------------------------------------------------------

    wording_lines <- c(
      lhs_wrap,
      rhs_wrap
    )


    # --------------------------------------------------------------------------
    # Baris pertama:
    # Rank + bunyi item + parameter + MI + EPC
    # --------------------------------------------------------------------------

    cat(
      sprintf(
        "%-6d %-72s %-24s %10.3f %10.3f\n",
        i,
        wording_lines[1],
        mi$Parameter[i],
        mi$mi[i],
        mi$epc[i]
      )
    )


    # --------------------------------------------------------------------------
    # Baris lanjutan:
    # hanya bunyi item
    # --------------------------------------------------------------------------

    if (
      length(wording_lines) > 1
    ) {

      for (
        j in 2:length(wording_lines)
      ) {

        cat(
          sprintf(
            "%-6s %-72s %-24s %10s %10s\n",
            "",
            wording_lines[j],
            "",
            "",
            ""
          )
        )
      }
    }


    cat(
      strrep(
        "-",
        126
      ),
      "\n"
    )
  }


  invisible(
    mi
  )
}

# ==============================================================================
# 17. FULL OUTPUT CFA
# ==============================================================================

print_cfa <- function(
  fit,
  title,
  data = NULL,
  items_removed = NULL,
  show_mi = TRUE,
  mi_cutoff = 10,
  mi_top_n = 15
) {

  if (
    is.null(items_removed)
  ) {

    items_removed <- get_removed_items(
      fit
    )
  }


  print_section(
    title
  )


  # --------------------------------------------------------------------------
  # 1. Model Info
  # --------------------------------------------------------------------------

  print_model_info(
    fit = fit,
    model_name = title,
    data = data,
    items_removed = items_removed
  )


  # --------------------------------------------------------------------------
  # 2. Factor Structure
  # --------------------------------------------------------------------------

  print_factor_structure(
    fit
  )


  # --------------------------------------------------------------------------
  # 3. Model Fit
  # --------------------------------------------------------------------------

  print_fit_compact(
    fit
  )


  # --------------------------------------------------------------------------
  # 4. Factor Loadings
  # --------------------------------------------------------------------------

  loading_tbl <- print_factor_loadings(
    fit
  )


  # --------------------------------------------------------------------------
  # 5. Factor Correlations
  # --------------------------------------------------------------------------

  print_factor_correlations(
    fit
  )


  # --------------------------------------------------------------------------
  # 6. Modification Indices
  # --------------------------------------------------------------------------

  if (
    isTRUE(show_mi)
  ) {

    print_modification_indices(
      fit,
      mi_cutoff = mi_cutoff,
      top_n = mi_top_n
    )
  }


  # --------------------------------------------------------------------------
  # 7. Reliability
  # --------------------------------------------------------------------------

  print_reliability(
    fit
  )


  invisible(
    loading_tbl
  )
}

# Laporan untuk script CFA yang dijalankan sendiri. Semua tahap tetap tercatat;
# rincian item, fit, dan reliability model akhir dicetak oleh print_cfa().
simpan_laporan_cfa <- function(konstruk, tahap, final, folder, faktor,
                              faktor_utama = NULL, perbandingan = list(),
                              diagnostik = list()) {
  pilihan_awal <- options(width = 160)
  on.exit(options(pilihan_awal), add = TRUE)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  tabel_fit <- do.call(rbind, lapply(names(tahap), function(nama) {
    fit_row(tahap[[nama]], nama)
  }))
  rel_dimensi <- get_first_order_reliability(final, konstruk, faktor)
  rel_konstruk <- if (is.null(faktor_utama)) NULL else {
    get_second_order_reliability(final, faktor_utama)
  }
  laporan <- capture.output({
    cat("CFA", konstruk, "| N =", lavaan::lavInspect(final, "nobs"), "| estimator = MLR\n")
    cat("\nRIWAYAT MODEL (urutan pengujian)\n")
    tabel_cetak <- round_numeric(tabel_fit[, c(
      "Model", "Observed_Items", "Chisq", "df", "p", "CFI", "TLI",
      "RMSEA", "RMSEA_L", "RMSEA_U", "SRMR", "Converged", "Heywood"
    )], 3)
    tabel_cetak$p <- ifelse(tabel_fit$p < .001, "<.001", sprintf("%.3f", tabel_fit$p))
    print(tabel_cetak, row.names = FALSE)
    if (length(unique(tabel_fit$Observed_Items)) > 1L) {
      cat("Perbandingan fit pada jumlah item berbeda bersifat deskriptif.\n")
    }
    if (length(perbandingan)) {
      cat("\nPERBANDINGAN MODEL DENGAN ITEM YANG SAMA\n")
      for (nama in names(perbandingan)) {
        cat("\n", nama, "\n", sep = "")
        print(perbandingan[[nama]])
      }
    }
    if (length(diagnostik)) {
      cat("\nDIAGNOSTIK TAHAP\n")
      for (nama in names(diagnostik)) {
        cat("\n", nama, "\n", sep = "")
        print(round_numeric(diagnostik[[nama]], 3), row.names = FALSE)
      }
    }
    cat("\nRINCIAN MODEL AKHIR\n")
    print_cfa(final, paste("CFA", konstruk, "- model akhir"), show_mi = TRUE)
    cat("\nRELIABILITY DAN AVE PER DIMENSI\n")
    print(round_numeric(rel_dimensi, 3), row.names = FALSE)
    if (!is.null(rel_konstruk)) {
      cat("\nRELIABILITY FAKTOR SECOND-ORDER\n")
      print(round_numeric(rel_konstruk, 3), row.names = FALSE)
    }
  })
  laporan <- laporan[!(trimws(laporan) == "" & c(FALSE, head(trimws(laporan) == "", -1)))]
  writeLines(laporan, file.path(folder, "laporan_cfa.txt"), useBytes = TRUE)
  invisible(tabel_fit)
}
