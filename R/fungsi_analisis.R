# Fungsi tabel, reliabilitas, diagnostik, dan skor Bartlett.

# Pastikan seluruh indikator model tersedia dan layak dianalisis.
cek_item_cfa <- function(data, model) {
  pt <- lavaan::lavaanify(model)
  latent <- unique(pt$lhs[pt$op == "=~"])
  items <- unique(setdiff(pt$rhs[pt$op == "=~"], latent))
  missing_items <- setdiff(items, names(data))
  if (length(missing_items)) {
    stop("Kolom item tidak ada di data: ", paste(missing_items, collapse = ", "))
  }
  if (!all(vapply(data[items], is.numeric, logical(1)))) {
    stop("Semua item CFA harus numerik.")
  }
  if (anyNA(data[items]) || any(!is.finite(as.matrix(data[items])))) {
    stop("Ada missing / non-finite value pada item analisis.")
  }
  invisible(items)
}

# 1. Fungsi fit, item, reliabilitas, dan regresi ----

safe_fit_value <- function(
  fit,
  primary,
  fallback = NULL
) {
  fm <- lavaan::fitMeasures(fit)

  if (
    primary %in% names(fm) &&
      is.finite(fm[[primary]])
  ) {
    return(unname(fm[[primary]]))
  }

  if (
    !is.null(fallback) &&
      fallback %in% names(fm) &&
      is.finite(fm[[fallback]])
  ) {
    return(unname(fm[[fallback]]))
  }

  NA_real_
}


fit_row <- function(
  fit,
  label
) {
  pe <- lavaan::parameterEstimates(fit)
  ov <- lavaan::lavNames(fit, type = "ov")

  heywood <- any(
    pe$op == "~~" &
      pe$lhs == pe$rhs &
      pe$lhs %in% ov &
      pe$est < 0,
    na.rm = TRUE
  )

  data.frame(
    Model = label,
    N = lavaan::lavInspect(fit, "nobs"),
    Observed_Items = length(ov),
    Converged = lavaan::lavInspect(fit, "converged"),
    Chisq = safe_fit_value(fit, "chisq.scaled", "chisq"),
    df = safe_fit_value(fit, "df.scaled", "df"),
    p = safe_fit_value(fit, "pvalue.scaled", "pvalue"),
    CFI = safe_fit_value(fit, "cfi.robust", "cfi.scaled"),
    TLI = safe_fit_value(fit, "tli.robust", "tli.scaled"),
    RMSEA = safe_fit_value(fit, "rmsea.robust", "rmsea.scaled"),
    RMSEA_L = safe_fit_value(
      fit,
      "rmsea.ci.lower.robust",
      "rmsea.ci.lower.scaled"
    ),
    RMSEA_U = safe_fit_value(
      fit,
      "rmsea.ci.upper.robust",
      "rmsea.ci.upper.scaled"
    ),
    SRMR = safe_fit_value(fit, "srmr"),
    AIC = safe_fit_value(fit, "aic"),
    BIC = safe_fit_value(fit, "bic"),
    Heywood = heywood,
    stringsAsFactors = FALSE
  )
}


loading_table <- function(
  fit,
  construct
) {
  pe <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE,
    ci = TRUE
  )

  x <- pe[
    pe$op == "=~",
    c(
      "lhs",
      "rhs",
      "est",
      "se",
      "z",
      "pvalue",
      "ci.lower",
      "ci.upper",
      "std.all"
    )
  ]

  names(x) <- c(
    "Factor",
    "Indicator",
    "Estimate",
    "SE",
    "z",
    "p",
    "CI_L",
    "CI_U",
    "Std_Loading"
  )

  x$Construct <- construct

  x[
    ,
    c(
      "Construct",
      "Factor",
      "Indicator",
      "Estimate",
      "SE",
      "z",
      "p",
      "CI_L",
      "CI_U",
      "Std_Loading"
    )
  ]
}


mi_table <- function(
  fit,
  cutoff = 10,
  top_n = 30
) {
  mi <- lavaan::modindices(
    fit,
    sort. = TRUE
  )

  mi <- mi[
    mi$mi >= cutoff,
  ]

  if (
    nrow(mi) > top_n
  ) {
    mi <- mi[
      seq_len(top_n),
    ]
  }

  if (!nrow(mi)) {
    return(data.frame())
  }

  mi$Parameter <- paste(
    mi$lhs,
    mi$op,
    mi$rhs
  )

  mi[
    ,
    c(
      "Parameter",
      "lhs",
      "op",
      "rhs",
      "mi",
      "epc",
      "sepc.all"
    )
  ]
}


item_diagnostic <- function(
  fit,
  item,
  intended_factor
) {
  pe <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE
  )

  loading <- pe[
    pe$op == "=~" &
      pe$lhs == intended_factor &
      pe$rhs == item,
    "std.all"
  ]

  loading <- if (
    length(loading)
  ) {
    loading[1]
  } else {
    NA_real_
  }

  mi <- lavaan::modindices(fit)

  involved <- mi[
    mi$lhs == item |
      mi$rhs == item,
  ]

  residual <- mi[
    mi$op == "~~" &
      mi$lhs != mi$rhs &
      (
        mi$lhs == item |
          mi$rhs == item
      ),
  ]

  if (nrow(residual)) {
    residual <- residual[
      order(
        residual$mi,
        decreasing = TRUE
      ),
    ]

    max_resid <- residual$mi[1]

    partner <- if (
      residual$lhs[1] == item
    ) {
      residual$rhs[1]
    } else {
      residual$lhs[1]
    }
  } else {
    max_resid <- NA_real_
    partner <- NA_character_
  }

  cross <- mi[
    mi$op == "=~" &
      mi$rhs == item &
      mi$lhs != intended_factor,
  ]

  if (nrow(cross)) {
    cross <- cross[
      order(
        cross$mi,
        decreasing = TRUE
      ),
    ]

    max_cross <- cross$mi[1]
    cross_factor <- cross$lhs[1]
  } else {
    max_cross <- NA_real_
    cross_factor <- NA_character_
  }

  data.frame(
    Item = item,
    Intended_Factor = intended_factor,
    Std_Loading = loading,
    MI_Count_GE10 = sum(
      involved$mi >= 10,
      na.rm = TRUE
    ),
    Max_Residual_MI = max_resid,
    Residual_Partner = partner,
    Max_CrossLoading_MI = max_cross,
    CrossLoading_Factor = cross_factor,
    stringsAsFactors = FALSE
  )
}


round_numeric <- function(
  x,
  digits = 4
) {
  out <- x

  is_num <- vapply(
    out,
    is.numeric,
    logical(1)
  )

  out[is_num] <- lapply(
    out[is_num],
    round,
    digits = digits
  )

  out
}


calc_rel <- function(
  fit,
  tau.eq,
  higher = character(0)
) {
  args <- list(
    object = fit,
    tau.eq = tau.eq,
    obs.var = TRUE,
    higher = higher,
    return.total = FALSE,
    return.df = TRUE
  )

  formal_names <- names(
    formals(
      semTools::compRelSEM
    )
  )

  if (!("higher" %in% formal_names)) {
    stop(
      "Versi semTools tidak mendukung argumen higher. ",
      "Perbarui semTools."
    )
  }

  if ("simplify" %in% formal_names) {
    args$simplify <- TRUE
  }

  do.call(
    semTools::compRelSEM,
    args
  )
}


extract_rel <- function(
  result,
  factor
) {
  if (
    is.data.frame(result) ||
      is.matrix(result)
  ) {
    if (
      factor %in% colnames(result) &&
        nrow(result) == 1L
    ) {
      value <- as.numeric(
        result[, factor]
      )
    } else {
      stop(
        "Format compRelSEM tidak dikenali untuk factor ",
        factor
      )
    }
  } else {
    flat <- unlist(
      result,
      recursive = TRUE,
      use.names = TRUE
    )

    idx <- which(
      names(flat) == factor
    )

    if (!length(idx)) {
      idx <- which(
        endsWith(
          names(flat),
          paste0(".", factor)
        )
      )
    }

    if (length(idx) != 1L) {
      stop(
        "Koefisien reliability tidak ditemukan secara unik: ",
        factor
      )
    }

    value <- as.numeric(
      flat[idx]
    )
  }

  if (
    length(value) != 1L ||
      !is.finite(value)
  ) {
    stop(
      "Reliability tidak finite: ",
      factor
    )
  }

  value
}


get_first_order_reliability <- function(
  fit,
  construct,
  first_order_factors
) {
  alpha <- calc_rel(
    fit,
    tau.eq = TRUE
  )

  omega <- calc_rel(
    fit,
    tau.eq = FALSE
  )

  pe <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE
  )

  loads <- pe[
    pe$op == "=~",
    c(
      "lhs",
      "rhs",
      "std.all"
    )
  ]

  ov <- lavaan::lavNames(
    fit,
    type = "ov"
  )

  item_loads <- loads[
    loads$rhs %in% ov,
  ]

  do.call(
    rbind,
    lapply(
      first_order_factors,
      function(f) {
        lam <- item_loads$std.all[
          item_loads$lhs == f
        ]

        data.frame(
          Construct = construct,
          Factor = f,
          Items = length(lam),
          Alpha = extract_rel(
            alpha,
            f
          ),
          Omega = extract_rel(
            omega,
            f
          ),
          AVE_item_std = mean(
            lam^2
          ),
          Loading_min = min(lam),
          Loading_max = max(lam),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}


get_second_order_reliability <- function(
  fit,
  construct
) {
  omega_higher <- calc_rel(
    fit,
    tau.eq = FALSE,
    higher = construct
  )

  pe <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE
  )

  lam <- pe$std.all[
    pe$op == "=~" &
      pe$lhs == construct
  ]

  data.frame(
    Construct = construct,
    Omega_second_order = extract_rel(
      omega_higher,
      construct
    ),
    Dimensions = length(lam),
    Mean_squared_second_order_loading = mean(lam^2),
    Loading_min = min(lam),
    Loading_max = max(lam),
    stringsAsFactors = FALSE
  )
}


regression_table_hc3 <- function(
  model
) {
  V <- sandwich::vcovHC(
    model,
    type = "HC3"
  )

  ct <- lmtest::coeftest(
    model,
    vcov. = V
  )

  df_res <- stats::df.residual(model)

  crit <- stats::qt(
    .975,
    df = df_res
  )

  data.frame(
    Term = rownames(ct),
    b = ct[, 1],
    SE = ct[, 2],
    t = ct[, 3],
    p = ct[, 4],
    CI_L = ct[, 1] - crit * ct[, 2],
    CI_U = ct[, 1] + crit * ct[, 2],
    SE_Type = "HC3",
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


regression_table_ols <- function(
  model
) {
  sm <- summary(model)$coefficients
  ci <- confint(model)

  data.frame(
    Term = rownames(sm),
    b = sm[, 1],
    SE = sm[, 2],
    t = sm[, 3],
    p = sm[, 4],
    CI_L = ci[, 1],
    CI_U = ci[, 2],
    SE_Type = "OLS",
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


simple_slopes_hc3 <- function(
  model,
  moderator_values
) {
  V <- sandwich::vcovHC(
    model,
    type = "HC3"
  )

  co <- coef(model)

  if (
    !"ESI_c" %in% names(co) ||
      !"ESI_c:PSMLS_c" %in% names(co)
  ) {
    stop(
      "Nama koefisien model moderasi tidak sesuai."
    )
  }

  b1 <- co["ESI_c"]
  b3 <- co["ESI_c:PSMLS_c"]

  df_res <- stats::df.residual(model)
  crit <- stats::qt(.975, df_res)

  out <- lapply(
    names(moderator_values),
    function(label) {
      w <- moderator_values[[label]]

      est <- b1 + b3 * w

      var_est <-
        V["ESI_c", "ESI_c"] +
        w^2 * V["ESI_c:PSMLS_c", "ESI_c:PSMLS_c"] +
        2 * w * V["ESI_c", "ESI_c:PSMLS_c"]

      se <- sqrt(var_est)

      tval <- est / se

      pval <- 2 * stats::pt(
        abs(tval),
        df = df_res,
        lower.tail = FALSE
      )

      data.frame(
        PSMLS_Level = label,
        PSMLS_c = w,
        Slope_ESI_to_PEB = est,
        SE_HC3 = se,
        t = tval,
        p = pval,
        CI_L = est - crit * se,
        CI_U = est + crit * se,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(
    rbind,
    out
  )
}


# 2. Fungsi skor Bartlett ----

# This helper is only a fallback for a single continuous higher-order target
# when lavPredict(method = "Bartlett") does not return a usable score.
#
# With eta = B eta + zeta and y = nu + Lambda eta + epsilon:
#   A     = Lambda %*% solve(I - B)
#   L     = A[, target]
#   Omega = Theta + A %*% Psi_without_target %*% t(A)
#
# The marginal measurement model is:
#   y = mu + L * target + error
#
# Bartlett weights:
#   w = (L' Omega^-1 L)^-1 L' Omega^-1
#
# The implementation below also verifies the covariance reconstruction,
# normalization of the weights, equivalence of GLS forms, and the expected
# identity between regression and Bartlett scores.

bartlett_higher_order <- function(
  fit,
  data,
  target
) {
  if (
    lavaan::lavInspect(fit, "ngroups") != 1L ||
      isTRUE(lavaan::lavInspect(fit, "categorical"))
  ) {
    stop(
      "Fallback Bartlett higher-order memerlukan satu grup dan indikator kontinu."
    )
  }

  est <- lavaan::lavInspect(
    fit,
    "est"
  )

  Lambda <- est$lambda
  B <- est$beta
  Psi <- est$psi
  Theta <- est$theta

  if (
    is.null(B) ||
      !target %in% colnames(Lambda)
  ) {
    stop(
      "Faktor higher-order tidak ditemukan: ",
      target
    )
  }

  lv <- colnames(Lambda)
  ov <- rownames(Lambda)

  B <- B[
    lv,
    lv,
    drop = FALSE
  ]

  Psi <- Psi[
    lv,
    lv,
    drop = FALSE
  ]

  Theta <- Theta[
    ov,
    ov,
    drop = FALSE
  ]

  k <- match(
    target,
    lv
  )

  # The fallback is intentionally narrow: one exogenous higher-order target
  # without direct observed indicators.
  if (
    any(
      abs(
        B[k, ]
      ) > 1e-10
    ) ||
      any(
        abs(
          Psi[k, -k]
        ) > 1e-10
      ) ||
      any(
        abs(
          Lambda[, k]
        ) > 1e-10
      )
  ) {
    stop(
      "Fallback tidak sesuai untuk target ini: ",
      target
    )
  }

  if (
    !all(
      vapply(
        data[
          ,
          ov,
          drop = FALSE
        ],
        is.numeric,
        logical(1)
      )
    )
  ) {
    stop(
      "Indikator harus numerik untuk scoring ",
      target
    )
  }

  Y <- as.matrix(
    data[
      ,
      ov,
      drop = FALSE
    ]
  )

  if (
    any(!is.finite(Y)) ||
      nrow(Y) != as.integer(
        lavaan::lavInspect(
          fit,
          "nobs"
        )
      )
  ) {
    stop(
      "Data scoring tidak lengkap / tidak sama dengan sampel CFA: ",
      target
    )
  }

  A <- Lambda %*% solve(
    diag(
      length(lv)
    ) - B
  )

  L <- A[
    ,
    k,
    drop = FALSE
  ]

  phi <- as.numeric(
    Psi[k, k]
  )

  Psi_without_target <- Psi
  Psi_without_target[k, ] <- 0
  Psi_without_target[, k] <- 0

  Omega <-
    Theta +
    A %*%
    Psi_without_target %*%
    t(A)

  Sigma <- lavaan::lavInspect(
    fit,
    "cov.ov"
  )[
    ov,
    ov,
    drop = FALSE
  ]

  reconstruction_error <- max(
    abs(
      Sigma -
        (
          L %*%
            t(L) *
            phi +
            Omega
        )
    )
  )

  if (
    !is.finite(phi) ||
      phi <= 0 ||
      reconstruction_error >
        1e-7 *
          max(
            1,
            max(
              abs(Sigma)
            )
          )
  ) {
    stop(
      "Dekomposisi covariance higher-order gagal: ",
      target
    )
  }

  # Stop rather than silently using a pseudo-inverse.
  chol(Sigma)
  chol(Omega)

  Sinv_L <- solve(
    Sigma,
    L
  )

  information <- as.numeric(
    crossprod(
      L,
      Sinv_L
    )
  )

  if (
    !is.finite(information) ||
      information <= 0
  ) {
    stop(
      "Bobot Bartlett tidak dapat dihitung: ",
      target
    )
  }

  weights <- Sinv_L / information

  Oinv_L <- solve(
    Omega,
    L
  )

  weights_residual <-
    Oinv_L /
      as.numeric(
        crossprod(
          L,
          Oinv_L
        )
      )

  normalization_error <- abs(
    as.numeric(
      crossprod(
        weights,
        L
      )
    ) - 1
  )

  weights_error <- max(
    abs(
      weights -
        weights_residual
    )
  )

  if (
    normalization_error > 1e-7 ||
      weights_error >
        1e-7 *
          max(
            1,
            max(
              abs(weights)
            )
          )
  ) {
    stop(
      "Verifikasi bobot Bartlett gagal: ",
      target
    )
  }

  mu <- colMeans(Y)
  latent_mean <- 0

  if (
    isTRUE(
      lavaan::lavInspect(
        fit,
        "meanstructure"
      )
    )
  ) {
    mu <- lavaan::lavInspect(
      fit,
      "mean.ov"
    )[ov]

    latent_mean <- lavaan::lavInspect(
      fit,
      "mean.lv"
    )[[target]]
  }

  scores <- as.numeric(
    sweep(
      Y,
      2L,
      mu,
      "-"
    ) %*%
      weights +
      latent_mean
  )

  if (
    any(!is.finite(scores)) ||
      stats::sd(scores) <= 1e-10
  ) {
    stop(
      "Fallback Bartlett menghasilkan score tidak valid: ",
      target
    )
  }

  # Independent cross-check against regression scores from the same CFA.
  rho <- phi * information

  reg <- lavaan::lavPredict(
    fit,
    type = "lv",
    method = "regression",
    transform = FALSE
  )[
    ,
    target
  ]

  regression_check <- max(
    abs(
      reg -
        (
          latent_mean +
            rho *
              (
                scores -
                  latent_mean
              )
        )
    )
  )

  if (
    !is.finite(regression_check) ||
      rho <= 0 ||
      rho > 1 + 1e-7 ||
      regression_check >
        1e-6 *
          max(
            1,
            max(
              abs(reg)
            )
          )
  ) {
    stop(
      "Verifikasi silang Bartlett/regression gagal: ",
      target
    )
  }

  list(
    scores = scores,
    method = "collapsed-loading Bartlett fallback",
    pemeriksaan = data.frame(
      Factor = target,
      N = length(scores),
      SD = stats::sd(scores),
      Min = min(scores),
      Max = max(scores),
      Model_score_reliability = rho,
      Covariance_reconstruction_error = reconstruction_error,
      Weight_normalization_error = normalization_error,
      GLS_vs_residual_weights_error = weights_error,
      Regression_identity_error = regression_check,
      stringsAsFactors = FALSE
    )
  )
}


get_bartlett_outcome_score <- function(
  fit,
  data,
  target
) {
  direct <- try(
    lavaan::lavPredict(
      fit,
      type = "lv",
      method = "Bartlett",
      transform = FALSE
    ),
    silent = TRUE
  )

  if (
    !inherits(
      direct,
      "try-error"
    )
  ) {
    direct_df <- as.data.frame(
      direct
    )

    if (
      target %in% names(direct_df)
    ) {
      x <- direct_df[[target]]

      if (
        length(x) == nrow(data) &&
          all(is.finite(x)) &&
          is.finite(
            stats::sd(x)
          ) &&
          stats::sd(x) > 1e-10
      ) {
        return(
          list(
            scores = as.numeric(x),
            method = "lavaan::lavPredict Bartlett",
            pemeriksaan = data.frame(
              Factor = target,
              N = length(x),
              SD = stats::sd(x),
              Min = min(x),
              Max = max(x),
              Model_score_reliability = NA_real_,
              Covariance_reconstruction_error = NA_real_,
              Weight_normalization_error = NA_real_,
              GLS_vs_residual_weights_error = NA_real_,
              Regression_identity_error = NA_real_,
              stringsAsFactors = FALSE
            )
          )
        )
      }
    }
  }

  message(
    "lavPredict Bartlett tidak menghasilkan score higher-order yang usable untuk ",
    target,
    "; memakai fallback Bartlett dari fitted CFA."
  )

  bartlett_higher_order(
    fit,
    data,
    target
  )
}
