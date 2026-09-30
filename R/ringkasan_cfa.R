# Siapkan tabel gabungan CFA untuk laporan analisis lengkap dan tahap berikutnya.

# 1. Gabungkan pengujian model bertahap ----

tahap_fits <- list()
tahap_fit_table <- data.frame()
tahap_special <- data.frame()
if (RUN_MODEL_STAGES) {
  tahap_fits <- list(
    ESI_Baseline6 = fit_esi_baseline,
    ESI_Final6_Correlated23 = fit_esi_final_tahap,
    PSMLS_Awal4Correlated = fit_psmls_awal,
    PSMLS_Final14_SecondOrder = fit_psmls,
    PEB_Baseline19 = fit_peb_baseline19,
    PEB_Old14 = fit_peb_14,
    PEB_15_FourCorrelated = fit_peb_15_four_correlated,
    PEB_15_FourSecondOrder = fit_peb_15_four_second_order,
    PEB_ThreeFactor15 = fit_peb_15,
    PEB_16_FourCorrelated = fit_peb_16_four_correlated,
    PEB_Final16_FourSecondOrder = fit_peb_16_four_second_order,
    PEB_16_ThreeSecondOrder = fit_peb_16_three_second_order,
    PWB_Baseline42 = fit_pwb_baseline42,
    PWB_ReverseMethod42 = fit_pwb_method42,
    PWB_Old22 = fit_pwb_old22,
    PWB_Final21_Correlated = fit_pwb_correlated21,
    PWB_Final21_SecondOrder = fit_pwb_final21,
    PWB_M7_Add_PWB21R = fit_pwb_m7,
    PWB_Final21_Drop_PWB22 = fit_pwb_drop22,
    PWB_Final21_Drop_PWB26 = fit_pwb_drop26
  )

  tahap_fit_table <- do.call(
    rbind,
    lapply(
      names(tahap_fits),
      function(nm) {
        fit_row(
          tahap_fits[[nm]],
          nm
        )
      }
    )
  )

  # Key final-decision diagnostics.
  tahap_special <- rbind(
    cbind(
      Model = "PEB_Final16_FourSecondOrder",
      item_diagnostic(
        fit_peb_16_four_second_order,
        "PEB_15",
        "EnvCit"
      )
    ),
    cbind(
      Model = "PEB_Final16_FourSecondOrder",
      item_diagnostic(
        fit_peb_16_four_second_order,
        "PEB_18",
        "LandStew"
      )
    ),
    cbind(
      Model = "PWB_M7_Add_PWB21R",
      item_diagnostic(
        fit_pwb_m7,
        "PWB_21_R",
        "Growth"
      )
    )
  )

}

# Pemeriksaan dan output final ----
final_fits <- list(
  ESI = fit_esi,
  PSMLS = fit_psmls,
  PEB = fit_peb,
  PWB = fit_pwb
)

for (
  nm in names(final_fits)
) {
  fit <- final_fits[[nm]]

  if (
    !isTRUE(
      lavaan::lavInspect(
        fit,
        "converged"
      )
    )
  ) {
    stop(
      nm,
      " tidak konvergen."
    )
  }

  if (
    !isTRUE(
      lavaan::lavInspect(
        fit,
        "post.check"
      )
    )
  ) {
    stop(
      nm,
      " menghasilkan solusi yang tidak admissible."
    )
  }
}


# 2. Output model CFA final ----

final_fit_table <- do.call(
  rbind,
  lapply(
    names(final_fits),
    function(nm) {
      fit_row(
        final_fits[[nm]],
        nm
      )
    }
  )
)

final_loadings <- do.call(
  rbind,
  lapply(
    names(final_fits),
    function(nm) {
      loading_table(
        final_fits[[nm]],
        nm
      )
    }
  )
)



# 3. Reliabilitas dan AVE ----

cat("Menghitung reliability dan AVE...\n")

first_order_map <- list(
  ESI = c(
    "ESI"
  ),
  PSMLS = c(
    "TechComp",
    "SocialRel",
    "Privacy",
    "InfoAware"
  ),
  PEB = c(
    "ConsLife",
    "SocEnv",
    "EnvCit",
    "LandStew"
  ),
  PWB = c(
    "Autonomy",
    "Mastery",
    "Growth",
    "Relations",
    "Purpose",
    "Acceptance"
  )
)

reliability_first <- do.call(
  rbind,
  lapply(
    names(final_fits),
    function(nm) {
      get_first_order_reliability(
        final_fits[[nm]],
        nm,
        first_order_map[[nm]]
      )
    }
  )
)

reliability_second <- rbind(
  get_second_order_reliability(
    fit_psmls,
    "PSMLS"
  ),
  get_second_order_reliability(
    fit_peb,
    "PEB"
  ),
  get_second_order_reliability(
    fit_pwb,
    "PWB"
  )
)
