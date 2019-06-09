summ_classmetric <- function(f, g, threshold, method = "F1") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_missing(threshold, "classification threshold")
  assert_type(threshold, is.numeric)
  assert_type(method, is_string)
  assert_in_set(method, names(classmetric_aliases))

  classmetric(
    p_f_t = as_p(f)(threshold),
    p_g_t = as_p(g)(threshold),
    method = classmetric_aliases[method]
  )
}

summ_classmetric_df <- function(f, g, threshold, method = "F1") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_missing(threshold, "classification threshold")
  assert_type(threshold, is.numeric)
  assert_type(method, is.character)
  if (!all(method %in% names(classmetric_aliases))) {
    stop_collapse(
      "`method` should contain only values allowed in `summ_classmetric()`."
    )
  }

  p_f_t <- as_p(f)(threshold)
  p_g_t <- as_p(g)(threshold)

  res <- data.frame(threshold = threshold)
  for (meth in method) {
    res[[meth]] <- classmetric(p_f_t, p_g_t, classmetric_aliases[meth])
  }

  res
}

#' @param p_f_t Vector of cumulative probabilites of "negative" distribution at
#'   some thresholds.
#' @param p_g_t Vector of cumulative probabilites of "positive" distribution at
#'   the same thresholds, as in `p_g_t`.
#'
#' @noRd
classmetric <- function(p_f_t, p_g_t, method) {
  # In 'pdqr' setup, total amount of "negative" and "positive" values is 1 for
  # each. In classification terminology, it means that `P = 1` and `N = 1`.
  # This also implies that quantities "TP" (amount of "true positive"), "TN"
  # (amount of "true negative"), "FP" (amount of "false positive"), and "FN"
  # (amount of "false negative") are equal to corresponding "rates".
  switch(
    method,
    # Simple metrics

    # TPR is a proportion of actual positives correctly identified
    TPR = 1 - p_g_t,
    # TNR is a proportion of actual negatives correctly identified
    TNR = p_f_t,
    # FPR is a proportion of actual negatives not correctly identified (as pos.)
    FPR = 1 - p_f_t,
    # FNR is a proportion of actual positives not correctly identified (as neg.)
    FNR = p_g_t,
    # PPV = TP / (TP + FP)
    PPV = (1 - p_g_t) / ((1 - p_g_t) + (1 - p_f_t)),
    # NPV = TN / (TN + FN)
    NPV = p_f_t / (p_f_t + p_g_t),
    # FDR = FP / (FP + TP)
    FDR = (1 - p_f_t) / ((1 - p_f_t) + (1 - p_g_t)),
    # FOR = FN / (FN + TN)
    FOR = p_g_t / (p_g_t + p_f_t),
    # LR+ = TPR / (1 - TNR)
    `LR+` = (1 - p_g_t) / (1 - p_f_t),
    # LR- = (1 - TPR) / TNR
    `LR-` = p_g_t / p_f_t,

    # Combined metrics

    # Ac = (TP + TN) / (TP + TN + FP + FN)
    Acc = ((1 - p_g_t) + p_f_t) / 2,
    # ER = (FP + FN) / (TP + TN + FP + FN)
    ER = ((1 - p_f_t) + p_g_t) / 2,
    # GM = sqrt(TPR * TNR)
    GM = sqrt((1 - p_g_t) * p_f_t),
    # F1 = 2*TP / (2*TP + FP + FN)
    F1 = 2 * (1 - p_g_t) / (2*(1 - p_g_t) + (1 - p_f_t) + p_g_t),
    # OP = ACC - abs(TPR - TNR) / (TPR + TNR)
    OP = classmetric_op(p_f_t, p_g_t),
    # MCC is computed based on explicit formula in `classmetric_mcc()`
    MCC = classmetric_mcc(p_f_t, p_g_t),
    # YI = TPR + TNR - 1
    YI = (1 - p_g_t) + p_f_t - 1,
    # MK = PPV + NPV - 1
    MK = classmetric(p_f_t, p_g_t, "PPV")+classmetric(p_f_t, p_g_t, "NPV")-1,
    # Jaccard = TP / (TP + FN + FP)
    Jaccard = (1 - p_g_t) / (1 + (1 - p_f_t)),
    # DOR = LR+ / LR-
    DOR = ((1 - p_g_t) / (1 - p_f_t)) / (p_g_t / p_f_t)
  )
}

# OP is "Optimization Precision"
classmetric_op <- function(p_f_t, p_g_t) {
  tpr <- classmetric(p_f_t, p_g_t, "TPR")
  tnr <- classmetric(p_f_t, p_g_t, "TNR")
  acc <- classmetric(p_f_t, p_g_t, "Acc")

  acc - abs(tpr - tnr) / (tpr + tnr)
}

# MCC is "Matthews Correlation Coefficient"
classmetric_mcc <- function(p_f_t, p_g_t) {
  tp <- 1 - p_g_t
  tn <- p_f_t
  fp <- 1 - p_f_t
  fn <- p_g_t

  # TP + FN = 1; TN + FP = 1
  (tp*tn - fp*fn) / sqrt((tp + fp) * (tn + fn))
}

#' Aliases of methods for `summ_classmetric()`
#'
#' This is a vector of aliases for possible values of `method` argument in
#' `summ_classmetric()`. Its names are all possible values of `method`, and
#' values - "canonical" method name.
#'
#' @noRd
classmetric_aliases <- c(
  # Simple metrics
  TPR   = "TPR", TP = "TPR", sensitivity = "TPR", recall = "TPR",
  TNR   = "TNR", TN = "TNR", specificity = "TNR",
  FPR   = "FPR", FP = "FPR", `fall-out` = "FPR",
  FNR   = "FNR", FN = "FNR", miss_rate = "FNR",
  PPV   = "PPV", precision = "PPV",
  NPV   = "NPV",
  FDR   = "FDR",
  FOR   = "FOR",
  `LR+` = "LR+",
  `LR-` = "LR-",
  # Combined metrics
  Acc   = "Acc", accuracy = "Acc",
  ER    = "ER", error_rate = "ER",
  GM    = "GM",
  F1    = "F1",
  OP    = "OP",
  MCC   = "MCC", corr = "MCC",
  YI    = "YI", youden = "YI", informedness = "YI",
  MK    = "MK", markedness = "MK",
  Jaccard = "Jaccard",
  DOR   = "DOR", odds_ratio = "DOR"
)
