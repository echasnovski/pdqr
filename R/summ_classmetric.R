#' Summarize pair of distributions with classification metric
#'
#' Compute metric of the following one-dimensional binary classification setup:
#' any `x` value not more than `threshold` value is classified as "negative"; if
#' strictly greater - "positive". Classification metrics are computed based on
#' two pdqr-functions: `f`, which represents the distribution of values which
#' *should be* classified as "negative" ("true negative"), and `g` - the same
#' for "positive" ("true positive").
#'
#' @param f A pdqr-function of any [type][meta_type()] and
#'   [class][meta_class()]. Represents distribution of "true negative" values.
#' @param g A pdqr-function of any type and class. Represents distribution of
#'   "true positive" values.
#' @param threshold A numeric vector of classification threshold(s).
#' @param method Method of classification metric (might be a vector for
#'   `summ_classmetric_df()`). Should be one of "TPR", "TNR", "FPR", "FNR",
#'   "PPV", "NPV", "FDR", "FOR", "LR+", "LR-", "Acc", "ER", "GM", "F1", "OP",
#'   "MCC", "YI", "MK", "Jaccard", "DOR" (with possible aliases, see Details).
#'
#' @details Binary classification setup used here to compute metrics is a
#' simplified version of the most common one, when there is a finite set of
#' already classified objects. Usually, there are `N` objects which are truly
#' "negative" and `P` truly "positive" ones. Values `N` and `P` can vary, which
#' often results in class imbalance. However, in current setup both `N` and
#' `P` are equal to 1 (total probability of `f` and `g`).
#'
#' In common setup, classification of all `N + P` objects results into the
#' following values: "TP" (number of truly "positive" values classified as
#' "positive"), "TN" (number of negatives classified as "negative"), "FP"
#' (number of negatives falsely classified as "positive"), and "FN" (number of
#' positives falsely classified as "negative"). In current setup all those
#' values are equal to respective "rates" (because `N` and `P` are both equal to
#' 1).
#'
#' Both `summ_classmetric()` and `summ_classmetric_df()` allow aliases to some
#' classification metrics (for readability purposes).
#'
#' Following classification metrics are available:
#' - Simple metrics:
#'     - *True positive rate*, `method` "TPR" (aliases: "TP", "sensitivity",
#'     "recall"): proportion of actual positives correctly classified as such.
#'     Computed as `1 - as_p(g)(threshold)`.
#'     - *True negative rate*, `method` "TNR" (aliases: "TN", "specificity"):
#'     proportion of actual negatives correctly classified as such. Computed as
#'     `as_p(f)(threshold)`.
#'     - *False positive rate*, `method` "FPR" (aliases: "FP", "fall-out"):
#'     proportion of actual negatives falsely classified as "positive". Computed
#'     as `1 - as_p(f)(threshold)`.
#'     - *False negative rate*, `method` "FNR" (aliases: "FN", "miss_rate"):
#'     proportion of actual positives falsely classified as "negative". Computed
#'     as `as_p(g)(threshold)`.
#'     - *Positive predictive value*, `method` "PPV" (alias: "precision"):
#'     proportion of output positives that are actually "positive". Computed as
#'     `TP / (TP + FP)`.
#'     - *Negative predictive value*, `method` "NPV": proportion of output
#'     negatives that are actually "negative". Computed as `TN / (TN + FN)`.
#'     - *False discovery rate*, `method` "FDR": proportion of output positives
#'     that are actually "negative". Computed as `FP / (TP + FP)`.
#'     - *False omission rate*, `method` "FOR": proportion of output negatives
#'     that are actually "positive". Computed as `FN / (TN + FN)`.
#'     - *Positive likelihood*, `method` "LR+": measures how much the odds of
#'     being "positive" increase when value is classified as "positive".
#'     Computed as `TPR / (1 - TNR)`.
#'     - *Negative likelihood*, `method` "LR-": measures how much the odds of
#'     being "positive" decrease when value is classified as "negative".
#'     Computed as `(1 - TPR) / TNR`.
#' - Combined metrics (for all, except "error rate", bigger value represents
#' better classification performance):
#'     - *Accuracy*, `method` "Acc" (alias: "accuracy"): proportion of total
#'     number of input values that were correctly classified. Computed as `(TP +
#'     TN) / 2` (here 2 is used because of special classification setup,
#'     `TP + TN + FP + FN = 2`).
#'     - *Error rate*, `method` "ER" (alias: "error_rate"): proportion of
#'     total number of input values that were incorrectly classified. Computed
#'     as `(FP + FN) / 2`.
#'     - *Geometric mean*, `method` "GM": geometric mean of TPR and TNR.
#'     Computed as `sqrt(TPR * TNR)`.
#'     - *F1 score*, `method` "F1": harmonic mean of PPV and TPR. Computed as
#'     `2*TP / (2*TP + FP + FN)`.
#'     - *Optimized precision*, `method` "OP": accuracy, penalized for
#'     imbalanced class performance. Computed as `Acc - abs(TPR - TNR) / (TPR +
#'     TNR)`.
#'     - *Matthews correlation coefficient*, `method` "MCC" (alias: "corr"):
#'     correlation between the observed and predicted classifications. Computed
#'     as `(TP*TN - FP*FN) / sqrt((TP+FP) * (TN+FN))` (here equalities `TP+FN =
#'     1` and `TN+FP = 1` are used to simplify formula).
#'     - *Youden’s index*, `method` "YI" (aliases: "youden", "informedness"):
#'     evaluates the discriminative power of the classification setup. Computed
#'     as `TPR + TNR - 1`.
#'     - *Markedness*, `method` "MK" (alias: "markedness"): evaluates the
#'     predictive power of the classification setup. Computed as `PPV + NPV -
#'     1`.
#'     - *Jaccard*, `method` "Jaccard": accuracy ignoring correct classification
#'     of negatives. Computed as `TP / (TP + FP + FN)`.
#'     - *Diagnostic odds ratio*, `method` "DOR" (alias: "odds_ratio"): ratio
#'     between positive and negative likelihoods. Computed as `"LR+" / "LR-"`.
#'
#' @return `summ_classmetric()` returns a numeric vector, of the same length as
#' `threshold`, representing classification metrics for different threshold
#' values.
#'
#' `summ_classmetric_df()` returns a data frame with rows corresponding to
#' `threshold` values. First column is "threshold" (with `threshold` values),
#' and all other represent classification metric for every input method (see
#' Examples).
#'
#' @seealso [summ_separation] for computing optimal separation threshold (which
#' is symmetrical with respect to `f` and `g`).
#'
#' @family summary functions
#'
#' @examples
#' d_unif <- as_d(dunif)
#' d_norm <- as_d(dnorm)
#' t_vec <- c(0, 0.5, 0.75, 1.5)
#'
#' summ_classmetric(d_unif, d_norm, threshold = t_vec, method = "F1")
#' summ_classmetric(d_unif, d_norm, threshold = t_vec, method = "Acc")
#'
#' summ_classmetric_df(
#'   d_unif, d_norm, threshold = t_vec, method = c("F1", "Acc")
#' )
#'
#' # Using method aliases
#' summ_classmetric_df(
#'   d_unif, d_norm, threshold = t_vec, method = c("TPR", "sensitivity")
#' )
#' @name summ_classmetric
NULL

#' @rdname summ_classmetric
#' @export
summ_classmetric <- function(f, g, threshold, method = "F1") {
  assert_pdqr_fun(f)
  assert_pdqr_fun(g)
  assert_missing(threshold, "classification threshold")
  assert_type(threshold, is.numeric)
  assert_method(method, methods_classmetric)

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

  classmetric(
    p_f_t = as_p(f)(threshold),
    p_g_t = as_p(g)(threshold),
    method = classmetric_aliases[method]
  )
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

methods_classmetric <- names(classmetric_aliases)

#' @rdname summ_classmetric
#' @export
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

  # Speed optimization (skips possibly expensive assertions)
  disable_asserting_locally()

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
    F1 = 2 * (1 - p_g_t) / (2 * (1 - p_g_t) + (1 - p_f_t) + p_g_t),
    # OP = Acc - abs(TPR - TNR) / (TPR + TNR)
    OP = classmetric_op(p_f_t, p_g_t),
    # MCC is computed based on explicit formula in `classmetric_mcc()`
    MCC = classmetric_mcc(p_f_t, p_g_t),
    # YI = TPR + TNR - 1
    YI = (1 - p_g_t) + p_f_t - 1,
    # MK = PPV + NPV - 1
    MK = classmetric(p_f_t, p_g_t, "PPV") +
      classmetric(p_f_t, p_g_t, "NPV") - 1,
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
  (tp * tn - fp * fn) / sqrt((tp + fp) * (tn + fn))
}
