## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5, fig.height = 3
)
library(knitr)

## -----------------------------------------------------------------------------
  library(predtools)
  data(gusto)
  set.seed(1)

  gusto <- gusto[gusto$tx %in% c("tPA", "SK"), ]
  gusto$y      <- gusto$day30
  gusto$a      <- as.numeric(gusto$tx == "tPA")          # 1 = tPA (active), 0 = SK (control)
  gusto$female <- as.numeric(gusto$sex == "female")
  gusto$kill   <- (as.numeric(gusto$Killip) > 1) * 1
  gusto$miloc  <- relevel(factor(gusto$miloc), ref = "Inferior")
  gusto$sbp    <- pmin(gusto$sysbp, 100)

## -----------------------------------------------------------------------------
  dev_data <- gusto[!gusto$regl %in% c(1, 7, 9, 10, 11, 12, 14, 15), ]
  val_data <- gusto[ gusto$regl %in% c(1, 7, 9, 10, 11, 12, 14, 15), ]
  model <- glm(y ~ female + age + miloc + pmi + kill + sbp + pulse + a + a:female + a:age,
               data = dev_data, family = binomial(link = "logit"))

## -----------------------------------------------------------------------------
  val_data$pi0 <- predict(model, newdata = transform(val_data, a = 0), type = "response")  # baseline (control) risk
  val_data$pi1 <- predict(model, newdata = transform(val_data, a = 1), type = "response")  # risk under treatment
  val_data$d   <- val_data$pi0 - val_data$pi1                                              # predicted ITE

## -----------------------------------------------------------------------------
  library(cumulcalib)
  res <- cumulcalibITE(val_data$y, h = val_data$d, a = val_data$a, method = "BB")
  summary(res)

## -----------------------------------------------------------------------------
  plot(res)

## ----eval=FALSE---------------------------------------------------------------
#   # Conditional approach: supply the predicted baseline (control) risk via p
#   res_conditional <- cumulcalibITE(val_data$y, h = val_data$d, a = val_data$a,
#                                     p = val_data$pi0, method = "BB")

