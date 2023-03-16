### LIBRERIE
library(tidyverse)
library(yardstick)
library(boot)
library(gtsummary)

list.files(here::here("R"), pattern = "\\.R", full.names = TRUE) |>
  lapply(source) |>
  invisible()


### GLOBAL VARIABLES
reason_levels <- c(
  "Altro (specificare)",
  "ARDS",
  "BPCO Riacutizzata",
  "Complicanze Postoperatorie",
  "Polmonite",
  "Scompenso Cardiaco",
  "Sepsi",
  "Trauma - Politrauma"
)
metric_list <- c(
  "Balanced accuracy",
  "MCC",
  "CLAP-score",
  "Precision",
  "Recall"
)


batch_size <- 32

### LOAD STUFF
xgb_model <- readRDS(here::here("train_xgboost/trained_models/xgboost_tree/elastic_net/20230312003958_xgboost_tree_elastic_net_1_1_ensemble.RDS"))
e15a79 <- keras::load_model_hdf5(here::here("models/models_epoch-15_acc_0.79.hdf5"), custom_objects = NULL, compile = TRUE)

dd <- tar_read(dbTest)
db_test <- tar_read(dbTestNorm)

### PREPROCESS
test_generator <- create_batch_generator(db_test, batch_size)
test_n_batches <- db_test |>
  purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size)) |>
  sum()


### PREDICT

predRNN <- predict(
  object = e15a79,
  x = test_generator,
  batch_size = batch_size,
  steps = test_n_batches
  ) |>
  apply(1, which.max) -1


pred_obs <- purrr::map(1:28, ~dd[[.x]]$out |>
      tibble::as_tibble(rownames = "id_univoco") |>
      dplyr::mutate(
        pred0 = 0,
        pred1 = 1,
        pred2 = 2,
        #predRT = as.numeric(dd[[.x]]$daily[, .x, 2] == 12),
        predCoin = sample(0:2, size = length(pred0), replace = TRUE),
        predStrat = sample(0:2, size = length(pred0),
                           replace = TRUE, prob = c(78,12,10)
        ),
        dplyr::across(tidyselect::where(is.numeric),
                      ~ factor(.x, levels = c(0,1,2))
        ),
        predXbg = dd[[.x]]$daily[, .x, ] |>
          tibble::as_tibble(rownames = "id_univoco") |>
          dplyr::mutate(giorno_studio = .x) |>
          dplyr::left_join(
            dd[[.x]]$baseline |>
              tibble::as_tibble(rownames = "id_univoco"),
            by = "id_univoco"
          ) |>
          purrr::set_names(
            c("id_univoco", "sofa", "susp_tot", "ega_ph",
              "ega_pao2", "ega_paco2", "giorno_studio",
              "type", "sesso", "anni_eta", "bmi",
              "ibw", "saps", "reason")) |>
          dplyr::mutate(sbt = NA, test_set = TRUE,
                 type = factor(type, levels = c("nava", "psv")),
                 sesso = factor(sesso, levels = c("F", "M")),
                 reason = factor(reason, levels = reason_levels)
          ) |>
          familiar::predict(object = xgb_model) |>
          pull("predicted_class")
        ),
      id_univoco = rownames(dd[[.x]]$daily[, .x, ])
    ) |>
  dplyr::bind_rows(.id = "giorno_studio") |>
  dplyr::mutate(giorno_studio = as.integer(giorno_studio)) |>
  dplyr::bind_cols(
    predRNN = factor(predRNN, levels = c(0,1,2))
  )

### PERFORMANCE TABLE (no bootstrap)
purrr::map(colnames(pred_obs[4:11]),  ~ multi_metric(
  data = pred_obs,
  truth = value,
  estimate = {{.x}},
  estimator = "macro_weighted"
  ) |> dplyr::mutate(model = .x)) |>
  dplyr::bind_rows(.id = NULL) |>
  tidyr::pivot_wider(names_from = model, values_from = .estimate) |>
  dplyr::rename(
    "Metric" = .metric,
    "All 0" = pred0,
    "All 1" = pred1,
    "All 2" = pred2,
    "Coint toss" = predCoin,
    "Stratified random" = predStrat,
    "XG Boost" = predXbg,
    "AI model" = predRNN,
    "Readiness Test" = predRT
  ) |>
  dplyr::mutate( Metric = metric_list) |>
  dplyr::select(-.estimator) |>
  gt::gt() |>
  gt::fmt_number(
    columns = 2:9,
    decimals = 2
  )


### PLOT Predictions
pred_obs |>
  dplyr::mutate(
    predicted = predXbg,
    observed = value,
    patient = id_univoco,
    suggested  = predicted,
    across(c(observed, predicted),
           ~ .x |>
             as.character() |>
             forcats::fct_recode(
               # "invasive MV" = "0",
               # "invasive MV" = "2",
               # "Successful MV Stop" = "1"
               "SBT n.a" = "0",
               "SBT ok" = "1",
               "SBT fail" = "2"
             ) |>
             forcats::fct_relevel(
               #c("invasive MV", "Successful MV Stop")
               c("SBT n.a","SBT fail", "SBT ok")
             )
    ),
    across(c(suggested),
           ~ .x |>
             as.character() |>
             forcats::fct_recode(
               "continue MV" = "0",
               "continue MV" = "2",
               "SBT" = "1"
             ) |>
             forcats::fct_relevel(
               c("continue MV", "SBT")
             )
    )
  ) |>
  ggplot(aes( x = giorno_studio)) +
  geom_step(
    aes( y = observed,
         group = "observed"),
    color = "grey70",
    direction = "mid") +
  geom_point(
    aes(y = observed),
    color = "grey70",
    size = 5) +
  geom_point(aes(
    y = predicted,
    color = suggested),
    size = 2) +
  facet_wrap(~patient, scales = "free_x") +
  scale_color_brewer(palette = "Dark2", direction=-1) +
  labs(
    title = "Predicted vs Observed clinical course",
    x = "Days of study",
    y = "",
    color = "AI Model suggestion"
  ) +
  theme(legend.position = "top")



### BOOTSTRAP

bs_df <- names(select(pred_obs, pred0:predRNN)) |>
    purrr::set_names() |>
    map(
      ~{
        aux <- boot(data = pred_obs,
                    statistic = bs,
                    R = 1e3,
                    model = {{.x}})[["t"]]
        colnames(aux) <- metric_list
        as_tibble(aux)
      }
    ) |>
    bind_rows(.id = "model")

# saveRDS(bs_df, "bs_df.RDS")

bs_df |>
  select(-NPV) |>
  mutate( model = case_when(
    model == "pred0" ~ "All 0",
    model == "pred1" ~ "All 1",
    model == "pred2" ~ "All 2",
    model == "predCoin" ~ "Coin toss",
    model == "predRNN" ~ "AI Model",
    model == "predStrat" ~ "Stratified random",
    model == "predXbg" ~ "XG Boost"
  )) |>
  tbl_summary(
    by = model,
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{median}", "({p5}, {p95})"),
    digits = all_continuous() ~ 2,
    missing = "no"
  )

### CONFRONTO pred_obs con suggerimento binario

pred_obs_binary <- pred_obs |>
  mutate(
    across(where(is.factor),
           ~ forcats::fct_collapse(.x, "0" = c("0","2"))
    ))

purrr::map(colnames(pred_obs[4:10]),  ~ multi_metric_bin(
  data = pred_obs_binary,
  truth = value,
  estimate = {{.x}},
  estimator = "binary"
) |> dplyr::mutate(model = .x)) |>
  dplyr::bind_rows(.id = NULL) |>
  tidyr::pivot_wider(names_from = model, values_from = .estimate) |>
  dplyr::rename(
    "Metric" = .metric,
    "All 0" = pred0,
    "All 1" = pred1,
    "All 2" = pred2,
    "Coint toss" = predCoin,
    "Stratified random" = predStrat,
    # "Readiness Test" = predRT
    "XG Boost" = predXbg,
    "AI model" = predRNN
  ) |>
  dplyr::mutate( Metric = metric_list) |>
  dplyr::select(-.estimator) |>
  gt::gt() |>
  gt::fmt_number(
    columns = 2:8,
    decimals = 2
  )
