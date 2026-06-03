# Regression test for the score_irt() column-order bug.
#
# mirt::fscores(mod, response.pattern = data) matches the columns of
# response.pattern to the model's items *by position, not by name* (verified:
# reordering named columns changes the returned theta). score_irt() must
# therefore reorder its data to items(mod_rec) before scoring. If it does not,
# each response vector is scored against a permuted item set, producing wrong
# (but plausible-looking) abilities.
#
# The fixture uses a 2PL model with varying discrimination: under a Rasch model
# the raw score is sufficient for EAP, so permuting columns of complete data
# would not change the score and could not detect the bug.

# build a small single-group 2PL model + ModelRecord, plus long-format trial
# data (in a deliberately shuffled item order) that score_irt() consumes
make_fixture <- function() {
  skip_if_not_installed("mirt")

  set.seed(123)
  n_persons <- 400
  item_uids <- paste0("item", 1:6)
  discrimination <- c(0.6, 1.8, 1.0, 2.2, 0.8, 1.5)
  difficulty <- c(-2, -1.5, -1, 0.5, 1.8, 2.5)
  theta <- rnorm(n_persons)
  probs <- plogis(sweep(outer(theta, difficulty, `-`), 2, discrimination, `*`))
  # mirt item columns are item_inst names: "<item_uid>-<instance>"
  resp <- matrix(rbinom(length(probs), 1, probs),
                 nrow = n_persons,
                 dimnames = list(NULL, paste0(item_uids, "-1")))

  mod <- mirt::mirt(resp, 1, itemtype = "2PL", verbose = FALSE)
  run_ids <- paste0("run", seq_len(n_persons))
  mod_rec <- modelrecord(mod, run_ids)

  # one row per run x item, as get_trials()/recode_trials() would produce.
  # items appear in a shuffled order, so the wide column order that
  # to_mirt_shape_grouped() produces differs from items(mod_rec).
  item_order <- c(3, 1, 5, 2, 6, 4)
  grid <- expand.grid(run = seq_len(n_persons), item = item_order)
  trials <- data.frame(
    run_id = run_ids[grid$run],
    site = "all",
    item_uid = item_uids[grid$item],
    correct = as.logical(resp[cbind(grid$run, grid$item)])
  )

  mod_spec <- list(item_task = "test", dataset = "all", model_set = "test",
                   subset = "all", itemtype = "2PL", nfact = "f1",
                   invariance = "scalar", redivis_source = "test:abcd:v1_0")

  list(mod_rec = mod_rec, trials = trials, mod_spec = mod_spec)
}

test_that("score_irt() reproduces the model's calibration EAP scores", {
  fx <- make_fixture()
  scored <- suppressMessages(score_irt(fx$trials, fx$mod_spec, fx$mod_rec))
  gold <- scores(fx$mod_rec)
  joined <- merge(scored, gold, by = "run_id")
  expect_equal(joined$score, joined$ability, tolerance = 0.05)
})

test_that("score_irt() is invariant to the item column order of its input", {
  fx <- make_fixture()
  trials_fwd <- fx$trials[order(fx$trials$item_uid), ]
  trials_rev <- fx$trials[order(fx$trials$item_uid, decreasing = TRUE), ]
  scored_fwd <- suppressMessages(score_irt(trials_fwd, fx$mod_spec, fx$mod_rec))
  scored_rev <- suppressMessages(score_irt(trials_rev, fx$mod_spec, fx$mod_rec))
  joined <- merge(scored_fwd, scored_rev, by = "run_id")
  expect_equal(joined$score.x, joined$score.y, tolerance = 1e-8)
})
