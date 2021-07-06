

# Fit CFAs for the sexes on Wordbank WS data
wbws_sex_fit <- wbws %>%
  filter(
    !is.na(sex)
  ) %>%
  group_by(sex) %>%
  nest() %>%
  run_cfas()

# Wordbank birth order (First/Later) fits
wbws_bo_fit <- wbws %>%
  filter(
    !is.na(birth_order)
  ) %>%
  group_by(birth_order) %>%
  nest() %>%
  run_cfas()

# Now do fits by age bin
wbws_age_fit <- wbws %>%
  group_by(age_bin) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  ) %>%
  run_cfas()

# This is the fit on all the data regardless of demo status
wbws_complete_fit <- wbws %>%
  nest(data = everything()) %>%
  run_cfas()

# Extract fit measures from Wordbank fits
# I separate these out so the data doesn't have to be dragged around everywhere
wbws_sex_stats <- extract_stats(wbws_sex_fit)
wbws_bo_stats <- extract_stats(wbws_bo_fit)
wbws_age_stats <- extract_stats(wbws_age_fit)

wbws_all_stats <- wbws_age_stats %>%
  ungroup %>%
  mutate(
    across(contains("_fit_"), as.numeric)
  ) %>%
  add_row() %>%
  bind_rows(
    # Do the same this as the external pipeline, strip the annoying
    # lavaan.vector class from columns
    mutate(wbws_sex_stats, across(contains("fit"), as.numeric)),
  ) %>%
  add_row() %>%
  bind_rows(
    # Do the same this as the external pipeline, strip the annoying
    # lavaan.vector class from columns
    mutate(wbws_bo_stats, across(contains("fit"), as.numeric))
  ) %>%
  select(age_bin, sex, birth_order, n, everything())

write_csv(wbws_all_stats, paste0(output_dir, "/wbws_all_stats.csv"))


wbws_complete_stats <- extract_stats(wbws_complete_fit)
write_csv(wbws_complete_stats, paste0(output_dir, "/wbws_complete_stats.csv"))

# Store the fit stats in the same df
wbws_sexbo_fit <- bind_rows(wbws_sex_fit, wbws_bo_fit) %>%
  select(sex, birth_order, n, data, everything()) %>%
  pivot_longer(-c(sex, birth_order, n, data), values_to = "lavaan") %>%
  mutate(

    loadings = map(lavaan, ~lavInspect(.x, "std")$lambda %>%
                              as.data.frame() %>%
                              rownames_to_column() ),

    modindices = map(lavaan, ~lavInspect(.x, "mi") %>%
                                filter(mi >= 10) %>%
                                arrange(desc(mi)) )
  )

# All loadings
wbws_sexbo_loadings <- wbws_sexbo_fit %>%
  select(-data, -lavaan) %>%
  unnest(loadings)

# All modification indices
wbws_sexbo_mi <- wbws_sexbo_fit %>%
  select(-data, -lavaan, -loadings) %>%
  unnest(modindices) %>%
  select(-op)

# write_csv(wbws_sexbo_loadings, "wbws_sexbo_loadings.csv")
# write_csv(wbws_sexbo_mi, "cfa1/wbws_sexbo_mi.csv")


modindices(wbws_all_fit$syn_cfa[[1]], sort. = TRUE, minimum.value = 10)

summary(wbws_all_fit$lex_cfa[[1]], fit.measures = TRUE, standardized = TRUE)
summary(wbws_all_fit$syn_cfa[[1]], fit.measures = TRUE, standardized = TRUE)

###############################################################################

eirli_dx_fit <- eirli %>%
  group_by(age_bin, dx) %>%
  nest() %>%
  arrange(dx, age_bin) %>%
  mutate(
    n = map_int(data, nrow)
  ) %>%
  run_cfas()

eirli_sex_fit <- eirli %>%
  group_by(age_bin, gender) %>%
  nest() %>%
  arrange(gender, age_bin) %>%
  mutate(
    n = map_int(data, nrow)
  ) %>%
  run_cfas()

eirli_sex_stats <- extract_stats(eirli_sex_fit)
eirli_dx_stats <- extract_stats(eirli_dx_fit)

# Join EIRLI into one df for export (this is different and better than WBWS but
# I haven't gone back and improved WBWS yet)
eirli_all_stats <- eirli_sex_stats %>%
  mutate(
    across(contains("_fit_"), as.numeric)
  ) %>%
  add_row() %>%
  bind_rows(
    # Do the same this as the external pipeline, strip the annoying
    # lavaan.vector class from columns
    mutate(eirli_dx_stats, across(contains("fit"), as.numeric))
  )


write_csv(eirli_all_stats, paste0(output_dir, "/eirli_all_stats.csv"))
