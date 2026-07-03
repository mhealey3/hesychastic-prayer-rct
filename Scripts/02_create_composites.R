install.packages("psych")

library(haven)
library(dplyr)
library(tidyr)
library(psych)
library(here)
library(glue)
library(stringr)
library(writexl)

##load cleaned data
data <- readRDS(here("Outputs", "1_cleaned_hp_dataR.rds"))

############CREATING COMPOSITE SCORES##################
# Each composite and subscore calculated and placed after relevant variable
# Also added to analysis_data that contains just composites and subscores


# NOTE: items are re-scored according to scale instructions when needed (i.e. reverse coded)
# General reverse scoring function. works for any scale
reverse_score <- function(x, min = 1, max) min + max - x


# ── Beauty (Diessner et al. 2008) ────────────────────────────────────────────
# Source: Diessner, R., Solom, R. C., Frost, N. K., Parsons, L., & Davidson, J. (2008). https://doi.org/10.3200/JRLP.142.3.303-332
# Scoring format: 14 items, 3 subscales, no reverse scoring needed. Summed.
# Subscales: Natural Beauty: items 1-4; Artistic Beauty: items 5-8; Moral Beauty: items 9-14
# Response type: All items scored 1-7 (Very unlike me to Very much like me)

data <- data %>%
  mutate(
    BEAUTY_Natural_T1  = rowSums(select(., BEAUTY_1_T1:BEAUTY_4_T1),  na.rm = FALSE),
    BEAUTY_Artistic_T1 = rowSums(select(., BEAUTY_5_T1:BEAUTY_8_T1),  na.rm = FALSE),
    BEAUTY_Moral_T1    = rowSums(select(., BEAUTY_9_T1:BEAUTY_14_T1), na.rm = FALSE),
    BEAUTY_Total_T1    = rowSums(select(., BEAUTY_1_T1:BEAUTY_14_T1), na.rm = FALSE),
    .after = BEAUTY_14_T1
  )

data <- data %>%
  mutate(
    BEAUTY_Natural_T2  = rowSums(select(., BEAUTY_1_T2:BEAUTY_4_T2),  na.rm = FALSE),
    BEAUTY_Artistic_T2 = rowSums(select(., BEAUTY_5_T2:BEAUTY_8_T2),  na.rm = FALSE),
    BEAUTY_Moral_T2    = rowSums(select(., BEAUTY_9_T2:BEAUTY_14_T2), na.rm = FALSE),
    BEAUTY_Total_T2    = rowSums(select(., BEAUTY_1_T2:BEAUTY_14_T2), na.rm = FALSE),
    .after = BEAUTY_14_T2
  )

# ── Flourishing (VanderWeele, 2017) ──────────────────────────────────────────
# Source: VanderWeele, T. J. (2017). https://doi.org/10.1073/pnas.1702996114
# Scoring format: 12 items, 6 domains, no reverse scoring needed. Summed.
# Subscales: HLS (items 1-2), MHPH (3-4), MP (5-6), CV (7-8), CSR (9-10), FMS (11-12); each domain range 0-20
# Response type: All items scored 0-10 (FMS pre-keyed: 0 = Worry all the time, 10 = Do not ever worry)

data <- data %>%
  mutate(
    FLOURISH_HLS_T1  = rowSums(select(., FLOURISH_HLS_1_T1, FLOURISH_HLS_2_T1), na.rm = FALSE),
    FLOURISH_MHPH_T1 = rowSums(select(., FL_MHPH_1_T1, FL_MHPH_2_T1),           na.rm = FALSE),
    FLOURISH_MP_T1   = rowSums(select(., FL_MP_1_T1, FL_MP_2_T1),               na.rm = FALSE),
    FLOURISH_CV_T1   = rowSums(select(., FL_CV_1_T1, FL_CV_2_T1),               na.rm = FALSE),
    FLOURISH_CSR_T1  = rowSums(select(., FL_CSR_1_T1, FL_CSR_2_T1),             na.rm = FALSE),
    FLOURISH_FMS_T1  = rowSums(select(., FL_FMS_1_T1, FL_FMS_2_T1),             na.rm = FALSE),
    .after = FL_FMS_2_T1
  )

data <- data %>%
  mutate(
    FLOURISH_TOTAL_T1 = rowSums(select(., FLOURISH_HLS_1_T1, FLOURISH_HLS_2_T1,
                                              FL_MHPH_1_T1, FL_MHPH_2_T1,
                                              FL_MP_1_T1, FL_MP_2_T1,
                                              FL_CV_1_T1, FL_CV_2_T1,
                                              FL_CSR_1_T1, FL_CSR_2_T1),
                                       na.rm = FALSE),
    FLOURISH_SECURE_TOTAL_T1 = rowSums(select(., FLOURISH_HLS_1_T1, FLOURISH_HLS_2_T1,
                                              FL_MHPH_1_T1, FL_MHPH_2_T1,
                                              FL_MP_1_T1, FL_MP_2_T1,
                                              FL_CV_1_T1, FL_CV_2_T1,
                                              FL_CSR_1_T1, FL_CSR_2_T1,
                                              FL_FMS_1_T1, FL_FMS_2_T1),
                                       na.rm = FALSE),
    .after = FLOURISH_FMS_T1
  )

data <- data %>%
  mutate(
    FLOURISH_HLS_T2  = rowSums(select(., FLOURISH_HLS_1_T2, FLOURISH_HLS_2_T2), na.rm = FALSE),
    FLOURISH_MHPH_T2 = rowSums(select(., FL_MHPH_1_T2, FL_MHPH_2_T2),           na.rm = FALSE),
    FLOURISH_MP_T2   = rowSums(select(., FL_MP_1_T2, FL_MP_2_T2),               na.rm = FALSE),
    FLOURISH_CV_T2   = rowSums(select(., FL_CV_1_T2, FL_CV_2_T2),               na.rm = FALSE),
    FLOURISH_CSR_T2  = rowSums(select(., FL_CSR_1_T2, FL_CSR_2_T2),             na.rm = FALSE),
    FLOURISH_FMS_T2  = rowSums(select(., FL_FMS_1_T2, FL_FMS_2_T2),             na.rm = FALSE),
    .after = FL_FMS_2_T2
  )

data <- data %>%
  mutate(
    FLOURISH_TOTAL_T2 = rowSums(select(., FLOURISH_HLS_1_T2, FLOURISH_HLS_2_T2,
                                              FL_MHPH_1_T2, FL_MHPH_2_T2,
                                              FL_MP_1_T2, FL_MP_2_T2,
                                              FL_CV_1_T2, FL_CV_2_T2,
                                              FL_CSR_1_T2, FL_CSR_2_T2),
                                       na.rm = FALSE),
    FLOURISH_SECURE_TOTAL_T2 = rowSums(select(., FLOURISH_HLS_1_T2, FLOURISH_HLS_2_T2,
                                              FL_MHPH_1_T2, FL_MHPH_2_T2,
                                              FL_MP_1_T2, FL_MP_2_T2,
                                              FL_CV_1_T2, FL_CV_2_T2,
                                              FL_CSR_1_T2, FL_CSR_2_T2,
                                              FL_FMS_1_T2, FL_FMS_2_T2),
                                       na.rm = FALSE),
    .after = FLOURISH_FMS_T2
  )



# ── Anxiety — GAD-7 (Spitzer et al., 2006) ───────────────────────────────────
# Source: Spitzer, R. L., Kroenke, K., Williams, J. B. W., & Löwe, B. (2006). https://doi.org/10.1001/archinte.166.10.1092
# Scoring format: 7 items, no subscales, no reverse scoring needed. Summed (range 0-21).
# Severity bands: 0-4 minimal, 5-9 mild, 10-14 moderate, 15-21 severe
# Response type: 0 (Not at all) / 1 (Several days) / 2 (More than half the days) / 3 (Nearly every day)

data <- data %>%
  mutate(
    ANXIETY_T1 = rowSums(select(., ANX_1_T1:ANX_7_T1), na.rm = FALSE),
    .after = GAD2_T1
  )

data <- data %>%
  mutate(
    ANXIETY_T2 = rowSums(select(., ANX_1_T2:ANX_7_T2), na.rm = FALSE),
    .after = GAD2_T2
  )


# ── Depression — BDI 13-item Short Form (Beck & Beck, 1972; BDI-II hybrid) ───
# Source: Beck, A. T., & Beck, R. W. (1972). https://doi.org/10.1080/00325481.1972.11713319
# Secondary: Beck, A. T., Steer, R. A., & Brown, G. K. (1996). BDI-II Manual. (Wording for items 3, 5, 8, 9; two-week timeframe)
# Scoring format: 13 items, no subscales, no reverse scoring needed. Summed (range 0-39).
# Severity bands: 0-4 none/minimal, 5-7 mild, 8-15 moderate, 16+ severe
# Response type: 0-3 per item (4 graded self-evaluative statements; "past two weeks" timeframe)
# Note: 13-item item set per Beck & Beck (1972); items 3, 5, 8, 9 use BDI-II wording (hybrid).

data <- data %>%
  mutate(
    DEPRESSION_T1 = rowSums(select(., BDI_1_T1:BDI_13_T1), na.rm = FALSE),
    .after = BDI_13_T1
  )

data <- data %>%
  mutate(
    DEPRESSION_T2 = rowSums(select(., BDI_1_T2:BDI_13_T2), na.rm = FALSE),
    .after = BDI_13_T2
  )


# ── Sleep — Disturbances Composite (PSQI items 5b-5j; Buysse et al., 1989) ───
# Source: Buysse, D. J., Reynolds, C. F., Monk, T. H., Berman, S. R., & Kupfer, D. J. (1989). https://doi.org/10.1016/0165-1781(89)90047-4
# Scoring format: 9 items (Sleep5_2 to Sleep5_10), no reverse scoring. Mean (range 0-3).
# Note: Administered items coded 1-4 in Qualtrics; recoded to PSQI standard 0-3 via (item - 1) before averaging.
# Note: Full PSQI global score not computed (items 6-9 not administered). Sleep duration (Sleep4) was free-text and excluded.

# ── Sleep — Disturbances Composite (PSQI items 5b-5j; Buysse et al., 1989) ───
# Source: Buysse, D. J., Reynolds, C. F., Monk, T. H., Berman, S. R., & Kupfer, D. J. (1989). https://doi.org/10.1016/0165-1781(89)90047-4
# Scoring format: 9 items (Sleep5_2 to Sleep5_10), no reverse scoring. Mean (range 0-3).
# Note: Full PSQI global score not computed (items 6-9 not administered). Sleep duration (Sleep4) was free-text and excluded.
# Note: Item recoding from administered 1-4 to PSQI standard 0-3 happens in 01_cleaning.R.

data <- data %>%
  mutate(
    SLEEP_Disturbances_T1 = rowMeans(select(., Sleep5_2_T1:Sleep5_10_T1), na.rm = FALSE),
    .after = Sleep5_10_T1
  )

data <- data %>%
  mutate(
    SLEEP_Disturbances_T2 = rowMeans(select(., Sleep5_2_T2:Sleep5_10_T2), na.rm = FALSE),
    .after = Sleep5_10_T2
  )

# ── Substance Use ─────────────────────────────────────────────────────────────
# No cleaning/scoring needed.

# ── Mystical Experience — Hood's 32-item M-Scale (Hood, 1975) ────────────────
# Source: Hood, R. W., Jr. (1975). https://doi.org/10.2307/1384454
# Three-factor source: Hood, R. W., Morris, R. J., & Watson, P. J. (1993). https://doi.org/10.2466/pr0.1993.73.3f.1176
# Verification source: Streib, Klein, Keller, & Hood (2020).
# Scoring format: 32 items; 16 reverse-scored via * -1 (symmetric -2 to +2 metric)
# Reverse items: 2, 6, 7, 8, 9, 10, 14, 15, 16, 21, 24, 25, 26, 27, 28, 30
# Two-factor structure (Hood, 1975):
#   F1 General Mysticism (20):       1, 2, 3, 4, 6, 8, 10, 11, 12, 15, 19, 21, 23, 24, 27, 28, 29, 30, 31, 32
#   F2 Religious Interpretation (12): 5, 7, 9, 13, 14, 16, 17, 18, 20, 22, 25, 26
# Three-factor structure (Hood, Morris, & Watson, 1993):
#   Introvertive (12):   1, 2, 3, 4, 6, 11, 15, 21, 23, 24, 27, 32
#   Extrovertive (8):    8, 10, 12, 19, 28, 29, 30, 31
#   Interpretation (12): 5, 7, 9, 13, 14, 16, 17, 18, 20, 22, 25, 26
# Response type: -2 (definitely not true) to +2 (definitely true), symmetric

# Reverse-score 16 items via * -1 (symmetric -2 to +2 metric)
meq_reverse_cols <- c(
  paste0("MEQ_", c(2, 6, 7, 8, 9, 10, 14, 15, 16, 21, 24, 25, 26, 27, 28, 30), "_T1"),
  paste0("MEQ_", c(2, 6, 7, 8, 9, 10, 14, 15, 16, 21, 24, 25, 26, 27, 28, 30), "_T2")
)
data[meq_reverse_cols] <- lapply(data[meq_reverse_cols], function(x) as.numeric(x) * -1) 

# Two-factor scoring (Hood, 1975)
data <- data %>%
  mutate(
    MEQ_F1_T1    = rowMeans(select(., paste0("MEQ_", c(1,2,3,4,6,8,10,11,12,15,19,21,23,24,27,28,29,30,31,32), "_T1")), na.rm = FALSE),
    MEQ_F2_T1    = rowMeans(select(., paste0("MEQ_", c(5,7,9,13,14,16,17,18,20,22,25,26), "_T1")),                      na.rm = FALSE),
    MEQ_TOTAL_T1 = rowMeans(select(., paste0("MEQ_", 1:32, "_T1")),                                                     na.rm = FALSE),
    .after = MEQ_32_T1
  )

data <- data %>%
  mutate(
    MEQ_F1_T2    = rowMeans(select(., paste0("MEQ_", c(1,2,3,4,6,8,10,11,12,15,19,21,23,24,27,28,29,30,31,32), "_T2")), na.rm = FALSE),
    MEQ_F2_T2    = rowMeans(select(., paste0("MEQ_", c(5,7,9,13,14,16,17,18,20,22,25,26), "_T2")),                      na.rm = FALSE),
    MEQ_TOTAL_T2 = rowMeans(select(., paste0("MEQ_", 1:32, "_T2")),                                                     na.rm = FALSE),
    .after = MEQ_32_T2
  )

# Three-factor scoring (Hood, Morris, & Watson, 1993)
data <- data %>%
  mutate(
    MEQ_Introvertive_T1   = rowMeans(select(., paste0("MEQ_", c(1,2,3,4,6,11,15,21,23,24,27,32), "_T1")),       na.rm = FALSE),
    MEQ_Extrovertive_T1   = rowMeans(select(., paste0("MEQ_", c(8,10,12,19,28,29,30,31), "_T1")),                na.rm = FALSE),
    MEQ_Interpretation_T1 = rowMeans(select(., paste0("MEQ_", c(5,7,9,13,14,16,17,18,20,22,25,26), "_T1")),     na.rm = FALSE),
    .after = MEQ_TOTAL_T1
  )

data <- data %>%
  mutate(
    MEQ_Introvertive_T2   = rowMeans(select(., paste0("MEQ_", c(1,2,3,4,6,11,15,21,23,24,27,32), "_T2")),       na.rm = FALSE),
    MEQ_Extrovertive_T2   = rowMeans(select(., paste0("MEQ_", c(8,10,12,19,28,29,30,31), "_T2")),                na.rm = FALSE),
    MEQ_Interpretation_T2 = rowMeans(select(., paste0("MEQ_", c(5,7,9,13,14,16,17,18,20,22,25,26), "_T2")),     na.rm = FALSE),
    .after = MEQ_TOTAL_T2
  )
   
# ── MAIA-2 (Mehling et al., 2018; partial administration) ────────────────────
# Source: Mehling, W. E., Acree, M., Stewart, A., Silas, J., & Jones, A. (2018). https://doi.org/10.1371/journal.pone.0208034
# Scoring format: 2 of 8 MAIA-2 subscales administered (7 items total). No reverse scoring. Subscale means.
# Subscales: Self-Regulation (items 1-4), Body Listening (items 5-7)
# Response type: administered 1-5 (Strongly disagree to Strongly agree); published MAIA-2 uses 0-5 frequency (Never to Always)

data <- data %>%
  mutate(
    MAIA_SELFREG_T1    = rowMeans(select(., MAIA_2_1_T1:MAIA_2_4_T1), na.rm = FALSE),
    MAIA_BODYLISTEN_T1 = rowMeans(select(., MAIA_2_5_T1:MAIA_2_7_T1), na.rm = FALSE),
    .after = MAIA_2_7_T1
  )

data <- data %>%
  mutate(
    MAIA_SELFREG_T2    = rowMeans(select(., MAIA_2_1_T2:MAIA_2_4_T2), na.rm = FALSE),
    MAIA_BODYLISTEN_T2 = rowMeans(select(., MAIA_2_5_T2:MAIA_2_7_T2), na.rm = FALSE),
    .after = MAIA_2_7_T2
  )

# ── Self-Compassion Short Form — SCS-SF (Raes et al., 2011) ──────────────────
# Source: Raes, F., Pommier, E., Neff, K. D., & Van Gucht, D. (2011). https://doi.org/10.1002/cpp.702
# Scoring format: 12 items, 6 two-item subscales; reverse score items 1, 4, 8, 9, 11, 12 (6 - x); subscales = item means.
# Subscales (scale 1-5):
#   Self-Kindness (SK):        items 2, 6
#   Self-Judgment (SJ):        items 11, 12  — reverse scored
#   Common Humanity (CH):      items 5, 10
#   Isolation (I):             items 4, 8    — reverse scored
#   Mindfulness (M):           items 3, 7
#   Over-Identification (OI):  items 1, 9    — reverse scored
# Total: mean of 6 subscale means
# Response type: 1-5 (Almost never to Almost always)

scs_reverse_t1 <- paste0("SCOMPASS_", c(1, 4, 8, 9, 11, 12), "_T1")
scs_reverse_t2 <- paste0("SCOMPASS_", c(1, 4, 8, 9, 11, 12), "_T2")

scs_all_reverse <- c(scs_reverse_t1, scs_reverse_t2)
saved_labels_scs <- lapply(data[scs_all_reverse], attr, "label")

data <- data %>%
  mutate(across(all_of(scs_all_reverse), ~ reverse_score(., max = 5)))

for (col in scs_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_scs[[col]], " [REVERSE SCORED]")
}

data <- data %>%
  mutate(
    SCS_SK_T1    = rowMeans(cbind(SCOMPASS_2_T1,  SCOMPASS_6_T1),  na.rm = FALSE),
    SCS_SJ_T1    = rowMeans(cbind(SCOMPASS_11_T1, SCOMPASS_12_T1), na.rm = FALSE),
    SCS_CH_T1    = rowMeans(cbind(SCOMPASS_5_T1,  SCOMPASS_10_T1), na.rm = FALSE),
    SCS_I_T1     = rowMeans(cbind(SCOMPASS_4_T1,  SCOMPASS_8_T1),  na.rm = FALSE),
    SCS_M_T1     = rowMeans(cbind(SCOMPASS_3_T1,  SCOMPASS_7_T1),  na.rm = FALSE),
    SCS_OI_T1    = rowMeans(cbind(SCOMPASS_1_T1,  SCOMPASS_9_T1),  na.rm = FALSE),
    SCS_TOTAL_T1 = (SCS_SK_T1 + SCS_SJ_T1 + SCS_CH_T1 +
                      SCS_I_T1  + SCS_M_T1  + SCS_OI_T1) / 6,
    .after = SCOMPASS_12_T1
  )

data <- data %>%
  mutate(
    SCS_SK_T2    = rowMeans(cbind(SCOMPASS_2_T2,  SCOMPASS_6_T2),  na.rm = FALSE),
    SCS_SJ_T2    = rowMeans(cbind(SCOMPASS_11_T2, SCOMPASS_12_T2), na.rm = FALSE),
    SCS_CH_T2    = rowMeans(cbind(SCOMPASS_5_T2,  SCOMPASS_10_T2), na.rm = FALSE),
    SCS_I_T2     = rowMeans(cbind(SCOMPASS_4_T2,  SCOMPASS_8_T2),  na.rm = FALSE),
    SCS_M_T2     = rowMeans(cbind(SCOMPASS_3_T2,  SCOMPASS_7_T2),  na.rm = FALSE),
    SCS_OI_T2    = rowMeans(cbind(SCOMPASS_1_T2,  SCOMPASS_9_T2),  na.rm = FALSE),
    SCS_TOTAL_T2 = (SCS_SK_T2 + SCS_SJ_T2 + SCS_CH_T2 +
                      SCS_I_T2  + SCS_M_T2  + SCS_OI_T2) / 6,
    .after = SCOMPASS_12_T2
  )

# ── Forgiveness — HFS (Thompson et al., 2005) ────────────────────────────────
# Source: Thompson, L. Y., et al. (2005). https://doi.org/10.1111/j.1467-6494.2005.00311.x
# Scoring format: 18 items, reverse score items 2, 4, 6, 7, 9, 11, 13, 15, 17 (8 - x); subscale sums (range 6-42), total = sum of 3 subscales.
# Subscales: Self (1-6), Other (7-12), Situation (13-18)
# Response type: 1-7 (Almost always false to Almost always true)

hfs_reverse_t1 <- paste0("Forgiveness_", c(2, 4, 6, 7, 9, 11, 13, 15, 17), "_T1")
hfs_reverse_t2 <- paste0("Forgiveness_", c(2, 4, 6, 7, 9, 11, 13, 15, 17), "_T2")

hfs_all_reverse <- c(hfs_reverse_t1, hfs_reverse_t2)
saved_labels_hfs <- lapply(data[hfs_all_reverse], attr, "label")

data <- data %>%
  mutate(across(all_of(hfs_all_reverse), ~ reverse_score(., max = 7)))

for (col in hfs_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_hfs[[col]], " [REVERSE SCORED]")
}

data <- data %>%
  mutate(
    Forgive_Self_T1  = rowSums(select(., paste0("Forgiveness_", 1:6,   "_T1")), na.rm = FALSE),
    Forgive_Other_T1 = rowSums(select(., paste0("Forgiveness_", 7:12,  "_T1")), na.rm = FALSE),
    Forgive_Sit_T1   = rowSums(select(., paste0("Forgiveness_", 13:18, "_T1")), na.rm = FALSE),
    Forgive_Total_T1 = Forgive_Self_T1 + Forgive_Other_T1 + Forgive_Sit_T1,
    .after = Forgiveness_18_T1
  )

data <- data %>%
  mutate(
    Forgive_Self_T2  = rowSums(select(., paste0("Forgiveness_", 1:6,   "_T2")), na.rm = FALSE),
    Forgive_Other_T2 = rowSums(select(., paste0("Forgiveness_", 7:12,  "_T2")), na.rm = FALSE),
    Forgive_Sit_T2   = rowSums(select(., paste0("Forgiveness_", 13:18, "_T2")), na.rm = FALSE),
    Forgive_Total_T2 = Forgive_Self_T2 + Forgive_Other_T2 + Forgive_Sit_T2,
    .after = Forgiveness_18_T2
  )


# ── Basic Empathy Scale, Adult version — BES-A (Carré et al., 2013) ───────────
# Source: Carré, A., Stefaniak, N., D'Ambrosio, F., Bensalah, L., & Besche-Richard, C. (2013). https://doi.org/10.1037/a0032297
# Scoring format: 20-item adult version; item 4 dropped (weak loadings, both models) -> 19 items scored.
#                 Reverse-score items 1, 6, 7, 8, 13, 18, 19, 20 (6 - x); subscales and total = item means.
# Two-factor:   affective (10 items), cognitive (9 items)
# Three-factor: contagion (5), cognitive empathy (8), disconnection (6)
# Total: mean of all 19 retained items (item 4 excluded)
# Response type: 1-5 (Strongly disagree to Strongly agree)
# Note: 8 reverse items incl. item 6; Carré's "seven reversed items" in the text is a typo,
#       contradicted by item 6's positive loading in Table B1 / Figure 2.

bes_reverse_t1 <- paste0("Empathy_", c(1, 6, 7, 8, 13, 18, 19, 20), "_T1")
bes_reverse_t2 <- paste0("Empathy_", c(1, 6, 7, 8, 13, 18, 19, 20), "_T2")

bes_all_reverse <- c(bes_reverse_t1, bes_reverse_t2)
saved_labels_bes <- lapply(data[bes_all_reverse], attr, "label")

data <- data %>%
  mutate(across(all_of(bes_all_reverse), ~ reverse_score(., max = 5)))

for (col in bes_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_bes[[col]], " [REVERSE SCORED]")
}

data <- data %>%
  mutate(
    BES_affective_T1 = rowMeans(select(., paste0("Empathy_", c(1, 2, 5, 7, 8, 11, 13, 15, 17, 18), "_T1")), na.rm = FALSE),
    BES_cognitive_T1 = rowMeans(select(., paste0("Empathy_", c(3, 6, 9, 10, 12, 14, 16, 19, 20), "_T1")),   na.rm = FALSE),
    BES_total_T1     = rowMeans(select(., paste0("Empathy_", c(1:3, 5:20), "_T1")),                          na.rm = FALSE),
    BES_cont_T1      = rowMeans(select(., paste0("Empathy_", c(2, 5, 11, 15, 17), "_T1")),                   na.rm = FALSE),
    BES_emp_T1       = rowMeans(select(., paste0("Empathy_", c(3, 6, 9, 10, 12, 14, 16, 20), "_T1")),        na.rm = FALSE),
    BES_dis_T1       = rowMeans(select(., paste0("Empathy_", c(1, 7, 8, 13, 18, 19), "_T1")),                na.rm = FALSE),
    .after = Empathy_20_T1
  )

data <- data %>%
  mutate(
    BES_affective_T2 = rowMeans(select(., paste0("Empathy_", c(1, 2, 5, 7, 8, 11, 13, 15, 17, 18), "_T2")), na.rm = FALSE),
    BES_cognitive_T2 = rowMeans(select(., paste0("Empathy_", c(3, 6, 9, 10, 12, 14, 16, 19, 20), "_T2")),   na.rm = FALSE),
    BES_total_T2     = rowMeans(select(., paste0("Empathy_", c(1:3, 5:20), "_T2")),                          na.rm = FALSE),
    BES_cont_T2      = rowMeans(select(., paste0("Empathy_", c(2, 5, 11, 15, 17), "_T2")),                   na.rm = FALSE),
    BES_emp_T2       = rowMeans(select(., paste0("Empathy_", c(3, 6, 9, 10, 12, 14, 16, 20), "_T2")),        na.rm = FALSE),
    BES_dis_T2       = rowMeans(select(., paste0("Empathy_", c(1, 7, 8, 13, 18, 19), "_T2")),                na.rm = FALSE),
    .after = Empathy_20_T2
  )


# ── UCLA 3-Item Loneliness Scale (Hughes et al., 2004) ────────────────────────
# Source: Hughes, M. E., Waite, L. J., Hawkley, L. C., & Cacioppo, J. T. (2004). https://doi.org/10.1177/0164027504268574
# 3 items measuring relational connectedness, social connectedness, self-perceived isolation
# Scoring format: sum of all 3 items (range 3-9), no reverse scoring. 3-5 = not lonely, 6-9 = lonely.
# Response type: 1-3 (administered: Never / Some of the time / Often; published anchor 1 = "Hardly ever")
# Note: short scale derived from the original UCLA Loneliness Scale (Russell, Peplau & Ferguson, 1978).

data <- data %>%
  mutate(
    UCLA_LON_T1 = rowSums(select(., UCLA_LON_1_T1:UCLA_LON_3_T1), na.rm = FALSE),
    .after = UCLA_LON_3_T1
  )

data <- data %>%
  mutate(
    UCLA_LON_T2 = rowSums(select(., UCLA_LON_1_T2:UCLA_LON_3_T2), na.rm = FALSE),
    .after = UCLA_LON_3_T2
  )


# ── Connectedness to Nature Scale (Mayer & Frantz, 2004) ──
# Source: Mayer, F. S., & Frantz, C. M. (2004). The connectedness to nature scale:
#   A measure of individuals' feeling in community with nature.
#   Journal of Environmental Psychology, 24(4), 503–515. https://doi.org/10.1016/j.jenvp.2004.10.001
# Scoring format: mean of all 14 items (range 1–5); higher = greater connectedness
# Subscales: none (single factor)
# Response type: 1 = Strongly disagree … 5 = Strongly agree ("how you generally feel")
# Reverse-scored: items 4, 12, 14  

natcon_reverse_t1 <- c("NatCon_4_T1", "NatCon_12_T1", "NatCon_14_T1")
natcon_reverse_t2 <- c("NatCon_4_T2", "NatCon_12_T2", "NatCon_14_T2")
natcon_all_reverse <- c(natcon_reverse_t1, natcon_reverse_t2)

saved_labels_natcon <- lapply(data[natcon_all_reverse], attr, "label")

data <- data %>%
  mutate(across(all_of(natcon_all_reverse), ~ reverse_score(., max = 5)))

for (col in natcon_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_natcon[[col]], " [REVERSE SCORED]")
}

data <- data %>%
  mutate(
    NatCon_T1 = rowMeans(select(., paste0("NatCon_", 1:14, "_T1")), na.rm = FALSE),
    .after = NatCon_14_T1
  )

data <- data %>%
  mutate(
    NatCon_T2 = rowMeans(select(., paste0("NatCon_", 1:14, "_T2")), na.rm = FALSE),
    .after = NatCon_14_T2
  )


# ── De Jong Gierveld Loneliness Scale, 11-item (de Jong Gierveld & Kamphuis, 1985) ──
# Source: de Jong Gierveld, J., & Kamphuis, F. (1985). The development of a Rasch-type
#   loneliness scale. Applied Psychological Measurement, 9(3), 289–299.
#   https://doi.org/10.1177/014662168500900307
#   Dichotomous cut-off scoring: van Tilburg & de Jong Gierveld (1999), Manual of the Loneliness Scale.
# Scoring format: each item dichotomized to lonely (1) / not (0), then summed
#   Emotional (neg. worded) items 2,3,5,6,9,10: lonely if response <= 3 (Yes!, Yes, More or less)
#   Social   (pos. worded) items 1,4,7,8,11:    lonely if response >= 3 (More or less, No, No!)
#   "More or less" (neutral) counts as lonely on both subscales (manual rule).
# Subscales: Emotional loneliness (0–6), Social loneliness (0–5); Total (0–11)
# Response type: 1 = Yes! … 3 = More or less … 5 = No!

emotional_items <- c(2, 3, 5, 6, 9, 10)
social_items    <- c(1, 4, 7, 8, 11)

# T1 dichotomize
data <- data %>%
  mutate(
    across(all_of(paste0("DGLon_", emotional_items, "_T1")), ~ as.integer(. <= 3), .names = "{.col}_d"),
    across(all_of(paste0("DGLon_", social_items,    "_T1")), ~ as.integer(. >= 3), .names = "{.col}_d")
  )

# T1 composites
data <- data %>%
  mutate(
    DGLon_Emotional_T1 = rowSums(select(., paste0("DGLon_", emotional_items, "_T1_d")), na.rm = FALSE),
    DGLon_Social_T1    = rowSums(select(., paste0("DGLon_", social_items,    "_T1_d")), na.rm = FALSE),
    DGLon_Total_T1     = DGLon_Emotional_T1 + DGLon_Social_T1,
    .after = DGLon_11_T1
  )

# T2 dichotomize
data <- data %>%
  mutate(
    across(all_of(paste0("DGLon_", emotional_items, "_T2")), ~ as.integer(. <= 3), .names = "{.col}_d"),
    across(all_of(paste0("DGLon_", social_items,    "_T2")), ~ as.integer(. >= 3), .names = "{.col}_d")
  )

# T2 composites
data <- data %>%
  mutate(
    DGLon_Emotional_T2 = rowSums(select(., paste0("DGLon_", emotional_items, "_T2_d")), na.rm = FALSE),
    DGLon_Social_T2    = rowSums(select(., paste0("DGLon_", social_items,    "_T2_d")), na.rm = FALSE),
    DGLon_Total_T2     = DGLon_Emotional_T2 + DGLon_Social_T2,
    .after = DGLon_11_T2
  )

# Drop intermediate dichotomized columns
data <- data %>%
  select(-ends_with("_d"))


# ── BFI-10 (Rammstedt & John, 2007) ──────────────────────────────────────────
# Source: Rammstedt, B., & John, O. P. (2007). Measuring personality in one minute
#   or less: A 10-item short version of the Big Five Inventory in English and German.
#   Journal of Research in Personality, 41(1), 203-212. doi:10.1016/j.jrp.2006.02.001
# Scoring format: 10 items, 1-5; five two-item trait MEANS; reverse items 1,3,4,5,7
# Subscales: Extraversion (1R,6); Agreeableness (2,7R); Conscientiousness (3R,8);
#            Neuroticism (4R,9); Openness (5R,10)
# Response type: 1 (Disagree strongly) to 5 (Agree strongly)

bfi_reverse_t1 <- paste0("BFI_10_", c(1, 3, 4, 5, 7), "_T1")
bfi_reverse_t2 <- paste0("BFI_10_", c(1, 3, 4, 5, 7), "_T2")

bfi_all_reverse <- c(bfi_reverse_t1, bfi_reverse_t2)
saved_labels_bfi <- lapply(data[bfi_all_reverse], attr, "label")

data <- data %>%
  mutate(across(all_of(bfi_all_reverse), ~ reverse_score(., max = 5)))

for (col in bfi_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_bfi[[col]], " [REVERSE SCORED]")
}

data <- data %>%
  mutate(
    BFI_Extraversion_T1     = rowMeans(cbind(BFI_10_1_T1, BFI_10_6_T1),  na.rm = FALSE),
    BFI_Agreeableness_T1    = rowMeans(cbind(BFI_10_2_T1, BFI_10_7_T1),  na.rm = FALSE),
    BFI_Conscientiousness_T1 = rowMeans(cbind(BFI_10_3_T1, BFI_10_8_T1), na.rm = FALSE),
    BFI_Neuroticism_T1      = rowMeans(cbind(BFI_10_4_T1, BFI_10_9_T1),  na.rm = FALSE),
    BFI_Openness_T1         = rowMeans(cbind(BFI_10_5_T1, BFI_10_10_T1), na.rm = FALSE),
    .after = BFI_10_10_T1
  )

data <- data %>%
  mutate(
    BFI_Extraversion_T2      = rowMeans(cbind(BFI_10_1_T2, BFI_10_6_T2),  na.rm = FALSE),
    BFI_Agreeableness_T2     = rowMeans(cbind(BFI_10_2_T2, BFI_10_7_T2),  na.rm = FALSE),
    BFI_Conscientiousness_T2 = rowMeans(cbind(BFI_10_3_T2, BFI_10_8_T2),  na.rm = FALSE),
    BFI_Neuroticism_T2       = rowMeans(cbind(BFI_10_4_T2, BFI_10_9_T2),  na.rm = FALSE),
    BFI_Openness_T2          = rowMeans(cbind(BFI_10_5_T2, BFI_10_10_T2), na.rm = FALSE),
    .after = BFI_10_10_T2
  )


# ── Spiritual Transcendence — STS (administered 24-item; labeled Piedmont, 1999) ──
# PROVENANCE: items do NOT match Piedmont (1999) (3/24 verbatim); unvalidated adapted
#   measure mislabeled as Piedmont. Scored as block SUMS, no reverse items.
# STRUCTURE: in-sample EFA (T1, N=484) did NOT recover the doc's 3 subscales. Items 1-8
#   form a clean Prayer Fulfillment factor; items 9-24 (intended Universality +
#   Connectedness) collapse onto ONE factor and do not separate. Scored accordingly:
#   PrayerFulfillment (1-8) + combined Universality-Connectedness (9-24) + Total.
# Response type: 1 (Strongly Disagree) to 5 (Strongly Agree)
data <- data %>%
  mutate(
    STS_PrayerFulfillment_T1   = rowSums(select(., paste0("STS_", 1:8,  "_T1")), na.rm = FALSE),
    STS_UnivConnect_T1         = rowSums(select(., paste0("STS_", 9:24, "_T1")), na.rm = FALSE),
    .after = STS_24_T1
  )
data <- data %>%
  mutate(STS_Total_T1 = STS_PrayerFulfillment_T1 + STS_UnivConnect_T1,
         .after = STS_UnivConnect_T1)
data <- data %>%
  mutate(
    STS_PrayerFulfillment_T2   = rowSums(select(., paste0("STS_", 1:8,  "_T2")), na.rm = FALSE),
    STS_UnivConnect_T2         = rowSums(select(., paste0("STS_", 9:24, "_T2")), na.rm = FALSE),
    .after = STS_24_T2
  )
data <- data %>%
  mutate(STS_Total_T2 = STS_PrayerFulfillment_T2 + STS_UnivConnect_T2,
         .after = STS_UnivConnect_T2)

# ── Transcendent Indebtedness to God — T-ITG (Nelson et al., 2023) ────────────
# Source: Nelson, J. M., Hardy, S. A., & Watkins, P. (2023). Transcendent indebtedness
#   to God: A new construct in the psychology of religion and spirituality.
#   Psychology of Religion and Spirituality, 15(1), 105-117.   [DOI: confirm]
# Scoring format: 6 items, 1-5, single unidimensional MEAN; no reverse scoring
# Subscales: none (unidimensional)
# Response type: 1 (Strongly disagree) to 5 (Strongly agree)
data <- data %>%
  mutate(ITG_T1 = rowMeans(select(., ITG_1_T1:ITG_6_T1), na.rm = FALSE), .after = ITG_6_T1)
data <- data %>%
  mutate(ITG_T2 = rowMeans(select(., ITG_1_T2:ITG_6_T2), na.rm = FALSE), .after = ITG_6_T2)


# ── Attachment to God Scale — AGS (Rowatt & Kirkpatrick, 2002) ───────────────
# Source: Rowatt, W. C., & Kirkpatrick, L. A. (2002). Two dimensions of attachment to God
#   and their relation to affect, religiosity, and personality constructs. Journal for the
#   Scientific Study of Religion, 41(4), 637-651. doi:10.1111/1468-5906.00143
# Scoring format: 9 items, two subscale SUMS; reverse items 1,4,6
# Subscales: Avoidance (1,2,4,6,7,9; reverse 1/4/6) | Anxiety (3,5,8; none reversed)
# Response type: administered 1 (Strongly disagree) to 5 (Strongly agree)
#   DEVIATION: published scale is 7-point; fielded here as 5-point for participant ease.

# --- Reverse-score items 1, 4, 6 (both timepoints), preserving labels ---
atg_all_reverse <- c(paste0("Attach_", c(1, 4, 6), "_T1"),
                     paste0("Attach_", c(1, 4, 6), "_T2"))
saved_labels_atg <- lapply(data[atg_all_reverse], attr, "label")
data <- data %>%
  mutate(across(all_of(atg_all_reverse), ~ reverse_score(., max = 5)))
for (col in atg_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_atg[[col]], " [REVERSE SCORED]")
}

# --- Subscale sums: T1 ---
data <- data %>%
  mutate(
    ATG_Avoidance_T1 = rowSums(cbind(Attach_1_T1, Attach_2_T1, Attach_4_T1,
                                     Attach_6_T1, Attach_7_T1, Attach_9_T1), na.rm = FALSE),
    ATG_Anxiety_T1   = rowSums(cbind(Attach_3_T1, Attach_5_T1, Attach_8_T1), na.rm = FALSE),
    .after = Attach_9_T1
  )

# --- Subscale sums: T2 ---
data <- data %>%
  mutate(
    ATG_Avoidance_T2 = rowSums(cbind(Attach_1_T2, Attach_2_T2, Attach_4_T2,
                                     Attach_6_T2, Attach_7_T2, Attach_9_T2), na.rm = FALSE),
    ATG_Anxiety_T2   = rowSums(cbind(Attach_3_T2, Attach_5_T2, Attach_8_T2), na.rm = FALSE),
    .after = Attach_9_T2
  )



# ── Daily Spiritual Experience Scale — DSES (Underwood, 2006) ────────────────
# 16 items, unidimensional. Mean of all 16 items. na.rm = FALSE.
# No reverse scoring — all items run the same direction (higher = more
# frequent / closer experience).
# Items 1-15: 6-point frequency scale (1 = Never ... 6 = Many times a day)
# Item 16:    4-point closeness scale ("Not at all close" ... "As close as
#             possible"). Recoded 1/3/5/6 to place it on the 1-6 metric of the
#             other items so its contribution to the mean is not compressed.
#             This is a scoring decision (not in Underwood); confirm SAV value
#             labels carry 1/3/5/6 and that higher = closer.

data <- data %>%
  mutate(
    DSES_T1 = rowMeans(select(., paste0("DSES_", 1:16, "_T1")), na.rm = FALSE),
    .after = DSES_16_T1
  )

data <- data %>%
  mutate(
    DSES_T2 = rowMeans(select(., paste0("DSES_", 1:16, "_T2")), na.rm = FALSE),
    .after = DSES_16_T2
  )


# ── Revised Religious Fundamentalism Scale — FUND (Altemeyer & Hunsberger, 2004)
# 12 items. Unidimensional sum score.
# Original scale uses -4 to +4 (9 points); Qualtrics version administered here
# uses a condensed -2 to +2 (5 points). Reverse-scoring logic and totalling
# method are identical — only the absolute range differs (range -24 to +24).
# Con-trait items (reverse scored via * -1): 2, 4, 7, 9, 10, 12

fund_reverse_t1 <- paste0("FUND_", c(2, 4, 7, 9, 10, 12), "_T1")
fund_reverse_t2 <- paste0("FUND_", c(2, 4, 7, 9, 10, 12), "_T2")

fund_all_reverse <- c(fund_reverse_t1, fund_reverse_t2)
saved_labels_fund <- lapply(data[fund_all_reverse], attr, "label")

data <- data %>%
  mutate(across(all_of(fund_all_reverse), ~ . * -1))

for (col in fund_all_reverse) {
  attr(data[[col]], "label") <- paste0(saved_labels_fund[[col]], " [REVERSE SCORED]")
}

data <- data %>%
  mutate(
    FUND_Total_T1 = rowSums(select(., paste0("FUND_", 1:12, "_T1")), na.rm = FALSE),
    .after = FUND_12_T1
  )

data <- data %>%
  mutate(
    FUND_Total_T2 = rowSums(select(., paste0("FUND_", 1:12, "_T2")), na.rm = FALSE),
    .after = FUND_12_T2
  )

# ── LAMBI God Representations Scale (Johnson et al., 2019) ───────────────────
# ADMINISTERED: full 41-item Study 1 EFA pool (GodConcept_1..41), NOT the
# published 25-item LAMBI. Composite uses the validated 25-item subset; the
# other 16 developmental items are retained in data but unused.
# 5 subscales × 5 items, mean of items. 7-point Likert (1 = strongly disagree
# ... 7 = strongly agree), "GOD IS ...". No reverse scoring (all keyed toward
# the dimension; higher = stronger endorsement). No overall LAMBI total.
# na.rm = FALSE per house rule (author's appendix uses SPSS mean.3; identical
# here since missingness is whole-scale).
# Final-25 item -> GodConcept number:
#   Limitless     : 19 boundless, 20 vast, 21 infinite, 22 immense, 23 limitless
#   Authoritarian : 1 strict, 2 punishing, 3 wrathful, 5 stern, 9 commanding
#   Mystical      : 27 nature, 28 universe, 29 energy, 30 consciousness, 31 cosmic
#   Benevolent    : 10 forgiving, 12 merciful, 14 compassionate, 16 gracious, 17 tolerant
#   Ineffable     : 34 unimaginable, 35 unknowable, 36 unknown, 37 incomprehensible, 38 inconceivable
# Note: item 33 is "Omnipresent" (administered) vs "Om" (paper); non-final, no impact.

data <- data %>%
  mutate(
    LAMBI_Limitless_T1     = rowMeans(select(., paste0("GodConcept_", c(19,20,21,22,23), "_T1")), na.rm = FALSE),
    LAMBI_Authoritarian_T1 = rowMeans(select(., paste0("GodConcept_", c(1,2,3,5,9),      "_T1")), na.rm = FALSE),
    LAMBI_Mystical_T1      = rowMeans(select(., paste0("GodConcept_", c(27,28,29,30,31), "_T1")), na.rm = FALSE),
    LAMBI_Benevolent_T1    = rowMeans(select(., paste0("GodConcept_", c(10,12,14,16,17), "_T1")), na.rm = FALSE),
    LAMBI_Ineffable_T1     = rowMeans(select(., paste0("GodConcept_", c(34,35,36,37,38), "_T1")), na.rm = FALSE),
    .after = GodConcept_41_T1
  )

data <- data %>%
  mutate(
    LAMBI_Limitless_T2     = rowMeans(select(., paste0("GodConcept_", c(19,20,21,22,23), "_T2")), na.rm = FALSE),
    LAMBI_Authoritarian_T2 = rowMeans(select(., paste0("GodConcept_", c(1,2,3,5,9),      "_T2")), na.rm = FALSE),
    LAMBI_Mystical_T2      = rowMeans(select(., paste0("GodConcept_", c(27,28,29,30,31), "_T2")), na.rm = FALSE),
    LAMBI_Benevolent_T2    = rowMeans(select(., paste0("GodConcept_", c(10,12,14,16,17), "_T2")), na.rm = FALSE),
    LAMBI_Ineffable_T2     = rowMeans(select(., paste0("GodConcept_", c(34,35,36,37,38), "_T2")), na.rm = FALSE),
    .after = GodConcept_41_T2
  )
 

saveRDS(data, here("Outputs", "02_cleaned_data_with_composites.rds"))
write.csv(data, here("Outputs", "02_cleaned_data_with_composites.csv"), row.names = FALSE)

