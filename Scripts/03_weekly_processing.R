# ============================================================================
# 03_weekly_processing.R
# ----------------------------------------------------------------------------
#   PART A — Data prep: read, clean, dedup all 24 weekly files -> weekly_dedup
#   PART B — Affect composites: PANAS-X + Gratitude + Empathic Concern scoring
#   PART C — Wide table (1 row/participant) + condition tag + sample filter
#   PART D — Attrition: weekly participation matrix
#
# Requires 01_cleaning.R & 02_create_composites. R to have been run (produces 02_cleaned_data_with_composites.rds)
#
# T1a is read from xlsx (no standalone SAV exists — Qualtrics download issue).
# T1b-T6b come from the 22 weekly SAV files.
# ============================================================================

library(haven)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(here)

# ============================================================================
# PART A — DATA PREP  
# ============================================================================

# ---- File lists -----------------------------------------------------------
sav_files <- list.files(here("SAV files", "Weekly"), full.names = TRUE)

xlsx_files <- list.files(here("Excels", "Weekly"),
                         full.names = TRUE,
                         pattern = "T1a")

# ---- Create helper functions -----------------------------------------------------

# Clean Finished to 0/1: handles numbers (1/0), text ("1"/"0"), booleans
# (TRUE/"TRUE"); anything unrecognized -> NA.
to_binary <- function(x) {
  x <- toupper(trimws(as.character(x)))
  case_when(
    x %in% c("1", "TRUE")  ~ 1,
    x %in% c("0", "FALSE") ~ 0,
    TRUE ~ NA_real_
  )
}

# Reader for the 22 SAV weeks: read + normalize Finished.
read_sav_tp <- function(path) {
  df <- read_sav(path)
  df$Finished <- to_binary(df$Finished)
  df
}

# Reader for the 2 XLSV weeks 
# Reads a T1a xlsx file and returns it in numeric form.
#   1. drops the first row (Qualtrics puts question text there, not data)
#   2. converts Finished to 0/1
#   3. converts survey-answer columns from text to numbers
# Why `donor`: xlsx reads as all-text, so we use the matching T1b sav — which
# has correct types — to tell which columns should be numbers vs. left as text.
read_t1a <- function(path, donor) {
  df <- read_xlsx(path)
  df <- df[-1, ]
  df$Finished <- to_binary(df$Finished)
  
  num_cols <- names(df)[map_lgl(names(df), function(nm) {
    nm != "Finished" && nm %in% names(donor) && is.numeric(donor[[nm]])
  })]
  df %>%
    mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))
}

###############################################################################
# ---- Read all 24 weeks ----------------------------------------------------
weekly_sav <- sav_files %>%
  set_names(str_c(str_extract(basename(.), "T[1-6][ab]"), "-",
                  str_extract(basename(.), "HP|LK"))) %>%
  map(read_sav_tp)

weekly_t1a <- list(
  "T1a-HP" = read_t1a(xlsx_files[str_detect(xlsx_files, "HP")], donor = weekly_sav[["T1b-HP"]]),
  "T1a-LK" = read_t1a(xlsx_files[str_detect(xlsx_files, "LK")], donor = weekly_sav[["T1b-LK"]])
)

weekly_raw <- c(weekly_sav, weekly_t1a)
weekly_raw <- weekly_raw[order(names(weekly_raw))]

length(weekly_raw)   # 24

# ---- Cleaning: standardize names, drop boilerplate metadata cols ----------
weekly_clean <- weekly_raw %>%
  map(~ rename(.x, participantId = matches(regex("^participant.?id$", ignore_case = TRUE)))) %>%
  map(~ rename(.x, Duration      = matches(regex("^duration",         ignore_case = TRUE)))) %>%
  map(~ select(.x,
               participantId,
               everything(),
               -any_of(c(
                 "EndDate", "Status", "IPAddress", "RecordedDate",
                 "RecipientLastName", "RecipientFirstName",
                 "RecipientEmail", "ExternalReference", "LocationLatitude",
                 "LocationLongitude", "DistributionChannel", "UserLanguage"
               ))
  ))

# ---- capture blank-ID rows before they're dropped for documentation purposes----------
empty_ids <- weekly_clean %>%
  imap_dfr(~ .x %>%
             filter(participantId == "" | is.na(participantId)) %>%
             transmute(
               week          = .y,
               participantId = as.character(participantId),
               Progress      = as.character(Progress),
               Finished      = as.character(Finished),
               ResponseId    = as.character(ResponseId)
             ))

# ---- Dedup + duration floor -----------------------------------------------
# Completion rule: usable participantId; keep first row per person (completed
# first, then earliest StartDate).
# Duration floor = 590s: the practice audio is a 10-min (600s) hard timer, with
# a 10s tolerance for duration-measurement error — sub-600s responses cluster
# tightly at 595-599s, consistent with genuine completion logged fractionally
# short rather than rushing. Forced-response administration guarantees all items
# are answered, so duration is the sole completion gate. Sub-590s responses are
# recoded Finished = 0 (didn't complete the practice), not deleted.
weekly_dedup <- weekly_clean %>%
  map(~ .x %>%
        filter(!(participantId == "" | is.na(participantId))) %>%
        mutate(dur = suppressWarnings(as.numeric(Duration))) %>%
        mutate(Finished = if_else(!is.na(dur) & dur < 590, 0, as.numeric(Finished))) %>%
        arrange(participantId, desc(Finished), StartDate) %>%
        distinct(participantId, .keep_all = TRUE) %>%
        select(-dur)
  )

# ---- Recover Excel-corrupted ENVMT1a in the T1a files ---------------------
# ENVMT1a's "1-5" response was auto-converted to date serial 45662 by Excel.
# Restore the text and re-code against a clean sav a-session's labels (ENVMT2a).
env_meta <- attr(weekly_dedup[["T2a-HP"]]$ENVMT2a, "labels")   # c("0"=1,"1-5"=2,"5+"=3)

decode_envmt <- function(x, meta) {
  x <- as.character(x)
  x <- ifelse(x == "45662", "1-5", x)
  unname(meta[x])
}

for (wk in c("T1a-HP", "T1a-LK")) {
  codes <- decode_envmt(weekly_dedup[[wk]]$ENVMT1a, env_meta)
  weekly_dedup[[wk]]$ENVMT1a <- labelled(codes, labels = env_meta)
}

# ---- Part A verification --------------------------------------------------
map_lgl(weekly_clean, ~ "participantId" %in% names(.x))    # all TRUE
map_lgl(weekly_clean, ~ "Duration"      %in% names(.x))    # all TRUE
map_int(weekly_dedup, ~ sum(duplicated(.x$participantId))) # all 0
map_int(weekly_dedup, ~ sum(is.na(.x$Finished)))           # all 0


# ============================================================================
# PART B — AFFECT COMPOSITES  (builds on weekly_dedup)
# ============================================================================
# Custom 27-item affect battery, PRE and POST, in every b-session. Four blocks
# (confirmed by in-sample inter-item correlations):
#   Items 1-3   Gratitude/Indebtedness — Grateful raw; items 2-3 = Indebted.
#   Items 4-11  PANAS-X Positive Affect (8-item reduced set).
#   Items 12-21 PANAS-X Negative Affect (10 items).
#   Items 22-27 Empathic Concern (Batson et al. 1987), 6 items.
# Item mean per block; na.rm = FALSE (any NA -> NA composite).
# ---------------------------------------------------------------------------

panas_weeks <- names(weekly_dedup) %>% str_subset("b-")

score_panas <- function(df, when, week_num) {
  stem <- str_c(when, "PANAS", week_num, "b_")     # e.g. "PREPANAS2b_"
  item <- function(n) as.numeric(df[[str_c(stem, n)]])
  block_mean <- function(nums) rowMeans(sapply(nums, item), na.rm = FALSE)
  
  df %>%
    mutate(
      "{when}_Grateful"   := item(1),
      "{when}_Indebted"   := block_mean(2:3),
      "{when}_PA"         := block_mean(4:11),
      "{when}_NA"         := block_mean(12:21),
      "{when}_EmpConcern" := block_mean(22:27),
      .after = str_c(stem, 27)
    )
}

weekly_scored <- weekly_dedup
weekly_scored[panas_weeks] <- panas_weeks %>%
  set_names() %>%
  map(~ {
    wk <- str_extract(.x, "\\d")
    weekly_dedup[[.x]] %>%
      score_panas("PRE", wk) %>%
      score_panas("PO",  wk)
  })

# ---- Part B verification --------------------------------------------------
map(weekly_scored[panas_weeks], ~ str_subset(names(.x), "^PRE_|^PO_")) %>%
  map_int(length)                                            # all 10

# ============================================================================
# PART C — WIDE TABLE + CONDITION + SAMPLE FILTER -> weekly_wide
# ============================================================================
# One row per participant, each week's questions as their own columns (session
# tag kept in name). Condition + outcomes come from script 2's composite dataset
# (which is built on script 1's cleaned data and adds the scored outcomes).
# ---------------------------------------------------------------------------

weekly_wide <- weekly_scored %>%
  imap(~ {
    wk <- str_extract(.y, "T\\d+[ab]")
    .x %>%
      select(participantId,
             any_of(c("PRE_Grateful","PRE_Indebted","PRE_PA","PRE_NA","PRE_EmpConcern",
                      "PO_Grateful","PO_Indebted","PO_PA","PO_NA","PO_EmpConcern")),
             matches("^ENVMT|^Distract|^ALTTASK")) %>%
      rename_with(~ str_c(.x, "_", wk), .cols = -participantId)
  }) %>%
  reduce(full_join, by = "participantId")

# Composite dataset from script 2 = condition roster + scored outcomes.
# (Depends on 02_create_composites.R having been run.)
cleaned <- readRDS(here("Outputs", "02_cleaned_data_with_composites.rds"))
roster  <- cleaned %>% select(participantId, Condition) %>% distinct()

weekly_wide <- weekly_wide %>%
  left_join(roster, by = "participantId") %>%
  relocate(Condition, .after = participantId)

# Drop participants not in the analytic sample.
# 15 had weekly data but no Condition: 3 never randomized, 1 double-assigned,
# 11 randomized but excluded by T1/T2 QC (none for weekly-data reasons).
# Weekly analyses link to T1/T2, so all 15 dropped for one consistent N.
drop_ids <- weekly_wide %>% filter(is.na(Condition)) %>% pull(participantId)

weekly_wide   <- weekly_wide   %>% filter(!participantId %in% drop_ids)
weekly_scored <- map(weekly_scored, ~ filter(.x, !participantId %in% drop_ids))
weekly_dedup  <- map(weekly_dedup,  ~ filter(.x, !participantId %in% drop_ids))

# ---- Part C verification --------------------------------------------------
sum(is.na(weekly_wide$Condition))                          # 0
nrow(weekly_wide)                                          # 379
length(unique(weekly_wide$participantId))                  # 379
count(weekly_wide, Condition)                              # HPrayer + LovingKindness

# ============================================================================
# PART D — ATTRITION / PARTICIPATION MATRIX
# ============================================================================
# One row per enrolled TREATMENT participant (roster spine, all 387 incl. those
# who never started weeklies — Control excluded, they get no check-ins).
# Cell = 1 if they completed the practice that week (Finished==1, which already
# requires Duration>=590s), else 0. Never-starters and non-completers show as 0.
# NOTE: because the practice IS the intervention, "opened but didn't finish the
# 10-min practice" counts the same as absent — both are 0 (no dose delivered).
# ---------------------------------------------------------------------------

week_order <- c("T1a","T1b","T2a","T2b","T3a","T3b",
                "T4a","T4b","T5a","T5b","T6a","T6b")

# Treatment-arm roster = enrollment spine
attrition_roster <- roster %>%
  filter(Condition %in% c("HPrayer", "LovingKindness"))

# Who completed each week (Finished == 1)
completion_long <- weekly_dedup %>%
  imap_dfr(~ .x %>%
             filter(Finished == 1) %>%
             transmute(participantId,
                       week = str_extract(.y, "T\\d+[ab]"),
                       completed = 1))

# Pivot to one column per week
completion_wide <- completion_long %>%
  distinct(participantId, week, completed) %>%
  pivot_wider(names_from = week, values_from = completed, values_fill = 0)

# Join onto roster spine: everyone enrolled appears; non-completers fill 0
attrition <- attrition_roster %>%
  left_join(completion_wide, by = "participantId") %>%
  mutate(across(any_of(week_order), ~ replace_na(., 0))) %>%
  select(participantId, Condition, any_of(week_order))

# ---- Part D verification --------------------------------------------------
nrow(attrition)                                    # 387 (all treatment enrolled)
count(attrition, Condition)                        # HPrayer 194, LovingKindness 193
attrition %>% summarise(across(any_of(week_order), sum))   # completions per timepoint


# Visualize
library(ggplot2)

curve_arm <- attrition %>%
  group_by(Condition) %>%
  summarise(across(any_of(week_order), sum)) %>%
  pivot_longer(-Condition, names_to = "week", values_to = "n") %>%
  mutate(week = factor(week, levels = week_order))

ggplot(curve_arm, aes(week, n, group = Condition, linetype = Condition)) +
  geom_line() +
  geom_point() +
  labs(x = "Timepoint", y = "Completed", title = "Weekly completion by arm")

# ============================================================================
# PART E — ADHERENCE MEASURE (attach weekly completion to outcome data)
# ============================================================================
# Collapse the attrition matrix into a per-person adherence measure, then join
# it onto the SCORED OUTCOME dataset (script 2's composites) so every outcome
# analysis can be hedged by how much of the practice each participant completed.
# ---------------------------------------------------------------------------

# Per-person adherence from the attrition matrix
adherence <- attrition %>%
  mutate(
    weeks_completed = rowSums(across(any_of(week_order))),   # 0-12
    pct_completed   = weeks_completed / length(week_order) * 100
  ) %>%
  select(participantId, Condition, weeks_completed, pct_completed)

# Join onto the composite/outcome data (`cleaned`, from script 2).
# Treatment never-starters -> 0 (offered the practice, did none). Control was
# never offered the practice, so adherence is undefined -> NA (not 0).
outcomes_with_adherence <- cleaned %>%
  left_join(select(adherence, participantId, weeks_completed, pct_completed),
            by = "participantId") %>%
  mutate(across(c(weeks_completed, pct_completed),
                ~ if_else(Condition %in% c("HPrayer", "LovingKindness") & is.na(.), 0, .))) %>%
  relocate(weeks_completed, pct_completed, .after = Condition)

# ---- verification ---------------------------------------------------------
nrow(outcomes_with_adherence)                    # 484
# outcome composites now present alongside adherence?
names(outcomes_with_adherence) %>% str_subset("ANXIETY|DEPRESSION|Flourish")
# adherence distribution by arm
outcomes_with_adherence %>%
  group_by(Condition) %>%
  summarise(n = n(),
            mean_weeks = mean(weeks_completed, na.rm = TRUE),
            mean_pct   = mean(pct_completed, na.rm = TRUE),
            n_NA       = sum(is.na(weeks_completed)))

# ============================================================================
# SAVE OUTPUTS
# ============================================================================

saveRDS(outcomes_with_adherence, here("Outputs", "03_outcomes_with_adherence.rds"))
saveRDS(weekly_wide,             here("Outputs", "weekly_wide.rds"))
saveRDS(attrition,               here("Outputs", "attrition_matrix.rds"))
saveRDS(weekly_scored,           here("Outputs", "weekly_scored.rds"))

# Optional flat CSVs for the non-list tables (Excel/sharing)
write_csv(outcomes_with_adherence, here("Outputs", "03_outcomes_with_adherence.csv"))
write_csv(attrition,               here("Outputs", "attrition_matrix.csv"))

# ---- confirm the files were written ---------------------------------------
list.files(here("Outputs"), pattern = "adherence|weekly_wide|attrition|weekly_scored")
