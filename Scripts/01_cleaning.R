# install.packages("haven")    # run once if not already installed
# install.packages("writexl")  # run once if not already installed
# install.packages("here")     # run once if not already installed
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(writexl)
library(here)

# ---- Input file paths (relative to project root) ----
intake_path  <- here("SAV files", "Hesychastic Prayer RCT OFFICIAL Intake_October 17, 2025_14.27.sav")
outtake_path <- here("SAV files", "Hesychastic Prayer RCT OFFICIAL Outtake (Copied)_October 17, 2025_14.28.sav")
control_path <- here("Group assignments", "Control (REAL)_01_19_2026 15_45_03.csv")
hprayer_path <- here("Group assignments", "HP Group (REAL)_01_19_2026 15_45_03.csv")
lk_path      <- here("Group assignments", "LK Group (REAL)_01_19_2026 15_45_03.csv")

# ---- CONSORT flow counts ----
# Captured as the script runs and saved as flow_counts.rds at the end, so the
# CONSORT diagram (flow_diagram.R) always reflects the actual run.  
flow <- list()

######## 1) LOAD DATA

intake_data <- read_sav(intake_path)

cat("Loaded intake data:", nrow(intake_data), "participants\n\n")
flow$intake_loaded <- nrow(intake_data)

# Clean participant IDs (convert whitespace-only IDs to NA)
intake_data <- intake_data %>%
  mutate(participantId = na_if(trimws(participantId), ""))

# First remove NA participant IDs (IDs that were not captured and cannot be linked to responses)
temp_n <- nrow(intake_data)
intake_data <- intake_data %>%
  filter(!is.na(participantId))
cat("Removed participants with missing IDs:", temp_n - nrow(intake_data), "\n")
cat("Remaining:", nrow(intake_data), "\n\n")
flow$removed_missing_id <- temp_n - nrow(intake_data)

# Then check for duplicate participant IDs
duplicates <- intake_data %>%
  group_by(participantId) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicates) > 0) {
  n_duplicated_ids <- duplicates %>%
    distinct(participantId) %>%
    nrow()
  
  cat("Found", nrow(duplicates), "rows involving", n_duplicated_ids, "participant(s) with duplicates\n")
  
  temp_n <- nrow(intake_data)
  intake_data <- intake_data %>%
    group_by(participantId) %>%
    slice(1) %>%
    ungroup()
  
  cat("Removed", temp_n - nrow(intake_data), "duplicate row(s)\n")
  cat("Remaining:", nrow(intake_data), "\n\n")
  flow$removed_dup_id <- temp_n - nrow(intake_data)
} else {
  cat("No duplicates found\n\n")
  flow$removed_dup_id <- 0
}

########## 2) ELIGIBILITY SCREENING

cat("=== ELIGIBILITY SCREENING ===\n")
cat("Starting N:", nrow(intake_data), "\n\n")

cat("--- Individual Eligibility Criteria ---\n")

# At each stage, only remove those who FAIL in order to get accurate counts of exclusion reasons

# Age criterion
# Participants must be older than 18
cat("Age (AGET1 >= 18):\n")
cat("  Pass (>= 18):", sum(intake_data$AGET1 >= 18, na.rm = TRUE), "\n")
cat("  Fail (< 18):", sum(intake_data$AGET1 < 18, na.rm = TRUE), "\n")
cat("  Missing (did not reach / did not answer):", sum(is.na(intake_data$AGET1)), "\n")

temp_n <- nrow(intake_data)
eligible <- intake_data %>%
  filter(is.na(AGET1) | AGET1 >= 18)

cat("  Decision rule: remove Fail only; keep Missing until completion screen\n")
cat("  Removed at this step:", temp_n - nrow(eligible), "\n")
cat("  Remaining:", nrow(eligible), "\n\n")
flow$removed_age <- temp_n - nrow(eligible)

# Christian criterion
# Participants must identify as Christian
cat("Christian (CHRISTIANIDT1 == 1):\n")
cat("  Pass (Christian):", sum(eligible$CHRISTIANIDT1 == 1, na.rm = TRUE), "\n")
cat("  Fail (Non-Christian):", sum(eligible$CHRISTIANIDT1 != 1, na.rm = TRUE), "\n")
cat("  Missing (did not reach / did not answer):", sum(is.na(eligible$CHRISTIANIDT1)), "\n")

temp_n <- nrow(eligible)
eligible <- eligible %>%
  filter(is.na(CHRISTIANIDT1) | CHRISTIANIDT1 == 1)

cat("  Decision rule: remove Fail only; keep Missing until completion screen\n")
cat("  Removed at this step:", temp_n - nrow(eligible), "\n")
cat("  Remaining:", nrow(eligible), "\n\n")
flow$removed_christian <- temp_n - nrow(eligible)

# Attention check criterion
# Must pass attention check (within survey)
cat("Attention Check (AC1_ == 1):\n")
cat("  Pass:", sum(eligible$AC1_ == 1, na.rm = TRUE), "\n")
cat("  Fail:", sum(eligible$AC1_ != 1, na.rm = TRUE), "\n")
cat("  Missing (did not reach / did not answer):", sum(is.na(eligible$AC1_)), "\n")

temp_n <- nrow(eligible)
eligible <- eligible %>%
  filter(is.na(AC1_) | AC1_ == 1)

cat("  Decision rule: remove Fail only; keep Missing until completion screen\n")
cat("  Removed at this step:", temp_n - nrow(eligible), "\n")
cat("  Remaining:", nrow(eligible), "\n\n")
flow$removed_attention <- temp_n - nrow(eligible)

# God screen criterion
# Participants must answer yes to belief in God question
cat("God Screen (God_Screen_ == 1):\n")
cat("  Pass (Believe in God):", sum(eligible$God_Screen_ == 1, na.rm = TRUE), "\n")
cat("  Fail (Do not believe):", sum(eligible$God_Screen_ != 1, na.rm = TRUE), "\n")
cat("  Missing (did not reach / did not answer):", sum(is.na(eligible$God_Screen_)), "\n")

temp_n <- nrow(eligible)
eligible <- eligible %>%
  filter(is.na(God_Screen_) | God_Screen_ == 1)

cat("  Decision rule: remove Fail only; keep Missing until completion screen\n")
cat("  Removed at this step:", temp_n - nrow(eligible), "\n")
cat("  Remaining:", nrow(eligible), "\n\n")
flow$removed_godscreen <- temp_n - nrow(eligible)

# Survey completion criterion
cat("Survey Completion (Progress == 100):\n")
cat("  Complete:", sum(eligible$Progress == 100, na.rm = TRUE), "\n")
cat("  Incomplete:", sum(eligible$Progress != 100, na.rm = TRUE), "\n")
cat("  NA:", sum(is.na(eligible$Progress)), "\n")

temp_n <- nrow(eligible)
eligible <- eligible %>%
  filter(Progress == 100)

cat("  Decision rule: remove all incomplete (includes all prior Missing)\n")
cat("  Removed at this step:", temp_n - nrow(eligible), "\n")
cat("  Remaining:", nrow(eligible), "\n\n")
flow$removed_incomplete <- temp_n - nrow(eligible)

cat("=== FINAL ELIGIBLE SAMPLE ===\n")
cat("Eligible for randomization:", nrow(eligible), "\n\n")
flow$eligible <- nrow(eligible)

########## 3) ASSIGN CONDITIONS

cat("=== ASSIGNING TO CONDITIONS ===\n")

control      <- read_csv(control_path)
hprayer      <- read_csv(hprayer_path)
lovingkindness <- read_csv(lk_path)

control        <- control        %>% mutate(Condition = "Control")
hprayer        <- hprayer        %>% mutate(Condition = "HPrayer")
lovingkindness <- lovingkindness %>% mutate(Condition = "LovingKindness")

all_conditions <- bind_rows(control, hprayer, lovingkindness)

cat("Randomized to conditions:\n")
cat("  Control:", nrow(control), "\n")
cat("  H Prayer:", nrow(hprayer), "\n")
cat("  Loving Kindness:", nrow(lovingkindness), "\n")
cat("  Total:", nrow(all_conditions), "\n\n")

# Check for duplicate assignments (participants in multiple conditions)
duplicate_assignments <- all_conditions %>%
  group_by(ParticipantId) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(duplicate_assignments) > 0) {
  n_duplicated_ids <- duplicate_assignments %>%
    distinct(ParticipantId) %>%
    nrow()
  
  cat("WARNING: Found", nrow(duplicate_assignments), "rows involving", n_duplicated_ids,
      "participant(s) with duplicate condition assignments\n")
  print(duplicate_assignments %>% select(ParticipantId, Condition))
  
  duplicate_ids <- unique(duplicate_assignments$ParticipantId)
  all_conditions <- all_conditions %>%
    filter(!ParticipantId %in% duplicate_ids)
  
  cat("\nRemoved", length(duplicate_ids), "participant(s) with duplicate assignments\n")
  cat("Remaining in all_conditions:", nrow(all_conditions), "\n\n")
  flow$removed_dup_assignment <- length(duplicate_ids)
} else {
  cat("No duplicate assignments found\n\n")
  flow$removed_dup_assignment <- 0
}

# Join conditions to eligible participants
eligible <- eligible %>%
  left_join(all_conditions, by = c("participantId" = "ParticipantId"))

cat("After joining conditions:\n")
cat("  Assigned to condition:", sum(!is.na(eligible$Condition)), "\n")
cat("  Not assigned:", sum(is.na(eligible$Condition)), "\n\n")

# Check for participants without condition assignments
unassigned <- eligible %>%
  filter(is.na(Condition))

if (nrow(unassigned) > 0) {
  cat("WARNING: Found", nrow(unassigned), "participant(s) not assigned to a condition\n")
  cat("These participants completed intake after randomization\n")
  print(unassigned$participantId)
  
  temp_n <- nrow(eligible)
  eligible <- eligible %>%
    filter(!is.na(Condition))
  
  cat("\nRemoved", temp_n - nrow(eligible), "participant(s) who were not randomized\n")
  cat("Final eligible sample:", nrow(eligible), "\n\n")
  flow$removed_unassigned <- temp_n - nrow(eligible)
} else {
  cat("All eligible participants assigned to conditions\n\n")
  flow$removed_unassigned <- 0
}

cat("Final sample by condition:\n")
print(table(eligible$Condition, useNA = "ifany"))
cat("\n")
flow$randomized <- nrow(eligible)
flow$alloc <- as.list(table(eligible$Condition))

########## 4) LOAD OUTTAKE & MERGE

outtake_data <- read_sav(outtake_path)

cat("Loaded outtake data:", nrow(outtake_data), "participants\n\n")

# Clean outtake participant IDs
outtake_data <- outtake_data %>%
  mutate(participantId = na_if(trimws(participantId), ""))

# Check for duplicates in outtake
cat("=== CHECKING OUTTAKE FOR DUPLICATES ===\n")

outtake_duplicates <- outtake_data %>%
  filter(!is.na(participantId)) %>%
  group_by(participantId) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(outtake_duplicates) > 0) {
  n_dup_ids <- outtake_duplicates %>%
    distinct(participantId) %>%
    nrow()
  
  cat("Found", nrow(outtake_duplicates), "rows involving", n_dup_ids, "participant(s) with duplicates\n")
  
  temp_n <- nrow(outtake_data)
  outtake_data <- outtake_data %>%
    filter(!is.na(participantId)) %>%
    group_by(participantId) %>%
    slice(1) %>%
    ungroup()
  
  cat("Removed", temp_n - nrow(outtake_data), "duplicate row(s)\n")
  cat("Remaining:", nrow(outtake_data), "\n\n")
} else {
  cat("No duplicates found in outtake\n\n")
}

# Merge
merged_data <- eligible %>%
  left_join(outtake_data, by = "participantId", suffix = c("", ".y"))

cat("After merge:", nrow(merged_data), "participants\n\n")

# Move participantId and Condition to front
all_columns   <- names(merged_data)
other_columns <- all_columns[!all_columns %in% c("participantId", "Condition")]
merged_data   <- merged_data[, c("participantId", "Condition", other_columns)]

########## 5) T2 RETURN RATES

cat("=== T2 RETURN RATES ===\n\n")

started_t2 <- sum(!is.na(merged_data$Progress.y))
cat("Participants who completed intake (T1):", nrow(merged_data), "\n")
cat("Participants who started outtake (T2):", started_t2, "\n\n")

for (cond in c("Control", "HPrayer", "LovingKindness")) {
  n_in_condition <- sum(merged_data$Condition == cond, na.rm = TRUE)
  n_started      <- sum(merged_data$Condition == cond & !is.na(merged_data$Progress.y), na.rm = TRUE)
  return_rate    <- round(n_started / n_in_condition * 100, 1)
  
  cat(cond, ":\n")
  cat("  Started (T1):", n_in_condition, "\n")
  cat("  Started (T2):", n_started, "\n")
  cat("  Return rate:", return_rate, "%\n\n")
}

# Capture per-arm T2 return for the CONSORT follow-up row
flow$returned_t2 <- sapply(c("Control", "HPrayer", "LovingKindness"), function(cond)
  sum(merged_data$Condition == cond & !is.na(merged_data$Progress.y), na.rm = TRUE))

########## 6) QUALITY CONTROL

cat("=== DATA QUALITY CONTROL ===\n\n")
cat("Starting N:", nrow(merged_data), "\n\n")

# Per-arm counts entering QC (for the CONSORT analysis row)
flow$pre_qc <- as.list(table(merged_data$Condition))

qc_checks <- list(
  
  # Step 1: Survey Completion
  unfinished_t1 = list(
    step = "Survey Completion",
    name = "Unfinished at T1",
    var = "Finished",
    type = "equals",
    threshold = 1
  ),
  unfinished_t2 = list(
    step = "Survey Completion",
    name = "Unfinished at T2",
    var = "Finished.y",
    type = "equals",
    threshold = 1
  ),
  
  # Step 2: Duplicate Detection
  relevant_dup_t1 = list(
    step = "Duplicate Detection",
    name = "Q_RelevantIDDuplicate at T1",
    var = "Q_RelevantIDDuplicate",
    type = "not_true"
  ),
  relevant_dup_t2 = list(
    step = "Duplicate Detection",
    name = "Q_RelevantIDDuplicate at T2",
    var = "Q_RelevantIDDuplicate.y",
    type = "not_true"
  ),
  duplicate_resp_t1 = list(
    step = "Duplicate Detection",
    name = "Q_DuplicateRespondent at T1",
    var = "Q_DuplicateRespondent",
    type = "not_true"
  ),
  duplicate_resp_t2 = list(
    step = "Duplicate Detection",
    name = "Q_DuplicateRespondent at T2",
    var = "Q_DuplicateRespondent.y",
    type = "not_true"
  ),
  dup_score_t1 = list(
    step = "Duplicate Detection",
    name = "Q_RelevantIDDuplicateScore = 100 at T1",
    var = "Q_RelevantIDDuplicateScore",
    type = "not_equals",
    threshold = 100
  ),
  dup_score_t2 = list(
    step = "Duplicate Detection",
    name = "Q_RelevantIDDuplicateScore = 100 at T2",
    var = "Q_RelevantIDDuplicateScore.y",
    type = "not_equals",
    threshold = 100
  ),
  
  # Step 3: Response Duration
  duration_t1 = list(
    step = "Response Duration",
    name = "Duration < 600 seconds at T1",
    var = "Duration__in_seconds_",
    type = "greater_equal",
    threshold = 600
  ),
  duration_t2 = list(
    step = "Response Duration",
    name = "Duration < 600 seconds at T2",
    var = "Duration__in_seconds_.y",
    type = "greater_equal",
    threshold = 600
  ),
  
  # Step 4: Ballot Box Stuffing
  ballot_box_t1 = list(
    step = "Ballot Box Stuffing",
    name = "Q_BallotBoxStuffing at T1",
    var = "Q_BallotBoxStuffing",
    type = "not_true"
  ),
  ballot_box_t2 = list(
    step = "Ballot Box Stuffing",
    name = "Q_BallotBoxStuffing at T2",
    var = "Q_BallotBoxStuffing.y",
    type = "not_true"
  ),
  
  # Step 5: Attention Check at T2 (T1 was checked in eligibility screening)
  attention_t2 = list(
    step = "Attention Checks",
    name = "Failed attention check at T2",
    var = "AC1_.y",
    type = "equals",
    threshold = 1
  )
  
  # NOTE: RelevantID fraud score (Q_RelevantIDFraudScore) is intentionally NOT used.
  # Qualtrics deprecated RelevantID on 2025-06-30 and advises that these fields be
  # ignored; the score has no documented threshold and was called unreliable by the
  # vendor. Duplicate detection (above) is Qualtrics' recommended replacement.
)

current_step <- ""

for (check in qc_checks) {
  
  if (check$step != current_step) {
    if (current_step != "") {
      cat("Remaining:", nrow(merged_data), "\n\n")
    }
    cat("---", check$step, "---\n")
    current_step <- check$step
  }
  
  temp_n <- nrow(merged_data)
  
  if (check$type == "equals") {
    merged_data <- merged_data %>%
      filter(is.na(.data[[check$var]]) | .data[[check$var]] == check$threshold)
    
  } else if (check$type == "not_equals") {
    merged_data <- merged_data %>%
      filter(is.na(.data[[check$var]]) | .data[[check$var]] != check$threshold)
    
  } else if (check$type == "not_true") {
    merged_data <- merged_data %>%
      filter(is.na(.data[[check$var]]) | .data[[check$var]] != "true")
    
  } else if (check$type == "greater_equal") {
    merged_data <- merged_data %>%
      filter(is.na(.data[[check$var]]) | .data[[check$var]] >= check$threshold)
    
  } else if (check$type == "less_equal") {
    merged_data <- merged_data %>%
      filter(is.na(.data[[check$var]]) | .data[[check$var]] <= check$threshold)
  }
  
  removed <- temp_n - nrow(merged_data)
  cat(check$name, "- Removed:", removed, "\n")
}

cat("Remaining:", nrow(merged_data), "\n\n")
cat("=== QUALITY CONTROL COMPLETE ===\n")
cat("Final N after all quality control:", nrow(merged_data), "\n\n")
cat("Final sample by condition:\n")
print(table(merged_data$Condition, useNA = "ifany"))
cat("\n")

# Per-arm counts after QC (post_qc minus pre_qc = QC exclusions per arm)
flow$post_qc <- as.list(table(merged_data$Condition))

########## 7) STANDARDIZE COLUMN NAMES
#Variable_Question Number_Timepoint

# 1) Drop merge artifacts and admin duplicates
merged_data <- merged_data %>%
  select(-matches("\\.y$"),
         -`Insertion Time (America/New_York)`)

# Drop Qualtrics admin columns with no analytic value
merged_data <- merged_data %>%
  select(-RecipientLastName, -RecipientFirstName, -RecipientEmail,
         -IPAddress, -LocationLatitude, -LocationLongitude,
         -Welcome, -ResponseId, -ExternalReference,
         -DistributionChannel, -UserLanguage, -Q_RelevantIDLastStartDate)

# Drop spent QC flag variables (used during Quality Control, now inert).
# Note: the .y (T2) versions were already dropped by the matches("\\.y$") step above,
# so only the T1 versions remain to remove here.
merged_data <- merged_data %>%
  select(-any_of(c("Q_RelevantIDDuplicate", "Q_RelevantIDDuplicateScore",
                   "Q_RelevantIDFraudScore", "Q_DuplicateRespondent",
                   "Q_BallotBoxStuffing")))

# 2) Simple one-off renames
merged_data <- merged_data %>%
  rename(AGE_T1         = AGET1,
         SEX_T1         = SEXT1,
         CHRISTIANID_T1 = CHRISTIANIDT1,
         AC_T1          = `AC1_`,
         God_Screen_T1  = `God_Screen_`,
         qualgod_T1     = qualgod,
         debrief_T1     = debrief,
         Duration_seconds_T1 = Duration__in_seconds_)

# 3) Scale specific renames
# Empathy items: item 2 was skipped in Qualtrics so items 3-21 are labelled one too high
#    Rename to correct item numbers (3->2, 4->3, ... 21->20)
old_names_t1 <- paste0("Empathy_", 3:21)
new_names_t1 <- paste0("Empathy_", 2:20)
merged_data <- merged_data %>%
  rename_with(~ new_names_t1, all_of(old_names_t1))

old_names_t2 <- paste0("Empathy_", 3:21, "_T2")
new_names_t2 <- paste0("Empathy_", 2:20, "_T2")
merged_data <- merged_data %>%
  rename_with(~ new_names_t2, all_of(old_names_t2))

# Nature connectedness: items 10 and 11 were switched 
merged_data <- merged_data %>%
  rename(NatCon_TEMP  = NatCon_T1_11,
         NatCon_T1_11 = NatCon_T1_10) %>%
  rename(NatCon_T1_10 = NatCon_TEMP)

merged_data <- merged_data %>%
  rename(NatCon_TEMP  = NatCon_T2_11,
         NatCon_T2_11 = NatCon_T2_10) %>%
  rename(NatCon_T2_10 = NatCon_TEMP)

# Recode PSQI sleep disturbance items from administered 1-4 to PSQI standard 0-3
sleep5_cols <- grep("^Sleep5_[0-9]+_T[12]$", names(merged_data), value = TRUE)
merged_data[sleep5_cols] <- lapply(merged_data[sleep5_cols], function(x) as.numeric(x) - 1)

# 4) Add _T1 suffix to scales missing it
merged_data <- merged_data %>%
  rename_with(~ paste0(., "_T1"), starts_with("BEAUTY__")     & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("FLOURISH_HLS") & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("FL_")          & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("ANX_")         & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("MAIA_2_")      & !contains("T2"))   %>%
  rename_with(~ paste0(., "_T1"), starts_with("SCOMPASS_")    & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("Forgiveness_") & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("Empathy_")     & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("BFI_10_")      & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("STS_")         & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("ITG_")         & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("FUND_")        & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("GodConcept_")  & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("Sleep5_")      & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("LON1_")        & !ends_with("_T2")) %>%
  rename_with(~ paste0(., "_T1"), starts_with("LON2__")       & !ends_with("_T2"))

# 5) BDI: prefix -> suffix
merged_data <- merged_data %>%
  rename_with(~ gsub("^T1_BDI_(.+)", "BDI_\\1_T1", .), starts_with("T1_BDI_")) %>%
  rename_with(~ gsub("^T2_BDI_(.+)", "BDI_\\1_T2", .), starts_with("T2_BDI_"))

# 6) Attachment: prefix -> suffix
merged_data <- merged_data %>%
  rename_with(~ gsub("^T1Attach_(.+)", "Attach_\\1_T1", .), starts_with("T1Attach_")) %>%
  rename_with(~ gsub("^T2Attach_(.+)", "Attach_\\1_T2", .), starts_with("T2Attach_"))

# 7) DSES (spirituality): prefix -> suffix, T1spir2.0 and T2spir2.0 -> item 16
merged_data <- merged_data %>%
  rename_with(~ gsub("^T1spir(.+)", "DSES_\\1_T1", .), starts_with("T1spir")) %>%
  rename_with(~ gsub("^T2spir(.+)", "DSES_\\1_T2", .), starts_with("T2spir")) %>%
  rename(DSES_16_T1 = `DSES_2.0_T1`,
         DSES_16_T2 = `DSES_2.0_T2`)

# 8) UCLA Loneliness (3-item): LON1 -> UCLA_LON T1, LON2_1/2/3 -> UCLA_LON T2
merged_data <- merged_data %>%
  rename_with(~ gsub("^LON1_(.+)_T1", "UCLA_LON_\\1_T1", .), starts_with("LON1_")) %>%
  rename(UCLA_LON_1_T2 = LON2_1,
         UCLA_LON_2_T2 = LON2_2,
         UCLA_LON_3_T2 = LON2_3)

# 9) De Jong Gierveld Loneliness (11-item): LON2__ -> DGLon T1, LON_T2__ -> DGLon T2
merged_data <- merged_data %>%
  rename_with(~ gsub("^LON2__(.+)_T1",  "DGLon_\\1_T1", .), starts_with("LON2__"))  %>%
  rename_with(~ gsub("^LON_T2__(.+)", "DGLon_\\1_T2", .), starts_with("LON_T2__"))

# 10) MAIA T2: fix item-first convention to match T1 (MAIA_2_T2_1 -> MAIA_2_1_T2)
merged_data <- merged_data %>%
  rename_with(~ gsub("^MAIA_2_T2_(.+)", "MAIA_2_\\1_T2", .), starts_with("MAIA_2_T2_"))

# 11) RELAFF: move T1 from middle to suffix
merged_data <- merged_data %>%
  rename_with(~ gsub("^RELAFF_T1_(.+)", "RELAFF_\\1_T1", .), starts_with("RELAFF_T1_"))

# 12) BEAUTY: remove double underscore
merged_data <- merged_data %>%
  rename_with(~ gsub("BEAUTY__", "BEAUTY_", .), starts_with("BEAUTY__"))

# 13) MEQ30: move timepoint from middle to suffix
merged_data <- merged_data %>%
  rename_with(~ gsub("^MEQ30_T1_(.+)", "MEQ30_\\1_T1", .), starts_with("MEQ30_T1_")) %>%
  rename_with(~ gsub("^MEQ30_T2_(.+)", "MEQ30_\\1_T2", .), starts_with("MEQ30_T2_"))

# Rename MEQ columns: instrument is Hood's 32-item M-Scale, not the 30-item MEQ30
names(merged_data) <- gsub("^MEQ30_", "MEQ_", names(merged_data))

# 14) NatCon: move timepoint from middle to suffix
merged_data <- merged_data %>%
  rename_with(~ gsub("^NatCon_T1_(.+)", "NatCon_\\1_T1", .), starts_with("NatCon_T1_")) %>%
  rename_with(~ gsub("^NatCon_T2_(.+)", "NatCon_\\1_T2", .), starts_with("NatCon_T2_"))

# 15) Process evaluation variables: clean up double underscores
merged_data <- merged_data %>%
  rename(Continuation_M1 = `Continuation__M1_`,
         Reflection_M3   = `Reflection__M3_`,
         OtherStudies_M2 = `Other_Studies__M2_`)

# Fix RACE text variable
merged_data <- merged_data %>%
  rename(RACE_7_TEXT_T1 = RACE_T1_7_TEXT)

# Fix Flourish: insert underscore between abbreviation and item number
merged_data <- merged_data %>%
  rename_with(~ gsub("([A-Za-z])([0-9])(_T)", "\\1_\\2\\3", .),
              starts_with("FLOURISH_") | starts_with("FL_"))


#16) Spent QC flag columns are dropped in the admin-column step above (section 7, part 1)


##########  SAVE

merged_data <- merged_data %>%
  mutate(Condition = factor(Condition, levels = c("Control", "HPrayer", "LovingKindness")))

saveRDS(merged_data, here("Outputs", "1_cleaned_hp_dataR.rds"))
write_xlsx(merged_data, here("Outputs", "1_cleaned_hp_dataR.xlsx"))

flow$analyzed <- as.list(table(merged_data$Condition))
saveRDS(flow, here("Outputs", "flow_counts.rds"))

cat("=== CLEANING COMPLETE ===\n")
cat("Final dataset saved with", nrow(merged_data), "participants and", ncol(merged_data), "columns\n")
cat("CONSORT flow counts saved to flow_counts.rds\n")
