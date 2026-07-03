# =============================================================================
# flow_diagram.R  --  CONSORT 2010 flow diagram for the Hesychastic Prayer RCT
# -----------------------------------------------------------------------------
# Reads flow_counts.rds (written by 01_cleaning.R) for the screening,
# randomization, allocation, T2-return, and QC/analysis counts, AND
# attrition_matrix.rds (written by 03_weekly_processing.R) for the
# "received intervention" row. Renders with the `consort` package.
#
# RUN ORDER: this is a run-at-the-end reporting script. Run 01, 02, and 03
# first (it needs outputs from both 01 and 03), then run this.

#
# BEFORE SUBMISSION: validate the final figure against the official CONSORT
# template (https://www.consort-statement.org) and your target journal.
# =============================================================================

# install.packages("consort")   # run once if not installed
library(consort)
library(here)
library(dplyr)

flow      <- readRDS(here("Outputs", "flow_counts.rds"))               # from 01
attrition <- readRDS(here("Outputs", "attrition_matrix.rds"))          # from 03

# ---- Received intervention: derived from weekly participation ---------------
# A treatment participant "received the intervention" if they completed at least
# one weekly practice. The attrition matrix has one row per enrolled treatment
# participant and one 0/1 column per weekly timepoint; a row-sum > 0 means they
# completed at least one session.
week_cols <- setdiff(names(attrition), c("participantId", "Condition"))
received_by_arm <- attrition %>%
  dplyr::mutate(any_week = rowSums(dplyr::across(dplyr::all_of(week_cols))) > 0) %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(n = sum(any_week), .groups = "drop")

get_received <- function(arm) {
  v <- received_by_arm$n[received_by_arm$Condition == arm]
  if (length(v) == 0) NA_integer_ else v
}

# Control has no weekly practice, so "received intervention" = allocated count.
received_intervention <- list(
  Control        = flow$alloc$Control,
  HPrayer        = get_received("HPrayer"),
  LovingKindness = get_received("LovingKindness")
)

# ---- lost_reasons: standard CONSORT label ----------------------------------
# The counts lost at T2 are derived below from flow$returned_t2. Per-person
# reasons were not recorded in the survey data, so the CONSORT-standard generic
# category "lost to follow-up" is used. If documented reasons become available
# (e.g. withdrew consent, ineligible), replace these with the specific breakdown.
lost_reasons          <- list(Control = "lost to follow-up",
                              HPrayer = "lost to follow-up",
                              LovingKindness = "lost to follow-up")

# ---- Build the exclusion text from captured counts --------------------------
screen_excl <- paste0(
  "Excluded (n = ",
  flow$removed_missing_id + flow$removed_dup_id + flow$removed_age +
    flow$removed_christian + flow$removed_attention + flow$removed_godscreen +
    flow$removed_incomplete, "):\n",
  "\u2022 Missing/duplicate ID (n = ", flow$removed_missing_id + flow$removed_dup_id, ")\n",
  "\u2022 Under 18 (n = ", flow$removed_age, ")\n",
  "\u2022 Non-Christian (n = ", flow$removed_christian, ")\n",
  "\u2022 Failed attention/God screen (n = ",
  flow$removed_attention + flow$removed_godscreen, ")\n",
  "\u2022 Incomplete survey (n = ", flow$removed_incomplete, ")"
)

alloc_excl <- paste0("Excluded before randomization (n = ",
                     flow$removed_dup_assignment + flow$removed_unassigned,
                     "):\n\u2022 Assignment errors")

# ---- Per-arm QC exclusions (Analysis row) -----------------------------------
qc_excl <- sapply(names(flow$pre_qc), function(arm)
  flow$pre_qc[[arm]] - flow$post_qc[[arm]])

# =============================================================================
# OPTION A -- consort_box_grob / build manually with consort package
# The consort package's easiest entry point is consort_plot() on a disposition
# data frame, but for a count-based funnel like this the explicit box approach
# below is clearer. Adjust text as needed.
# =============================================================================

# Build a simple top-down box list. (consort::add_box / add_side_box style.)
g <- add_box(txt = paste0("Intake responses received (n = ", flow$intake_loaded, ")"))

g <- add_side_box(g, txt = screen_excl)

g <- add_box(g, txt = paste0("Eligible (n = ", flow$eligible, ")"))

g <- add_side_box(g, txt = alloc_excl)

g <- add_box(g, txt = paste0("Randomized (n = ", flow$randomized, ")"))

# Split into three arms
g <- add_split(g, txt = c(
  paste0("Control\n(n = ", flow$alloc$Control, ")"),
  paste0("H-Prayer\n(n = ", flow$alloc$HPrayer, ")"),
  paste0("Loving-Kindness\n(n = ", flow$alloc$LovingKindness, ")")
))

# Received allocated intervention (PLACEHOLDER)
g <- add_box(g, txt = c(
  paste0("Received intervention\n(n = ", received_intervention$Control, ")"),
  paste0("Received intervention\n(n = ", received_intervention$HPrayer, ")"),
  paste0("Received intervention\n(n = ", received_intervention$LovingKindness, ")")
))

# Follow-up: returned at T2, with losses
g <- add_box(g, txt = c(
  paste0("Returned at T2 (n = ", flow$returned_t2["Control"], ")\n",
         "Lost: ", flow$alloc$Control - flow$returned_t2["Control"],
         " (", lost_reasons$Control, ")"),
  paste0("Returned at T2 (n = ", flow$returned_t2["HPrayer"], ")\n",
         "Lost: ", flow$alloc$HPrayer - flow$returned_t2["HPrayer"],
         " (", lost_reasons$HPrayer, ")"),
  paste0("Returned at T2 (n = ", flow$returned_t2["LovingKindness"], ")\n",
         "Lost: ", flow$alloc$LovingKindness - flow$returned_t2["LovingKindness"],
         " (", lost_reasons$LovingKindness, ")")
))

# Analysis: per-arm QC exclusions, then analyzed
g <- add_box(g, txt = c(
  paste0("Excluded at QC (n = ", qc_excl["Control"], ")\n",
         "Analyzed (n = ", flow$analyzed$Control, ")"),
  paste0("Excluded at QC (n = ", qc_excl["HPrayer"], ")\n",
         "Analyzed (n = ", flow$analyzed$HPrayer, ")"),
  paste0("Excluded at QC (n = ", qc_excl["LovingKindness"], ")\n",
         "Analyzed (n = ", flow$analyzed$LovingKindness, ")")
))

# ---- Render and save --------------------------------------------------------
plot(g)

# Save to file for the manuscript
ggplot2::ggsave(here("Diagrams","consort_flow.pdf"), plot = build_grid(g),
                width = 10, height = 13)
ggplot2::ggsave(here("Diagrams","consort_flow.png"), plot = build_grid(g),
                width = 10, height = 13, dpi = 300)

cat("CONSORT flow diagram written to consort_flow.pdf / .png\n")
cat("received_intervention derived from weekly participation:\n")
cat("  Control =", received_intervention$Control,
    "| HPrayer =", received_intervention$HPrayer,
    "| LovingKindness =", received_intervention$LovingKindness, "\n")
cat("REMINDER: validate against the official CONSORT template before submission.\n")
