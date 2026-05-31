
# ----------------------------
# 1. LOAD DATA
# ----------------------------

kaggle_data <- read.csv("biomedical_research_abstracts_2024_2026.csv")

# Quick inspection
head(kaggle_data)
str(kaggle_data)
dim(kaggle_data)

# ----------------------------
# 2. BASIC CLEANING
# ----------------------------

kaggle_clean <- kaggle_data

# Remove rows with missing key fields
kaggle_clean <- kaggle_clean[
  !is.na(kaggle_clean$title) &
    !is.na(kaggle_clean$pub_year) &
    !is.na(kaggle_clean$abstract),
]

# Remove duplicate papers (based on PMID)
kaggle_clean <- kaggle_clean[!duplicated(kaggle_clean$pmid), ]

# Remove invalid publication years (safety check)
kaggle_clean <- kaggle_clean[kaggle_clean$pub_year >= 2000, ]

# Remove extremely short abstracts (noise filtering)
kaggle_clean <- kaggle_clean[nchar(kaggle_clean$abstract) > 50, ]

# ----------------------------
# 3. SUMMARY CHECK
# ----------------------------

cat("Original rows:", nrow(kaggle_data), "\n")
cat("Clean rows:", nrow(kaggle_clean), "\n")

# Optional: quick sanity checks
table(kaggle_clean$pub_year)
table(kaggle_clean$country)

# ----------------------------
# 4. EXPORT CLEAN DATA
# ----------------------------

write.csv(kaggle_clean, "kaggle_clean.csv", row.names = FALSE)