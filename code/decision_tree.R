
#install.packages("rpart")
#install.packages("rpart.plot")
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# 1. Load the cleaned dataset
df <- read_csv("biomed_clean_papers.csv")

# 2. Select predictor columns
df2 <- df %>%
  select(
    year,
    referencecount,
    influentialcitationcount,
    isopenaccess,
    venue,
    journal_name,
    s2fieldsofstudy
  )

# 3. Creating supervised label (high_cited vs low_cited)
df2 <- df2 %>%
  mutate(label = ifelse(df$citationcount > median(df$citationcount, na.rm = TRUE),
                        "high_cited", "low_cited")) %>%
  mutate(label = as.factor(label))

# 4. Converting categorical variables to factors
df2 <- df2 %>%
  mutate(
    isopenaccess = as.factor(isopenaccess),
    venue = as.factor(venue),
    journal_name = as.factor(journal_name),
    s2fieldsofstudy = as.factor(s2fieldsofstudy)
  )

# 5 Lumping high-cardinality factors
library(forcats)

df2 <- df2 %>%
  mutate(
    journal_name = fct_lump(journal_name, n = 20),
    venue = fct_lump(venue, n = 20),
    s2fieldsofstudy = fct_lump(s2fieldsofstudy, n = 20)
  )
df2 <- df2 %>% drop_na()

# 6. Creating train/test split
set.seed(123)
train_index <- createDataPartition(df2$label, p = 0.8, list = FALSE)
train <- df2[train_index, ]
test  <- df2[-train_index, ]

# 7. Training three decision trees 

# Tree 1: Gini
dt1 <- rpart(
  label ~ ., 
  data = train, 
  method = "class",
  control = rpart.control(
    cp = 0.02,
    minsplit = 40,
    maxdepth = 3
  )
)

# Tree 2: Entropy
train_small <- train %>%
  select(
    year,
    referencecount,
    influentialcitationcount,
    isopenaccess,
    label
  )

dt2 <- rpart(
  label ~ .,
  data = train_small,
  method = "class",
  parms = list(split = "information"),
  control = rpart.control(
    cp = 0.0005,
    minsplit = 20,
    maxdepth = 6
  )
)

cp <- dt2$cptable[which.min(dt2$cptable[,"xerror"]), "CP"]
dt2p <- prune(dt2, cp = cp)
summary(dt2)

# Tree 3: Pruned dt1

train <- train %>%
  mutate(
    venue = fct_lump(venue, n = 5),
    journal_name = fct_lump(journal_name, n = 10),
    s2fieldsofstudy = fct_lump(s2fieldsofstudy, n = 10)
  )

dt3 <- rpart(
  label ~ .,
  data = train,
  method = "class",
  control = rpart.control(cp = 0.001)
)
best_cp <- dt3$cptable[which.min(dt3$cptable[,"xerror"]), "CP"]
best_cp <- max(best_cp, 0.005)   # prevents stump collapse

# 8. Plotting trees
par(mfrow = c(1,1))

prp(dt1, main = "Decision Tree 1 (Gini)",
    type = 1, extra = 1, cex = 0.9,
    fallen.leaves = TRUE, varlen = -6, faclen = 2)

par(xpd = NA, mar = c(6, 4, 4, 4)) # Expaning margin

dt2$frame$var <- gsub("referencecount", "ref", dt2$frame$var)
dt2$frame$var <- gsub("influentialcitationcount", "inflcit", dt2$frame$var)

prp(
  dt2p,
  main = "Decision Tree 2 (Entropy)",
  type = 2,
  extra = 104,
  under = TRUE,
  cex = 0.65,       
  faclen = 0,
  varlen = 0,
  fallen.leaves = TRUE,
  box.palette = "Blues",
  branch.col = "gray40",
  branch.lty = 1,
  shadow.col = "gray85",
)

# Shortening labels
dt3p <- prune(dt3, cp = best_cp)

shorten_splits <- function(x, labs, digits, varlen, faclen) {
  labs <- gsub("Biochemical and Biophysical Research Communications - BBRC", "BBRC", labs)
  labs <- gsub("Journal of Biological Chemistry", "JBC", labs)
  labs <- gsub("Journal of Immunology", "JI", labs)
  labs <- gsub("Journal of the American Chemical Society", "JACS", labs)
  labs <- gsub("Physical Review Letters", "PRL", labs)
  labs
}

# Cleaning variable names
dt3p$frame$var <- gsub("journal_name", "journal", dt3p$frame$var)
dt3p$frame$var <- gsub("s2fieldsofstudy", "field", dt3p$frame$var)
dt3p$frame$var <- gsub("venue", "venue", dt3p$frame$var)

# Plotting the pruned tree
prp(
  dt3p,
  main = "Decision Tree 3 (Pruned)",
  type = 1,
  extra = 104,
  under = TRUE,
  cex = 0.75,
  split.cex = 0.6,
  faclen = 0,
  varlen = 0,
  fallen.leaves = TRUE,
  box.palette = "Greens",
  branch.col = "gray40",
  branch.lty = 1,
  shadow.col = "gray85",
  split.fun = shorten_splits
)

# 9. Predictions, confusion Matrix, and accuracy
pred_dt <- predict(dt1, test, type = "class")
results_dt <- confusionMatrix(pred_dt, test$label)
print(results_dt)

# 10. Confusion matrix heatmap
cm <- as.data.frame(results_dt$table)

ggplot(cm, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "#6baed6", high = "#08519c") +
  labs(title = "Decision Tree Confusion Matrix",
       x = "Actual Label",
       y = "Predicted Label") +
  theme_minimal()

# 11. Accuracy vs no-information rate plot
acc <- results_dt$overall["Accuracy"]
nir <- results_dt$overall["AccuracyNull"]

acc_df <- data.frame(
  Metric = c("Model Accuracy", "No-Information Rate"),
  Value = c(acc, nir)
)

ggplot(acc_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", Value * 100)),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#3182bd", "#9ecae1")) +
  ylim(0, 1) +
  labs(title = "Decision Tree Accuracy vs. No-Information Rate",
       y = "Value",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# 12. Feature Importance
importance <- dt1$variable.importance
print(importance)
