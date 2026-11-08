# =====================================================================
# Christopher McGlynn

# CA2 – Data Analysis: Men's singles at Wimbledon, 2022–2025
#
# Dataset : Jeff Sackmann, tennis_atp
#           https://github.com/JeffSackmann/tennis_atp
#           Files atp_matches_2022.csv to atp_matches_2025.csv
# Structure: one row = one match. Columns starting "winner_" / "w_"
#           for winner, "loser_" / "l_" for loser.
#
# The script has two parts:
#   PART 1 – Data preparation (steps 1 to 10)
#   PART 2 – One hypothesis test for each of the five questions
#
# Every question follows the same five steps:
#   1. State H0 and H1, name the independent (IV) and dependent (DV) variable
#   2. Describe the data (mean, median, variance)
#   3. Check normality – histogram, Q-Q plot and Shapiro-Wilk test
#   4. Choose the test from the variable types and the normality result
#   5. Run the test, compare p with 0.05 and write the conclusion
#
# The questions:
#  1.Do taller players serve more aces?
#  2.Are match winners older or younger than the players they beat?
#  3.Do left-handers win more often than right-handers when they meet?
#  4.Do winners serve fewer double faults than losers?
#  5.Does match duration differ by round?
# =====================================================================

#install.packages(c("lattice","car","rstatix","dplyr","coin"))  # run once only
library(lattice)  # histogram() split by group (Q5)
library(car)      # leveneTest() – checks for equal variances (Q5)
library(rstatix)  # wilcox_effsize() – effect size for the Wilcoxon test (Q4)
library(dplyr)    # group_by() and summarise() – average aces per player (Q1)

# =====================================================================
# PART 1 – DATA PREPARATION
# =====================================================================

# ---- Step 1: Read the dataset ----------------------------
all_matches <- read.csv("Data/atp_matches_2022_2025.csv")
dim(all_matches)   
str(all_matches)   # shows the type of every column
cat("Matches in the full dataset:", nrow(all_matches), "\n") # 11,923 rows (matches) 
cat("Columns in the full dataset:", ncol(all_matches), "\n") # 49 columns

# The file has no year column, so create one from tournament date
all_matches$year <- all_matches$tourney_date %/% 10000
table(all_matches$year)

# ---- Step 2: Keep Wimbledon matches only ----------------------------
wimbledon <- subset(all_matches, tourney_name == "Wimbledon")
dim(wimbledon)          
table(wimbledon$year)  # 127 matches every year
cat("Wimbledon matches:", nrow(wimbledon), "\n") # 508 matches

# These three columns can be dropped as Wimbledon will have the same
# figures for each one and therefore no point including these
table(wimbledon$surface)
table(wimbledon$best_of)
table(wimbledon$tourney_level)

# ---- Step 3: Remove retirements and walkovers -----------------------
# A match that stopped early (RET = retired, W/O = walkover,
# DEF = default)  would effect the results.
# grepl() returns TRUE for every score containing one of those words
retired <- grepl("RET|W/O|DEF", wimbledon$score)
sum(retired)                    
cat("Matches that did not finish:", sum(retired), "\n")  # 17 matches did not finish
wimbledon <- wimbledon[!retired, ]
nrow(wimbledon)                 
cat("Matches left after removing retirements and walkovers:", nrow(wimbledon), "\n")  # 491 matches left


# ---- Step 4: Keep only the variables needed for the five questions --
include_list <- names(wimbledon) %in% c("year", "round", "minutes",
                                        "winner_id", "winner_name", "winner_hand", "winner_ht", "winner_age",
                                        "loser_id", "loser_name", "loser_hand", "loser_ht", "loser_age",
                                        "w_ace", "l_ace", "w_df", "l_df")

wimbledon <- wimbledon[(include_list)]
str(wimbledon)


# ---- Step 5: Missing data ------------------------------------------
# Count the NAs in each column
colSums(is.na(wimbledon))            # one missing height for a winner and one for a loser
# Show the rows that have at least one NA
wimbledon[!complete.cases(wimbledon), ]

# Playing hand uses "U" for unknown instead of NA, so it is not
# picked up by is.na() so had to convert "U" to NA so R treats it as missing.
table(wimbledon$winner_hand)
table(wimbledon$loser_hand)
wimbledon$winner_hand[wimbledon$winner_hand == "U"] <- NA
wimbledon$loser_hand[wimbledon$loser_hand == "U"] <- NA

# na.omit() removes every row with an NA.
# Only 2 matches are affected
wimbledon <- na.omit(wimbledon)
nrow(wimbledon)                      

# ---- Step 6: Rename and convert variable types ---------------------
# I changed minutes to match minutes
names(wimbledon)[names(wimbledon) == "minutes"] <- "match_minutes"

# Round is read in as text (character). It is ORDINAL data - it has a natural order
# (first round -> final), so convert it to an ORDERED factor
wimbledon$round <- factor(wimbledon$round, order = TRUE,
                          levels = c("R128", "R64", "R32", "R16", "QF", "SF", "F"))
table(wimbledon$round)   # QF = 15, SF = 7, F = 4 – too few to test on their own so have to make amendments

# Group the quarter-finals, semi-finals and final into one level, "QF+",
# so that every group has enough matches for Q5 (26 matches)
wimbledon$round_group <- as.character(wimbledon$round)
wimbledon$round_group[wimbledon$round %in% c("QF", "SF", "F")] <- "QF+"
wimbledon$round_group <- factor(wimbledon$round_group, order = TRUE,
                                levels = c("R128", "R64", "R32", "R16", "QF+"))
table(wimbledon$round_group)

# Playing hand is NOMINAL data – the categories have no natural order –
# so convert it to a plain (unordered) factor
wimbledon$winner_hand <- factor(wimbledon$winner_hand, levels = c("R", "L"),
                                labels = c("Right", "Left"))
wimbledon$loser_hand  <- factor(wimbledon$loser_hand,  levels = c("R", "L"),
                                labels = c("Right", "Left"))

# ---- Step 7: New variables for the paired questions ----------------
# Q2 and Q4 compare the winner and loser of the SAME match, so they
# are paired. A paired test works on the difference within each match.
# A negative value here means the winner had less than the loser.
wimbledon$age_diff <- wimbledon$winner_age - wimbledon$loser_age   # Q2 Age
wimbledon$df_diff  <- wimbledon$w_df - wimbledon$l_df              # Q4


# ---- Step 8: Restructure to one row per player per match -----------
# At the moment each row in the spreadsheet holds TWO players.
# I Split every match into a winner row and a loser row with the same
# column names.
# This allows us to examine player statistics by match result.

winners <- data.frame(year = wimbledon$year, round = wimbledon$round,
                      height = wimbledon$winner_ht, age = wimbledon$winner_age,
                      hand = wimbledon$winner_hand, aces = wimbledon$w_ace,
                      double_faults = wimbledon$w_df, result = "Won")
losers  <- data.frame(year = wimbledon$year, round = wimbledon$round,
                      height = wimbledon$loser_ht, age = wimbledon$loser_age,
                      hand = wimbledon$loser_hand, aces = wimbledon$l_ace,
                      double_faults = wimbledon$l_df, result = "Lost")

# rbind() stacks the winner and loser rows
players <- rbind(winners, losers)     # 489 x 2 = 978 rows

# result is a new categorical variable (Won / Lost) -> factor
players$result <- factor(players$result, levels = c("Won", "Lost"))
str(players)

# ---- Step 9: Descriptive statistics --------------------------------
# Mean, median, variance and standard deviation of each numeric
# variable. sapply() applies the same function to every column,
# and data.frame() puts the four results side by side in one table.
num_vars <- players[, c("height", "age", "aces", "double_faults")]
desc_stats <- data.frame(Mean     = sapply(num_vars, mean),
                         Median   = sapply(num_vars, median),
                         Variance = sapply(num_vars, var),
                         SD       = sapply(num_vars, sd))
round(desc_stats, 2)   # round() to 2 decimal places so it is easier to read

# For aces, the mean (10.6) is higher than the median (9). A few matches
# with very high ace counts (up to 54) pull the mean up, while the median
# is not affected by extreme values.
#
# For height (187.2 v 188) and age (27.1 v 26.7) the mean and median
# are close, which suggests a more symmetrical spread.
#
# Note: These statistics describe individual player-match observations.
# Q1 uses a separate dataset containing average aces per player.
# player_aces: one row per player (237 players), with aces
# averaged across all their Wimbledon matches, so every player
# counts once and the observations are independent.

# Match duration is stored in the wimbledon data frame (one row per match),
# not in players, so its mean, median, variance and standard deviation
# are calculated separately here.
mean(wimbledon$match_minutes); median(wimbledon$match_minutes)
var(wimbledon$match_minutes);  sd(wimbledon$match_minutes)

# Categorical variables are described with counts
table(players$hand)          # 835 right-handed, 143 left-handed
table(players$result)        # 489 won, 489 lost
table(wimbledon$round_group)


# ---- Step 10: Charts -----------------------------------
# Charts related to the data
hist(wimbledon$match_minutes, col = "blue", main = "Match duration",
     xlab = "Minutes")
boxplot(match_minutes ~ round_group, data = wimbledon, col = "blue",
        main = "Match duration by round", xlab = "Round", ylab = "Minutes")
barplot(table(players$hand), col = "blue", main = "Playing hand",
        ylab = "Player-matches")

# =====================================================================
# PART 2 – HYPOTHESIS TESTS
# For every test:
# p <= 0.05 -> reject H0
# p > 0.05  -> fail to reject H0
# =====================================================================

# =====================================================================
# Q1 – Do taller players serve more aces?
# =====================================================================

# H0: There is no correlation between height and average aces
#     served per match (rho = 0)
# H1: There is a correlation between height and average aces
#     served per match (rho != 0)
# Two-tailed, alpha = 0.05
#
# Variables:
#   height   – INDEPENDENT variable, continuous numeric (cm)
#   avg_aces – DEPENDENT variable, numeric (average aces per match)
#   Both are numeric variables, so no conversion is required.
#
# Each player will only appear once in the analysis.
# Average aces are calculated across all their Wimbledon matches.

# ---------------------------------------------------------------
# Create one dataset containing all players
# ---------------------------------------------------------------

# Create one row for every winner
q1_winners <- data.frame(
  player_id = wimbledon$winner_id,
  player_name = wimbledon$winner_name,
  height = wimbledon$winner_ht,
  aces = wimbledon$w_ace
)

# Create one row for every loser
q1_losers <- data.frame(
  player_id = wimbledon$loser_id,
  player_name = wimbledon$loser_name,
  height = wimbledon$loser_ht,
  aces = wimbledon$l_ace
)

# Combine winners and losers
q1_player_matches <- rbind(q1_winners, q1_losers)

# Calculate average aces per player across all matches
player_aces <- q1_player_matches %>%
  group_by(player_id) %>%
  summarise(
    player_name = first(player_name),
    height = first(height),
    avg_aces = mean(aces),
    matches = n(),
    .groups = "drop"
  )

# Examine the new dataset
View(player_aces)
str(player_aces)   # 237 players
plot(player_aces$height, player_aces$avg_aces, pch = 19, col = "blue",
     main = "Height vs average aces", xlab = "Height (cm)",
     ylab = "Average aces per match")


# ---------------------------------------------------------------
# Statistics
# ---------------------------------------------------------------

summary(player_aces)

mean(player_aces$height)       # 186.5
median(player_aces$height)     # 185
var(player_aces$height)        # 46.7

mean(player_aces$avg_aces)     # 9.6
median(player_aces$avg_aces)   # 8.3
var(player_aces$avg_aces)      # 32.7


# ---------------------------------------------------------------
# Normality: visual checks
# ---------------------------------------------------------------

par(mfrow = c(1, 2))

hist(player_aces$height, col = "blue",
     main = "Player height", xlab = "Height (cm)")

hist(player_aces$avg_aces, col = "blue",
     main = "Average aces per match", xlab = "Average aces")

# Q-Q plots

qqnorm(player_aces$height, main = "Q-Q plot – Height")
qqline(player_aces$height, col = "red")

qqnorm(player_aces$avg_aces, main = "Q-Q plot – Average aces")
qqline(player_aces$avg_aces, col = "red")

par(mfrow = c(1, 1))


# ---------------------------------------------------------------
# Normality: Shapiro-Wilk tests
# ---------------------------------------------------------------

# H0: Data is normally distributed
# H1: Data is not normally distributed

shapiro.test(player_aces$height)     # W = 0.969, p < 0.001 -> reject H0 -> not normal
shapiro.test(player_aces$avg_aces)   # W = 0.935, p < 0.001 -> reject H0 -> not normal

# DECISION: two numeric variables, neither normally distributed
#   -> SPEARMAN'S RANK CORRELATION


# ---------------------------------------------------------------
# Statistical test: Spearman's rank correlation
# ---------------------------------------------------------------

# exact = FALSE because there are tied values (many players share a height)
cor.test(player_aces$height, player_aces$avg_aces,
         method = "spearman", exact = FALSE)
#   rho = 0.531, p < 2.2e-16 -> p <= 0.05 -> REJECT H0

# Interpreting rho:
# Positive = taller players tend to serve more aces.
# Negative = taller players tend to serve fewer aces.
#
# Strength:
# |rho| > 0.7 = strong
# 0.3–0.7 = moderate
# |rho| < 0.3 = weak

# CONCLUSION: Spearman's rank correlation found a significant
# moderate positive correlation between player height and average
# aces served per match, rho = 0.53, p < 0.001 (237 players).
# Therefore, H0 is rejected. Taller players tend to serve more
# aces per match.

# =====================================================================
# Q2 – Are match winners older or younger?
# =====================================================================
# H0: The median age difference (winner – loser) is zero
# H1: The median age difference is not zero
# Two-tailed, alpha = 0.05
#
# Variables:
#   result – INDEPENDENT variable, categorical (Won / Lost)
#   age    – DEPENDENT variable, continuous numeric (years)
#   The two ages come from the same match -> PAIRED samples,
#   so we test the difference age_diff created in step 7 above.

# --- Describe the groups ---
# tapply() splits age by result and applies the function to each group
tapply(players$age, players$result, summary)   # medians: won 26.6, lost 26.8
tapply(players$age, players$result, var)       # variances: won 21.4, lost 18.8

# --- Normality ---
# For a paired test, normality is checked on the DIFFERENCES,
# because the test only uses the differences.
hist(wimbledon$age_diff, col = "blue", main = "Age difference (winner – loser)",
     xlab = "Years")
qqnorm(wimbledon$age_diff); qqline(wimbledon$age_diff, col = "red")
shapiro.test(wimbledon$age_diff)   # W = 0.994, p = 0.038 -> reject H0 -> not normal
# Visually looks close to normal,but just fails the formal test

# DECISION: paired numeric data, differences not normal
#   -> WILCOXON SIGNED-RANK TEST 

# --- Run the test ---
# paired = TRUE tells R the two columns belong to the same match
wilcox.test(wimbledon$winner_age, wimbledon$loser_age, paired = TRUE, exact = FALSE)
#   V = 56120, p = 0.546 -> p > 0.05 -> FAIL TO REJECT H0

# Check: the paired t-test gives the same decision
t.test(wimbledon$winner_age, wimbledon$loser_age, paired = TRUE)
#   mean difference -0.06 years, t(488) = -0.20, p = 0.839 -> same conclusion
#
# CONCLUSION: The Wilcoxon signed-rank test found no significant
# difference in age between match winners and losers (median 26.6
# versus 26.8 years), V = 56120, p = 0.546.
# Therefore, H0 is not rejected. There is insufficient statistical
# evidence to suggest that match winners and their opponents differ
# in age.


# =================================================================================
# Q3 – Do left-handers win more often than right-handers?
# =================================================================================

# H0: Left-handed and right-handed players are equally likely to win
#     when facing each other.
# H1: Left-handed and right-handed players are not equally likely to win.
# Two-tailed, alpha = 0.05
#
# Variables:
#   hand   – INDEPENDENT variable, categorical / nominal (Left / Right)
#   result – DEPENDENT variable, categorical (Won / Lost)
#   Each left-v-right match has one winner, so the test counts the
#   playing hand of the winner (winner_hand) in each match.
#   Only matches involving one left-handed and one right-handed
#   player are included.
#   Normality does not apply to categorical variables.


# --- Separate the left-v-right matches ---
mixed <- subset(wimbledon, winner_hand != loser_hand)
nrow(mixed)   # 129 matches


# --- Describe: Count the playing hand of match winners ---

# Each match contributes one observation: the playing hand of the winner
winner_hand_counts <- table(mixed$winner_hand)

winner_hand_counts               # Right: 67, Left: 62
prop.table(winner_hand_counts)   # Right: 52%, Left: 48%

barplot(winner_hand_counts, col = "blue",
        main = "Playing hand of match winners",
        xlab = "Playing hand", ylab = "Number of matches")


# DECISION: One categorical variable with two possible outcomes.
# Compare the observed counts against an expected 50:50 distribution.
#   -> CHI-SQUARED GOODNESS-OF-FIT TEST

# --- Run the test ---
# p = c(0.5, 0.5) sets the expected split: if hand made no difference,
# each hand would win half of the left-v-right matches
chi_result <- chisq.test(winner_hand_counts, p = c(0.5, 0.5))

chi_result
#   X-squared = 0.194, df = 1, p = 0.660
#   p > 0.05 -> FAIL TO REJECT H0


# CONCLUSION: The chi-squared test found no significant
# difference between the number of left-handed and right-handed winners
# in matches between players of different playing hands (62 versus 67),
# X-squared(1) = 0.19, p = 0.660.
# Therefore, H0 is not rejected. There is insufficient statistical
# evidence to suggest that left-handed players are more likely to win
# than right-handed players.

# =====================================================================
# Q4 – Do winners serve fewer double faults than losers?
# =====================================================================
# H0: The median difference in double faults (winner - loser) is zero
# H1: The median difference in double faults (winner - loser) is less than zero
# ONE-tailed, alpha = 0.05
#
# Variables:
#   result        – INDEPENDENT variable, categorical (Won / Lost), paired
#   double_faults – DEPENDENT variable,  numeric count
#   Paired within each match, so we test df_diff from step 7.

# --- Describe the groups ---
tapply(players$double_faults, players$result, summary)   # medians: won 3, lost 4
tapply(players$double_faults, players$result, var)       # variances: won 8.1, lost 9.4

# --- Normality of the differences ---
hist(wimbledon$df_diff, col = "blue",
     main = "Double-fault difference (winner – loser)", xlab = "Double faults")
qqnorm(wimbledon$df_diff); qqline(wimbledon$df_diff, col = "red")
shapiro.test(wimbledon$df_diff)   # W = 0.988, p < 0.001 -> reject H0 -> not normal

# DECISION: paired counts, differences not normal
#   -> WILCOXON SIGNED-RANK TEST, one-tailed

# --- Run the test ---
# alternative = "less" tests whether winner double faults are LOWER
# than loser double faults (the first column minus the second is < 0)
wilcox.test(wimbledon$w_df, wimbledon$l_df, paired = TRUE,
            alternative = "less", exact = FALSE)
#   V = 35762, p = 2.97e-07 -> p < 0.05 -> REJECT H0


df_long <- data.frame(result = players$result, double_faults = players$double_faults)
wilcox_effsize(df_long, double_faults ~ result, paired = TRUE)   # r = 0.23, small

# How often did the winner serve fewer (-1), the same (0) or more (1)?
table(sign(wimbledon$df_diff))   # fewer: 269, same: 46, more: 174
#
# CONCLUSION: Winners serve significantly fewer double faults than
# losers (median 3 v 4), V = 35762, p < 0.001, r = 0.23 (small effect).


# =====================================================================
# Q5 – Does match duration differ by round?
# =====================================================================
# H0: Match duration is the same in every round
# H1: Match duration differs in at least one round
# alpha = 0.05
#
# Variables:
#   round_group   – INDEPENDENT variable, ordered factor with 5 levels
#                   (converted in step 6)
#   match_minutes – DEPENDENT variable, continuous numeric (minutes)

# --- Describe the groups ---
tapply(wimbledon$match_minutes, wimbledon$round_group, summary)
tapply(wimbledon$match_minutes, wimbledon$round_group, var)

# --- Normality WITHIN each group ---
histogram(~ match_minutes | round_group, data = wimbledon, layout = c(5, 1),
          main = "Match duration by round", xlab = "Minutes")
tapply(wimbledon$match_minutes, wimbledon$round_group, shapiro.test)
#   R128 p < 0.001, R64 p = 0.106, R32 p = 0.005, R16 p = 0.064, QF+ p = 0.482
#   R128 and R32 are NOT normal

# --- Equal variances: Levene's test ---
# H0: variances are equal   H1: variances are not equal
leveneTest(match_minutes ~ round_group, data = wimbledon, center = mean)
#   F(4, 484) = 1.03, p = 0.391 -> fail to reject H0 -> variances equal

# DECISION: numeric DV, categorical IV with 5 independent groups,
# two groups not normal -> KRUSKAL-WALLIS TEST 

# --- Run the test ---
kruskal.test(match_minutes ~ round_group, data = wimbledon)
#   chi-squared = 6.38, df = 4, p = 0.173 -> p > 0.05 -> FAIL TO REJECT H0

# CONCLUSION: Median duration rises from 148 minutes in the first round
# to 182 in the fourth round, but the difference between rounds is not
# significant, chi-squared(4) = 6.38, p = 0.17.