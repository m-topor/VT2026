# Import datafile
library(readr)
allotment_owners <- read_csv("allotment_owners.csv")


# View data
View(allotment_owners)
skimr::skim(allotment_owners)

#Remove missing rows
library(dplyr)
df_complete <- allotment_owners %>%
  filter(!is.na(MH_6), !is.na(Nat_2))

skimr::skim(df_complete)

# Mutate variables (total sums MH + Nat)
df_totals <- df_complete %>%
  mutate(MH_total = rowSums(across(starts_with("MH"))),
  Nat_total = rowSums(across(starts_with("Nat")))
  )
skimr::skim(df_totals)

# View mean differences between allotment owners and non-owners
df_totals %>%
  group_by(D_allot) %>%
  summarise(mean = mean(D_Age))
df_totals %>%
  group_by(D_allot) %>%
  summarise(mean = mean(D_hours))
df_totals %>%
  group_by(D_allot) %>%
  summarise(mean = mean(SE_1))
df_totals %>%
  group_by(D_allot) %>%
  summarise(mean = mean(MH_total))
df_totals %>%
  group_by(D_allot) %>%
  summarise(mean = mean(Nat_total))

# Here is where I would have checked assumptions if I had time :)

# Relationship between nature connection and time spent in nature
cor(df_totals$Nat_total, df_totals$D_hours)
plot(df_totals$Nat_total, df_totals$D_hours)

# Compare nature connection between allotment owners and non-owners
ttest_nat <- t.test(Nat_total ~ D_allot, data = df_totals)
t.test(Nat_total ~ D_allot, data = df_totals)
library(effsize)
cohen.d(Nat_total ~ D_allot, data = df_totals)
