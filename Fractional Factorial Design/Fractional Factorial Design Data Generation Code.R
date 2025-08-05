# Load necessary packages
library(dplyr)

# Step 1: Create 2^(4-1) fractional factorial design
design <- expand.grid(
  A = c(-1, 1),  # Training Frequency
  B = c(-1, 1),  # Repetition Range
  C = c(-1, 1)   # Rest Interval
)

# Step 2: Define the generator: D = A * B * C
design$D <- with(design, A * B * C)

# Step 3: Label factor levels (for readability)
design <- design %>%
  mutate(
    TrainingFrequency = ifelse(A == -1, "Low", "High"),
    RepetitionRange   = ifelse(B == -1, "Low", "High"),
    RestInterval      = ifelse(C == -1, "Long", "Short"),
    ExerciseOrder     = ifelse(D == -1, "Large-to-Small", "Small-to-Large")
  )

# Step 4: Add 3 replicates per run
design_replicated <- design[rep(1:nrow(design), each = 3), ]
design_replicated$Replicate <- rep(1:3, times = nrow(design))

# Step 5: Simulate response data (Muscle Endurance)
set.seed(123)

# Simulate normally distributed endurance values with constant variance
design_replicated <- design_replicated %>%
  mutate(
    MuscleEndurance = round(rnorm(
      n = n(),
      mean = 50 +
        5 * A +     # Higher training frequency improves endurance
        3 * B +     # Higher reps improves endurance
        2 * C +     # Short rest slightly improves endurance
        1.5 * D,    # Small-to-large order has small benefit
      sd = 3        # Constant standard deviation
    ), 1)
  )

# View the final dataset
head(design_replicated)

writexl::write_xlsx(design_replicated[,-c(1:4)],"Muscle Endurance Example.xlsx")
