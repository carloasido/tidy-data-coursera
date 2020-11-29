# Set working directory
setwd("../Downloads/Coursera/Data_Science_Foundations_using_R/Getting_and_cleaning_data/week4")

# Load necessary packages
# Tidyverse package is a collection of packages
# which includes dplyr, readr, tidyr, etc.
# install.packages("tidyverse)
library(tidyverse)


# Load variable names from features data------------------------------------------------------

features_df <-
  read_delim("UCI HAR Dataset/features.txt",
    delim = "\n",
    col_names = FALSE
  )


# Load labels data----------------------------------------------------------------------------

labels_df <-
  read_delim("UCI HAR Dataset/activity_labels.txt",
    delim = "\n",
    col_names = FALSE
  ) %>%
  # and convert to lowercase
  mutate_all(tolower) %>%
  # Remove leading row number
  mutate_all(str_remove, pattern = "[\\d]+") %>%
  # then trim leading whitespace
  mutate_all(trimws)


# Load train data-----------------------------------------------------------------------------

# List filenames to load
filenames1 <- list.files("UCI HAR Dataset/train", pattern = "*.txt")

# Apply read_delim function to each train file names and
train_df <- file.path("UCI HAR Dataset/train", filenames1) %>%
  # automatically combines each file as a column in the train_df dataframe
  map_dfc(read_delim, delim = "\n", col_names = FALSE)


# Load test data------------------------------------------------------------------------------

# List filenames to load
filenames2 <- list.files("UCI HAR Dataset/test", pattern = "*.txt")

# Apply read_delim function to each test file names
test_df <- file.path("UCI HAR Dataset/test", filenames2) %>%
  # and automatically combines each file as a column in the test_df dataframe
  map_dfc(read_delim, delim = "\n", col_names = FALSE)


# Merge data and perform tidying--------------------------------------------------------------

merged_data <- train_df %>%
  # Combine train and test df
  bind_rows(test_df) %>%
  # Trim leading and trailing whitespace on the 2nd column
  mutate_at(2, trimws) %>%
  # and split it on at least one whitespace
  separate("X1...2",
    sep = "[\\s]+",
    # while using features data as column names
    into = features_df$X1
  ) %>%
  # Select 1st, 2nd column while renaming them
  select(
    "subject" = X1...1,
    "activity" = X1...3,
    # and select other column names that contains the words 'mean' and 'std'.
    contains(c("mean", "std"))
  ) %>%
  # Convert all columns to numbers(double) except the first two (which already are)
  mutate_at(-(1:2), as.numeric) %>%
  # Map activity labels (2nd column) (using mapvalues from plyr package)
  mutate_at(2, plyr::mapvalues,
    # from numbers to specific activities on labels data
    from = c(1, 2, 3, 4, 5, 6),
    to = labels_df$X1
  ) %>%
  # Convert identifier of the subject (1st column) as a factor
  mutate_at(1, as.factor) %>%
  # and convert activity labels (2nd column) as a factor
  mutate_at(2, factor, levels = labels_df$X1) %>%
  # Remove leading digits on column names except the first two
  rename_with(str_remove, -(1:2), pattern = "[\\d]+") %>%
  # and trim leading whitespace
  rename_with(trimws, -(1:2))


# Generate summary table--------------------------------------------------------------------
  
  merged_data %>%
  group_by(subject, activity) %>%
  summarise_all(mean) %>%
  arrange(subject) %>%
  # write output to a file
  write.table(file = 'tidy_data.txt', row.names = FALSE, quote = FALSE)
