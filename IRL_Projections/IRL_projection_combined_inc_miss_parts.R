# Load the required libraries 
library(rjags)
library(tidyverse)
library(dplyr)

# Load the contacts dataset 
contact_common <- read.csv("2008_Mossong_POLYMOD_contact_common.csv")

# Participant's dataset
participant_common <- read.csv("2008_Mossong_POLYMOD_participant_common.csv")

# Participant extra 
participant_extra <- read.csv("2008_Mossong_POLYMOD_participant_extra.csv")

# Household common dataset 
hh_common <- read.csv("2008_Mossong_POLYMOD_hh_common.csv")

# Match household common with participant common 
household_lookup <- merge(participant_common[c("part_id","hh_id")], hh_common)

# Merge the tables 
df_participant_temp <- merge(participant_common,participant_extra,by = "part_id")
df <- merge(df_participant_temp,contact_common, by = "part_id")
df <- merge(df,household_lookup, by = c("part_id","hh_id"))


# Calculate estimated age by taking the average of cnt_age_est_min and cnt_age_est_max 
avg_est_age <- ifelse(!is.na(df[["cnt_age_est_min"]]) & !is.na(df[["cnt_age_est_max"]]), (df[["cnt_age_est_min"]] + df[["cnt_age_est_max"]]) / 2,
                      ifelse(is.na(df[["cnt_age_est_min"]]) & !is.na(df[["cnt_age_est_max"]]), df[["cnt_age_est_max"]],
                             ifelse(!is.na(df[["cnt_age_est_min"]]) & is.na(df[["cnt_age_est_max"]]), df[["cnt_age_est_min"]],
                                    NA)))


df[["contact_age"]] <- ifelse(is.na(df[["cnt_age_exact"]]),avg_est_age,df[["cnt_age_exact"]])

df$contact_age <- as.integer(df$contact_age)

# Remove rows with missing participant and contact ages 
df <- df[!is.na(df$part_age),]
df <- df[!is.na(df$contact_age),]

df_org <- df 


################################################################ OTHER CATEGORY ####################################################
# Create the other dataset 

df_other = df[df$cnt_other == 1,]
#df_leisure = df[df$cnt_leisure == 1,]
#df_transport = df[df$cnt_transport == 1,]

#df_other <- rbind(df_other,df_leisure)
#df_other <- rbind(df_other,df_transport)


# Define age brackets
age_brackets <- cut(df_other$contact_age, 
                    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 100), 
                    #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                    
                    labels = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                               "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"), 
                    right = FALSE)


# Add the age_brackets to the data
data <- df_other %>%
  mutate(age_bracket = age_brackets)

age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

data <- data %>%
  left_join(age_bracket_to_bucket, by = "age_bracket")


# Create a pivot table with counts of contacts per age bracket for each participant
output <- data %>%
  group_by(part_id,part_age, age_bracket) %>%
  summarise(contact_count = n()) %>%
  spread(age_bracket, contact_count, fill = 0)


output_other <- output %>%
  select("part_id","part_age",
         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
         "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16")


# Define age brackets and bucket numbers for participant age
age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

output_other <- output_other %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))


# Include those participant IDs as well that haven't reported contacts at other category
length(setdiff(df$part_id,output_other$part_id))

# Create a dataset with these missing participant_ids 
output_other_missing <- as.data.frame(setdiff(df$part_id,output_other$part_id))
colnames(output_other_missing) <- "part_id"
# Add columns for participant_age and participant_age_bracket 
output_other_missing <- left_join(output_other_missing,df[c("part_id","part_age")], by = "part_id")
output_other_missing <- output_other_missing %>% distinct()
output_other_missing[paste0("X", 1:16)] <- 0
# Add column for participant_age_bracket 
output_other_missing <- output_other_missing %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))


output_other <- rbind(output_other,output_other_missing)
output_other <- output_other %>% arrange(part_id)
output_other<- output_other[!is.na(output_other$part_id),]

rm(contact_common)
rm(df_participant_temp)
rm(hh_common)
rm(participant_common)
rm(participant_extra)
rm(age_bracket_to_bucket)
rm(data)
rm(df_leisure)
rm(df_transport)
rm(household_lookup)
rm(output)

############################################### HOUSEHOLD DATA ######################################################################


df_home = df[df$cnt_home == 1,]

# Define age brackets
age_brackets <- cut(df_home$contact_age, 
                    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 100), 
                    #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                    
                    labels = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                               "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"), 
                    right = FALSE)


# Add the age_brackets to the data
data <- df_home %>%
  mutate(age_bracket = age_brackets)

age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

data <- data %>%
  left_join(age_bracket_to_bucket, by = "age_bracket")


# Create a pivot table with counts of contacts per age bracket for each participant
output <- data %>%
  group_by(part_id,part_age,age_bracket) %>%
  summarise(contact_count = n()) %>%
  spread(age_bracket, contact_count, fill = 0)


output_home <- output %>%
  select("part_id","part_age",
         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
         "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16")


# Dataset containing the household information of the people living in the households 
hh_extra <- read.csv("2008_Mossong_POLYMOD_hh_extra.csv")

# Function to get non-missing values from a row (for household members)
get_non_missing_ages <- function(row) {
  # Exclude household_id and remove NAs
  non_missing_ages <- as.numeric(row[!is.na(row)])
  return(non_missing_ages)
}

# Apply the function row-wise using lapply
result_list <- lapply(1:nrow(hh_extra), function(i) {
  list(
    household_id = hh_extra$hh_id[i],
    non_missing_ages = get_non_missing_ages(hh_extra[i, 2:ncol(hh_extra)])  # Exclude household_id, use only age columns
  )
})

# View the result
print(result_list)

# Define the age ranges as breaks and labels
age_ranges <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf)
age_labels <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8","X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16")


# Initialize an empty data frame to store results
result_df <- data.frame(household_id = hh_extra$hh_id)

# Add columns for each age range with 0s to start
for (label in age_labels) {
  result_df[[label]] <- 0
}

# Loop through each household and count members in each age range
for (i in 1:length(result_df$household_id)) {
  print(i)
  ages <- result_list[[i]]$non_missing_ages  # Get ages for the household
  age_counts <- table(cut(ages, breaks = age_ranges, labels = age_labels, right = FALSE))  # Count ages in ranges
  
  # Update the result_df with counts for each range
  for (label in age_labels) {
    result_df[i, label] <- age_counts[[label]]
  }
}

# View the final result
print(result_df)
colnames(result_df) <- c("hh_id","participant_v_alpha_1", "participant_v_alpha_2", "participant_v_alpha_3",
                         "participant_v_alpha_4","participant_v_alpha_5","participant_v_alpha_6","participant_v_alpha_7",
                         "participant_v_alpha_8","participant_v_alpha_9","participant_v_alpha_10","participant_v_alpha_11",
                         "participant_v_alpha_12","participant_v_alpha_13","participant_v_alpha_14","participant_v_alpha_15",
                         "participant_v_alpha_16" )

result_df <- left_join(result_df,distinct(df_org[,c("part_id","hh_id")]),by="hh_id")

output_home <- left_join(output_home,result_df,by="part_id")

# Define age brackets and bucket numbers for participant age
age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

output_home <- output_home %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))



# Add the missing participants information 
# Include those participant IDs as well that haven't reported contacts at home category
length(setdiff(df$part_id,output_home$part_id))

# Create a dataset with these missing participant_ids 
output_home_missing <- as.data.frame(setdiff(df$part_id,output_home$part_id))
colnames(output_home_missing) <- "part_id"
# Add columns for participant_age and participant_age_bracket 
output_home_missing <- left_join(output_home_missing,df[c("part_id","part_age")], by = "part_id")
output_home_missing <- output_home_missing %>% distinct()
output_home_missing[paste0("X", 1:16)] <- 0
# Add information on household age structures 
output_home_missing <- left_join(output_home_missing,result_df,by = "part_id")

# Add column for participant_age_bracket 
output_home_missing <- output_home_missing %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))


output_home <- rbind(output_home,output_home_missing)
output_home <- output_home %>% arrange(part_id)
output_home <- output_home[!is.na(output_home$part_id),]

rm(age_bracket_to_bucket)
rm(data)
rm(hh_extra)
rm(output)
rm(result_df)
rm(result_list)
rm(age_brackets)
rm(age_counts)
rm(age_labels)
rm(age_ranges)
rm(ages)
rm(avg_est_age)
rm(i)
rm(label)
rm(get_non_missing_ages)

##################################################### SCHOOL ###################################################################

df_school <- df[df$cnt_school == 1,]

# Load dataset containing information on when the diary was filled out 
df_sday <- read.csv("2008_Mossong_POLYMOD_sday.csv")
colnames(df_sday) <- c("part_id" ,  "sday_id" ,  "day" ,      "month"  ,   "year" ,     "dayofweek")

df_school <- left_join(df_school,df_sday, by = "part_id")

# If the day of the week is 0 or 6 , then w_i vector is 0 otherwise it is 1 
#df_school$s_i <- ifelse(df_school$dayofweek == 0 | df_school$dayofweek == 6, 0, 1)
df_school$s_i <- 1

# Define age brackets
age_brackets <- cut(df_school$contact_age, 
                    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 100), 
                    labels = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                               "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"), 
                    right = FALSE)

# Add the age_brackets to the data
data <- df_school %>%
  mutate(age_bracket = age_brackets)

age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

data <- data %>%
  left_join(age_bracket_to_bucket, by = "age_bracket")


# Create a pivot table with counts of contacts per age bracket for each participant
output <- data %>%
  group_by(part_id, part_age, age_bracket) %>%
  summarise(contact_count = n()) %>%
  spread(age_bracket, contact_count, fill = 0)


output_school <- output %>%
  select("part_id","part_age",
         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
         "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16")

df_match <- unique(df_school[c("part_id","s_i")])
output_school <- left_join(output_school,df_match,by = "part_id")
output_school <- output_school[!is.na(output_school$s_i),]


# Define age brackets and bucket numbers for participant age
age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

output_school <- output_school %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))



# Add the missing participant_ids 
# Add the missing participants information 
# Include those participant IDs as well that haven't reported contacts at school category
length(setdiff(df$part_id,output_school$part_id))

# Create a dataset with these missing participant_ids 
output_school_missing <- as.data.frame(setdiff(df$part_id,output_school$part_id))
colnames(output_school_missing) <- "part_id"
# Add columns for participant_age and participant_age_bracket 
output_school_missing <- left_join(output_school_missing,df[c("part_id","part_age")], by = "part_id")
output_school_missing <- output_school_missing %>% distinct()
output_school_missing[paste0("X", 1:16)] <- 0
output_school_missing$s_i <- 0

# Add column for participant_age_bracket 
output_school_missing <- output_school_missing %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))


output_school <- rbind(output_school,output_school_missing)
output_school <- output_school %>% arrange(part_id)
output_school <- output_school[!is.na(output_school$part_id),]

rm(age_bracket_to_bucket)
rm(data)
rm(output)
rm(age_brackets)
rm(output_home_missing)
rm(output_other_missing)
rm(output_school_missing)
########################################### WORK ##########################################################

df_work <- df[df$cnt_work == 1,]

# Load dataset that contains information on the day of the week the diary was filled out 
df_sday <- read.csv("2008_Mossong_POLYMOD_sday.csv")
colnames(df_sday) <- c("part_id" ,  "sday_id" ,  "day" ,      "month"  ,   "year" ,     "dayofweek")

df_work <- left_join(df_work,df_sday, by = "part_id")

# If the day of the week is 0 or 6 , then w_i vector is 0 otherwise it is 1 

df_work$w_i <- 1

# Define age brackets
age_brackets <- cut(df_work$contact_age, 
                    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 100), 
                    labels = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                               "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"), 
                    right = FALSE)

# Add the age_brackets to the data
data <- df_work %>%
  mutate(age_bracket = age_brackets)

age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

data <- data %>%
  left_join(age_bracket_to_bucket, by = "age_bracket")


# Create a pivot table with counts of contacts per age bracket for each participant
output <- data %>%
  group_by(part_id, part_age, age_bracket) %>%
  summarise(contact_count = n()) %>%
  spread(age_bracket, contact_count, fill = 0)


output_work <- output %>%
  select("part_id","part_age" ,
         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
         "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16")

df_match <- unique(df_work[c("part_id","w_i")])
output_work <- left_join(output_work,df_match,by = "part_id")
output_work <- output_work[!is.na(output_work$w_i),]

# Define age brackets and bucket numbers for participant age
age_bracket_to_bucket <- data.frame(
  age_bracket = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",
                  "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16"),
  bucket_number = 1:16
)

output_work <- output_work %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))


# Add the missing participants information 
# Include those participant IDs as well that haven't reported contacts at school category
length(setdiff(df$part_id,output_work$part_id))

# Create a dataset with these missing participant_ids 
output_work_missing <- as.data.frame(setdiff(df$part_id,output_work$part_id))
colnames(output_work_missing) <- "part_id"
# Add columns for participant_age and participant_age_bracket 
output_work_missing <- left_join(output_work_missing,df[c("part_id","part_age")], by = "part_id")
output_work_missing <- output_work_missing %>% distinct()
output_work_missing[paste0("X", 1:16)] <- 0
output_work_missing$w_i <- 0

# Add column for participant_age_bracket 
output_work_missing <- output_work_missing %>%
  mutate(participant_age_bracket = cut(part_age, 
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, Inf), 
                                       #breaks = c(0, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 100), 
                                       labels = age_bracket_to_bucket$bucket, 
                                       right = FALSE))


output_work <- rbind(output_work,output_work_missing)
output_work <- output_work %>% arrange(part_id)
output_work <- output_work[!is.na(output_work$part_id),]


rm(age_bracket_to_bucket)
rm(data)
rm(df_match)
rm(output)
rm(output_work_missing)

output_home <- output_home[!is.na(output_home$participant_age_bracket),]
output_other <- output_other[!is.na(output_other$participant_age_bracket),]
output_school <- output_school[!is.na(output_school$participant_age_bracket),]
output_work <- output_work[!is.na(output_work$participant_age_bracket),]

matrix_size <- 16

# Supporting functions
get_adjacent_cells <- function(matrix_size, row, col) {
  adjacent_cells <- list()
  
  if (row > 0) {
    adjacent_cells <- append(adjacent_cells, list(c(row - 1, col)))
  }
  if (row < matrix_size - 1) {
    adjacent_cells <- append(adjacent_cells, list(c(row + 1, col)))
  }
  if (col > 0) {
    adjacent_cells <- append(adjacent_cells, list(c(row, col - 1)))
  }
  if (col < matrix_size - 1) {
    adjacent_cells <- append(adjacent_cells, list(c(row, col + 1)))
  }
  adjacent_cells <- append(adjacent_cells, list(c(row, col)))
  
  return(adjacent_cells)
}

convert_to_cell_numbers <- function(matrix_size, adjacent_cells) {
  cell_numbers <- sapply(adjacent_cells, function(cell) {
    row <- cell[1]
    col <- cell[2]
    return((row * matrix_size + col) + 1)
  })
  return(cell_numbers)
}

neighbour_cells_list <- list()
total_neighbours <- numeric(256)
matrix_size <- 16
count <- 1
for (i in 0:(matrix_size - 1)) {
  for (j in 0:(matrix_size - 1)) {
    adjacent_cells <- get_adjacent_cells(matrix_size, i, j)
    cell_numbers <- convert_to_cell_numbers(matrix_size, adjacent_cells)
    neighbour_cells_list[[count]] <- cell_numbers
    total_neighbours[count] <- length(cell_numbers)
    count <- count + 1
  }
}

max_neighbours <- max(sapply(neighbour_cells_list, length))
neighbour_cells_matrix <- t(sapply(neighbour_cells_list, function(x) c(x, rep(0, max_neighbours - length(x)))))

get_row_cell_numbers <- function(matrix_size, row) {
  cell_numbers <- (row - 1) * matrix_size + 1:matrix_size
  return(cell_numbers)
}


interaction_cells_matrix_other <- matrix(0, nrow = nrow(output_other), ncol = matrix_size)
interaction_cells_matrix_school <- matrix(0, nrow = nrow(output_school), ncol = matrix_size)
interaction_cells_matrix_work <- matrix(0, nrow = nrow(output_work), ncol = matrix_size)
interaction_cells_matrix_home <- matrix(0, nrow = nrow(output_home), ncol = matrix_size)

for(i in 1:nrow(output_other))
{
  interaction_cells_matrix_other[i,] <- get_row_cell_numbers(matrix_size, as.numeric(output_other$participant_age_bracket[i]))
}

for(i in 1:nrow(output_school))
{
  interaction_cells_matrix_school[i,] <- get_row_cell_numbers(matrix_size, as.numeric(output_school$participant_age_bracket[i]))
}

for(i in 1:nrow(output_work))
{
  interaction_cells_matrix_work[i,] <- get_row_cell_numbers(matrix_size, as.numeric(output_work$participant_age_bracket[i]))
}

for(i in 1:nrow(output_home))
{
  interaction_cells_matrix_home[i,] <- get_row_cell_numbers(matrix_size, as.numeric(output_home$participant_age_bracket[i]))
}

# Changing this to the participant_id based model 

unique_participant_ids <- (c(output_home$part_id, output_other$part_id, output_school$part_id, output_work$part_id))
part_id_home <- output_home$part_id
part_id_other <- output_other$part_id
part_id_school <- output_school$part_id
part_id_work <- output_work$part_id

model_string <- "
  model {
    # Define priors for the hierarchical model
    theta ~ dexp(1000)
    delta_home ~ dexp(1000)
    
    for (i in 1:N) {
      sigma[i] ~ dgamma(theta, theta)
    }

    
    for (i in 1:(matrix_size * matrix_size)) {
      epsilon_home[i] ~ dnorm(0,1/10000)
      epsilon_work[i] ~ dnorm(0,1/10000)
      epsilon_school[i] ~ dnorm(0,1/10000)
      epsilon_other[i] ~ dnorm(0,1/10000)
    }
    
    for (i in 1:256){
          lambda_val_home[i] <- exp(sum(epsilon_home[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
          lambda_val_other[i] <- exp(sum(epsilon_other[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
          lambda_val_work[i] <- exp(sum(epsilon_work[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
          lambda_val_school[i] <- exp(sum(epsilon_school[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])

    }


  
  # Likelihood for home contacts 
  for (i in 1:N_home){
    for (j in 1:matrix_size){
      X_home[i,j] ~ dpois((sigma[participant_id_home[i]]*lambda_val_home[interaction_cells_matrix_home[i,j]])*(home_vector[i,j] + delta_home))
    }
  }
  
  # Likelihood for other contacts 
  for (i in 1:N_other){
    for(j in 1:matrix_size){
      X_other[i,j] ~ dpois(sigma[participant_id_other[i]]*lambda_val_other[interaction_cells_matrix_other[i,j]])
    }
  }
  
  # Likelihood for work contacts
  for (i in 1:N_work){
    for (j in 1:matrix_size){
      X_work[i,j] ~ dpois((sigma[participant_id_work[i]]*lambda_val_work[interaction_cells_matrix_work[i,j]])*(work_vector[i]))
    }
  }
  
  # Likelihood for school contacts 
  for(i in 1:N_school){
    for (j in 1:matrix_size){
      X_school[i,j] ~ dpois((sigma[participant_id_school[i]]*lambda_val_school[interaction_cells_matrix_school[i,j]])*(school_vector[i]))
    }
  }
  
  }
"



data_list <- list(
  N = 8001,
  N_home = nrow(output_home),
  participant_id_home = output_home$part_id,
  N_other = nrow(output_other),
  participant_id_other = output_other$part_id,
  N_work = nrow(output_work),
  participant_id_work = output_work$part_id,
  N_school = nrow(output_school),
  participant_id_school = output_school$part_id,
  matrix_size = 16,
  interaction_cells_matrix_other = interaction_cells_matrix_other,
  interaction_cells_matrix_home = interaction_cells_matrix_home,
  interaction_cells_matrix_school = interaction_cells_matrix_school,
  interaction_cells_matrix_work = interaction_cells_matrix_work,
  neighbour_cells_matrix = neighbour_cells_matrix,
  total_neighbours = total_neighbours,
  X_home = as.matrix(output_home[,3:18]),
  X_other = as.matrix(output_other[,3:18]),
  X_work = as.matrix(output_work[,3:18]),
  X_school = as.matrix(output_school[,3:18]),
  home_vector = as.matrix(output_home[,20:35]),
  school_vector = output_school$s_i,
  work_vector = output_work$w_i
)

# Initial values
inits <- function() {
  list(
    theta = 1,
    sigma = rgamma(max(unique_participant_ids), shape = theta_org, rate = theta_org),
    delta_home = 0.05,
    epsilon_home = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000),
    epsilon_work = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000),
    epsilon_school = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000),
    epsilon_other = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000)
  )
}

params <- c("theta", "sigma","delta_home","epsilon_home")

theta_org <- 5 

inits <- list(
  theta = 1,
  sigma = rgamma(max(unique_participant_ids), shape = theta_org, rate = theta_org),
  delta_home = 0.05,
  epsilon_home = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000),
  epsilon_work = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000),
  epsilon_school = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000),
  epsilon_other = rnorm(matrix_size * matrix_size, mean = 0, sd = 1/10000)
)

params <- c("theta", "sigma","delta_home","epsilon_home")

theta_org <- 5 


# Run the MCMC chains in parallel 
# Function to run one chain of the RJAGS model
run_chain_other <- function(chain_id, model_string, data_list, inits) {
  # Initialize the model for each chain
  model <- jags.model(textConnection(model_string), data = data_list, inits = inits, n.chains = 1)
  
  # Sample from the model
  samples <- coda.samples(model = model, variable.names = c("epsilon_other"),n.iter = 5000, thin = 50)
  
  return(samples)
}


run_chain_school <- function(chain_id, model_string, data_list, inits) {
  # Initialize the model for each chain
  model <- jags.model(textConnection(model_string), data = data_list, inits = inits, n.chains = 1)
  
  # Sample from the model
  samples <- coda.samples(model = model, variable.names = c("epsilon_school"),n.iter = 5000, thin = 50)
  
  return(samples)
}

run_chain_work <- function(chain_id, model_string, data_list, inits) {
  # Initialize the model for each chain
  model <- jags.model(textConnection(model_string), data = data_list, inits = inits, n.chains = 1)
  
  # Sample from the model
  samples <- coda.samples(model = model, variable.names = c("epsilon_work"),n.iter = 5000, thin = 50)
  
  return(samples)
}

run_chain_home <- function(chain_id, model_string, data_list, inits) {
  # Initialize the model for each chain
  model <- jags.model(textConnection(model_string), data = data_list, inits = inits, n.chains = 1)
  
  # Sample from the model
  samples <- coda.samples(model = model, variable.names = c("epsilon_home"),n.iter = 5000, thin = 50)
  
  return(samples)
}

library(parallel)
# Number of cores to use (use detectCores to find number of cores available)
num_cores <- detectCores() - 1  # Leave 1 core free for system processes

# Run the chains in parallel using mclapply
results <- mclapply(1:10, function(chain_id) {
  run_chain_school(chain_id, model_string, data_list, inits)
}, mc.cores = num_cores)

# Measure the time taken for running the chains in parallel
system.time({
  results_other <- mclapply(1:10, function(chain_id) {
    run_chain_other(chain_id, model_string, data_list, inits)
  }, mc.cores = num_cores)
})

# Measure the time taken for running the chains in parallel
system.time({
  results_school <- mclapply(1:10, function(chain_id) {
    run_chain_school(chain_id, model_string, data_list, inits)
  }, mc.cores = num_cores)
})

# Measure the time taken for running the chains in parallel
system.time({
  results_work <- mclapply(1:10, function(chain_id) {
    run_chain_work(chain_id, model_string, data_list, inits)
  }, mc.cores = num_cores)
})

# Measure the time taken for running the chains in parallel
system.time({
  results_home <- mclapply(1:10, function(chain_id) {
    run_chain_home(chain_id, model_string, data_list, inits)
  }, mc.cores = num_cores)
})

# Run the model
#model <- jags.model(textConnection(model_string), data = data_list, inits = inits,n.chains = 10)

### Simulate epsilon values from posterior - other category 
epsilon_other_samples_posterior <- coda.samples(model = model, variable.names = c("epsilon_other"),n.iter = 5000, thin = 50)
summary_stats_epsilon_other <- summary(epsilon_other_samples_posterior)
sampled_epsilon_other_mean <- summary_stats_epsilon_other$statistics[,"Mean"]

# Effective sample size for epsilons and theta 
ess <- effectiveSize(epsilon_other_samples_posterior)
ess

# Autocorrelation for epsilons and theta 
autocorr <- autocorr.diag(epsilon_other_samples_posterior)
print(autocorr)

# Create an auto-correlation plot for saving the epsilon other samples from the posterior distribution
autocorr.plot(epsilon_other_samples_posterior, lag.max = 300)

# Traceplot for all epsilons 
plot(epsilon_other_samples_posterior, trace = TRUE)


# Create the lambda matrix 
sampled_lambda_vector_posterior <- rep(0,256)
for (i in 1:256) {
  sampled_lambda_vector_posterior[i] <- exp(sum(sampled_epsilon_other_mean[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
}

# Create the matrix of lambdas 
lambda_other_matrix <- matrix(sampled_lambda_vector_posterior, nrow = 16, ncol = 16, byrow = TRUE) 

# Population ratio of Ireland

irl_df <- age_df_countries("Ireland")
irl_pop <- irl_df$population / sum(irl_df$population)


# Proportion of population for all POLYMOD countries 
bel_popr <- age_df_countries("Belgium")[,3] / (sum(age_df_countries("Belgium")[,3]))
germany_popr <- age_df_countries("Germany")[,3] / (sum(age_df_countries("Germany")[,3]))
finland_popr <- age_df_countries("Finland")[,3] / (sum(age_df_countries("Finland")[,3]))
gbp_popr <- age_df_countries("UK")[,3] / (sum(age_df_countries("UK")[,3]))
italy_popr <- age_df_countries("Italy")[,3] / (sum(age_df_countries("Italy")[,3]))
lux_popr <- age_df_countries("Luxembourg")[,3] / (sum(age_df_countries("Luxembourg")[,3]))
nether_popr <- age_df_countries("Netherlands")[,3] / (sum(age_df_countries("Netherlands")[,3]))
pol_popr <- age_df_countries("Poland")[,3] / (sum(age_df_countries("Poland")[,3]))
polymod_popr <- cbind(bel_popr,germany_popr,finland_popr,gbp_popr,italy_popr,lux_popr,nether_popr,pol_popr)
polympod_popr_combined <- rowMeans(polymod_popr)

ratio_irl <- irl_pop / polympod_popr_combined
ratio_irl <- as.numeric(ratio_irl)

# Multiply each row of the lambda matrix by the population proportion ratio 

irl_projection_matrix <- matrix(0,nrow = 16, ncol = 16)

for (i in 1:16) {
  print(i)
  irl_projection_matrix[i,] <- lambda_other_matrix[i,]*ratio_irl
  
}


# Create a vector with all the data
irl_other_contact_vector <- c(
  1.154766164, 0.465038221, 0.220562143, 0.144351749, 0.236710175, 0.445055441, 0.626316254, 0.506246981, 0.320408537, 0.212755952, 0.252919895, 0.224164784, 0.160763882, 0.1499188, 0.083238627, 0.039809615,
  0.506355517, 1.77428762, 0.602678119, 0.148116191, 0.12168057, 0.297301086, 0.441999045, 0.462483471, 0.365669356, 0.164703043, 0.129716583, 0.14269051, 0.143169458, 0.115784942, 0.048606717, 0.036485891,
  0.151207351, 0.747848073, 2.451771612, 0.339530661, 0.217838131, 0.226805988, 0.318817163, 0.359094368, 0.387588736, 0.230830082, 0.149152239, 0.098460862, 0.074647901, 0.083682954, 0.052590729, 0.046004607,
  0.073145624, 0.214205055, 0.923782395, 2.454533243, 0.576405682, 0.331912234, 0.255968886, 0.288678967, 0.276327909, 0.230386519, 0.10230044, 0.053929869, 0.042703282, 0.042134533, 0.022243634, 0.015013233,
  0.100727267, 0.119352691, 0.164924766, 0.940958836, 1.47605115, 0.718337604, 0.518353475, 0.32432209, 0.235747742, 0.257739686, 0.153954373, 0.108239811, 0.047043959, 0.03918004, 0.037982531, 0.030419623,
  0.214354729, 0.103663434, 0.086497642, 0.299592374, 0.811873799, 1.275502325, 0.883178725, 0.557059091, 0.34814539, 0.295498804, 0.245682062, 0.139847485, 0.059519713, 0.048081705, 0.026672006, 0.013668664,
  0.263675321, 0.174347028, 0.245457166, 0.173353118, 0.445221352, 0.805852944, 1.230729938, 0.827690763, 0.486841053, 0.348708266, 0.347888513, 0.266549903, 0.131794184, 0.104271554, 0.046532707, 0.043961462,
  0.225389769, 0.247155592, 0.172548379, 0.113155874, 0.261473087, 0.565002303, 0.861054025, 0.988145046, 0.644002125, 0.387489628, 0.259810015, 0.218599337, 0.202790688, 0.153941255, 0.080377089, 0.032339792,
  0.144205369, 0.174088706, 0.252789881, 0.143823981, 0.250431314, 0.370212428, 0.622240345, 0.633338298, 0.642077661, 0.366568244, 0.247395764, 0.123542437, 0.126411754, 0.097438242, 0.065704365, 0.028206407,
  0.040844275, 0.062463891, 0.075415512, 0.127395168, 0.169869391, 0.259606423, 0.387688563, 0.407936376, 0.365818263, 0.351508387, 0.269039721, 0.130765973, 0.100459704, 0.072229929, 0.057059258, 0.044798584,
  0.063898066, 0.111574055, 0.117392263, 0.154566536, 0.273825827, 0.427941875, 0.381898821, 0.348939523, 0.369065799, 0.386504905, 0.298636573, 0.251202259, 0.164898859, 0.09891415, 0.057785055, 0.039610364,
  0.092348269, 0.076451805, 0.076227618, 0.080130444, 0.194659526, 0.403967412, 0.509769821, 0.398412976, 0.363964304, 0.240477574, 0.347111793, 0.344071702, 0.240635808, 0.147496923, 0.072458101, 0.040004887,
  0.067045554, 0.068212332, 0.054409278, 0.076565111, 0.15654064, 0.29658642, 0.372742348, 0.420819537, 0.339191046, 0.257504545, 0.229209463, 0.301592797, 0.283326605, 0.226856222, 0.141027171, 0.060173947,
  0.069984833, 0.086088037, 0.050700436, 0.047274971, 0.13387224, 0.257427009, 0.396823279, 0.334987106, 0.298041332, 0.222172169, 0.241506358, 0.277664225, 0.284428902, 0.272099255, 0.127782794, 0.077525338,
  0.023240965, 0.04584482, 0.061212108, 0.101182372, 0.091013939, 0.16339936, 0.204937328, 0.268691385, 0.294059942, 0.20779065, 0.167145873, 0.177233254, 0.315054206, 0.311773158, 0.25471487, 0.094419093,
  0.036460608, 0.034515422, 0.047244752, 0.023118446, 0.049081046, 0.080688944, 0.186561603, 0.141053205, 0.125770369, 0.146230704, 0.104896964, 0.107479467, 0.109868903, 0.160840054, 0.124169276, 0.085656656
)

# Convert the vector to a matrix with 16 rows and 16 columns
irl_other_matrix <- matrix(irl_other_contact_vector, nrow = 16, byrow = TRUE)
irl_other_matrix <- contact_matrix("Ireland",location = "other")

# Print the resulting matrix
plot(c(t(irl_projection_matrix)),c(t(irl_other_matrix)))


# Autocorrelation plot for theta 
autocorr <- autocorr.diag(epsilon_other_samples_posterior)
print(autocorr)

plot(epsilon_other_samples_posterior, trace = TRUE)

epsilon_other_samples_posterior <- mcmc.list(lapply(results_other, as.mcmc))
epsilon_school_samples_posterior <- mcmc.list(lapply(results_school, as.mcmc))
epsilon_home_samples_posterior <- mcmc.list(lapply(results_home, as.mcmc))
epsilon_work_samples_posterior <- mcmc.list(lapply(results_work, as.mcmc))


summary_stats_epsilon_other<- summary(epsilon_other_samples_posterior)
summary_stats_epsilon_school<- summary(epsilon_school_samples_posterior)
summary_stats_epsilon_work<- summary(epsilon_work_samples_posterior)
summary_stats_epsilon_home <- summary(epsilon_home_samples_posterior)

sampled_epsilon_other_mean <- summary_stats_epsilon_other$statistics[,"Mean"]
sampled_epsilon_school_mean <- summary_stats_epsilon_school$statistics[,"Mean"]
sampled_epsilon_work_mean <- summary_stats_epsilon_work$statistics[,"Mean"]
sampled_epsilon_home_mean <- summary_stats_epsilon_home$statistics[,"Mean"]


epsilon_home_samples_posterior <- coda.samples(model = model, variable.names = c("epsilon_home"),n.iter = 5000, thin = 50)
summary_stats_epsilon_home <- summary(epsilon_home_samples_posterior)
sampled_epsilon_home_mean <- summary_stats_epsilon_home$statistics[,"Mean"]


# Create the lambda matrix 
sampled_lambda_vector_posterior_home <- rep(0,256)
for (i in 1:256) {
  sampled_lambda_vector_posterior_home[i] <- exp(sum(sampled_epsilon_home_mean[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
}

# Create the matrix of lambdas 
lambda_other_matrix_home <- matrix(sampled_lambda_vector_posterior_home, nrow = 16, ncol = 16, byrow = TRUE) 
lambda_other_matrix_home <- lambda_other_matrix_home * HAM_WORLD$IRL


# Effective sample size for epsilons and theta 
ess <- effectiveSize(epsilon_home_samples_posterior)
ess

# Autocorrelation for epsilons and theta 
autocorr <- autocorr.diag(epsilon_home_samples_posterior)
print(autocorr)

# Create an auto-correlation plot for saving the epsilon other samples from the posterior distribution
autocorr.plot(epsilon_home_samples_posterior, lag.max = 300)

# Traceplot for all epsilons 
plot(epsilon_home_samples_posterior, trace = TRUE)



# Lambda matrix for germany - home setting
lambda_org_matrix_home_data <- c(
  0.441298209, 0.342797514, 0.107625147, 0.094557669, 0.262981537, 0.274822318, 0.605972461, 0.45588805, 0.224152495, 0.064846886, 0.058404839, 0, 0.028073771, 0.010474916, 0.009680842, 0.002896204,
  0.072719675, 0.445168301, 0.310223629, 0.163259522, 0.038614078, 0.182485265, 0.394252913, 0.394504398, 0.414942824, 0.121563683, 0.097053639, 0.026229999, 0.004149868, 0.008645196, 0.005633539, 0.000739748,
  0.024200763, 0.167163655, 0.600622837, 0.385624147, 0.026716471, 0.040442123, 0.074206113, 0.275135207, 0.656653317, 0.210306484, 0.109515598, 0.017647231, 0, 0.021595657, 0.009592576, 0.003496345,
  0.014732847, 0.07269281, 0.170452937, 0.510944116, 0.170179048, 0.025851998, 0.014942705, 0.274857218, 0.431561246, 0.281430238, 0.119269303, 0.047232204, 0.020526746, 0.00809119, 0.005162315, 0.000350897,
  0.067557503, 0.027166533, 0.037792869, 0.165803149, 0.965391667, 0.218378876, 0.047311604, 0.017848038, 0.168551046, 0.299557621, 0.187684252, 0.060208736, 0.016978435, 0.002001376, 0.001244037, 0.007090623,
  0.231391106, 0.083166159, 0.026908617, 0.065201828, 0.223836663, 0.414978861, 0.1171398, 0.021676513, 0.008421773, 0.143471537, 0.187093618, 0.086809179, 0.024149764, 0.010488814, 0.000961553, 0.011019727,
  0.345236049, 0.230249512, 0.063828075, 0.003301823, 0.024949507, 0.129711963, 0.458173659, 0.202437134, 0.050686702, 0.000978263, 0.089699058, 0.031085924, 0.04574886, 0.012340773, 0.009465061, 0.002643328,
  0.143980498, 0.188816483, 0.232656307, 0.238770917, 0.019513102, 0.015242926, 0.157152485, 0.677110566, 0.185830343, 0.01577774, 0.011381188, 0.006006825, 0.020961466, 0.018106297, 0.014287342, 0.001679248,
  0.079813855, 0.269828097, 0.40863651, 0.444662693, 0.099933242, 0.005584289, 0.023002066, 0.086946027, 0.681608674, 0.142878658, 0.036341672, 0.000849265, 0.011694501, 0.030139415, 0.00236336, 6.08197E-24,
  0.024084132, 0.027448967, 0.126112396, 0.411053675, 0.269426321, 0.052679398, 0.006792082, 0.024235309, 0.122348289, 0.414727115, 0.164024746, 0.015679868, 0.010973501, 0.004792092, 0.002169992, 0.004748295,
  0.022713281, 0.052292496, 0.114914165, 0.166810325, 0.205644365, 0.134825926, 0.061665933, 0.024199919, 0.096866742, 0.156099768, 0.502898868, 0.105133534, 0.013143567, 0.007055674, 0, 0.003258206,
  0, 0.011033451, 0.009932547, 0.041851005, 0.101658235, 0.131470336, 0.050881462, 0.023316351, 0.002442553, 0.04517607, 0.151427284, 0.474224827, 0.142606531, 0.036212679, 0.000635489, 0.001474677,
  0.004179865, 0.00506382, 0, 0.047427905, 0.047184262, 0.044284358, 0.070874126, 0.083288324, 0.0320648, 0.034498072, 0.048873666, 0.204914177, 0.542117922, 0.165890632, 1.93927E-12, 0.004050243,
  0.004011337, 0.013365331, 0.043665536, 0.0097919, 0.011520958, 0.022075517, 0.034674079, 0.11252811, 0.154830066, 0.04774209, 0.013223502, 0.031864867, 0.125691559, 0.612880034, 0.11889934, 0.007598219,
  8.04669E-73, 0.04190472, 3.15603E-26, 0.035237809, 1.12043E-31, 0.008067473, 0.095882794, 0.209258561, 0.042421856, 0.081239713, 0, 0.013834313, 0.029574981, 0.284937334, 0.527195551, 0.078982779,
  0.029185407, 0.043403315, 0.021354887, 0.014289167, 1.11081E-21, 0.021981672, 0.088575463, 0.028423544, 0.11821243, 9.92172E-22, 0.213657745, 0.057567855, 0.028999218, 0.186966167, 0.028425162, 0.259683632
)

library(contactdata)
lambda_org_matrix_home_data <- contact_matrix("Ireland", location = "home")

# Define matrix with 16 rows and 16 columns
lambda_org_matrix_home <- matrix(lambda_org_matrix_home_data, nrow = 16, ncol = 16, byrow = TRUE)



plot(c(t(lambda_other_matrix_home)),c(t(lambda_org_matrix_home)))


# Simulate epilon home from coda.samples

epsilon_school_samples_posterior <- coda.samples(model = model, variable.names = c("epsilon_school"),n.iter = 5000, thin = 50)
summary_stats_epsilon_school <- summary(epsilon_school_samples_posterior)
sampled_epsilon_school_mean <- summary_stats_epsilon_school$statistics[,"Mean"]


# Create the lambda matrix 
sampled_lambda_vector_posterior_school <- rep(0,256)
for (i in 1:256) {
  sampled_lambda_vector_posterior_school[i] <- exp(sum(sampled_epsilon_school_mean[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
}

lambda_matrix_school <- matrix(sampled_lambda_vector_posterior_school, nrow = 16, ncol = 16, byrow = TRUE) 

write.csv(lambda_matrix_school, file = "lambda_school_matrix.csv", row.names = FALSE)


# For Ireland and POLYMOD countries 
s_a <- as.numeric(school[school$iso == "IRL",2:17])
irl_sa <- as.numeric(school[school$iso == "IRL",2:17])
bel_sa <- as.numeric(school[school$iso == "BEL",2:17]) / (sum(as.numeric(school[school$iso == "BEL",2:17])))
germany_sa <- as.numeric(school[school$iso == "DEU",2:17]) / (sum(as.numeric(school[school$iso == "DEU",2:17])))
finland_sa <- as.numeric(school[school$iso == "FIN",2:17]) / (sum(as.numeric(school[school$iso == "FIN",2:17])))
gbp_sa <- as.numeric(school[school$iso == "GBR",2:17]) / (sum(as.numeric(school[school$iso == "GBR",2:17])))
italy_sa <- as.numeric(school[school$iso == "ITA",2:17]) / (sum(as.numeric(school[school$iso == "ITA",2:17])))
lux_sa <- as.numeric(school[school$iso == "LUX",2:17]) / (sum(as.numeric(school[school$iso == "LUX",2:17])))
nether_sa <- as.numeric(school[school$iso == "NLD",2:17]) / (sum(as.numeric(school[school$iso == "NLD",2:17])))
poland_sa <- as.numeric(school[school$iso == "POL",2:17]) / (sum(as.numeric(school[school$iso == "POL",2:17])))
polymod_sa <- cbind(bel_sa,germany_sa,finland_sa,gbp_sa,italy_sa,lux_sa,nether_sa,poland_sa)
polympod_sa_combined <- rowMeans(polymod_sa)

ratio_irl <- irl_sa / polympod_sa_combined
p_sa_survey <- sum(output_school$s_i) / nrow(output_school)
irl_projection_matrix <- matrix(0,nrow = 16, ncol = 16)

for (i in 1:16) {
  print(i)
  irl_projection_matrix[i,] <- lambda_matrix_school[i,]*ratio_irl*p_sa_survey
  
}

irl_projection_matrix_school <- irl_projection_matrix

saveRDS(epsilon_school_samples_posterior, file = "epsilon_school_samples_posterior.rds")
epsilon_school_samples_posterior <- readRDS("epsilon_school_samples_posterior.rds")
epsilon_matrix <- as.matrix(epsilon_school_samples_posterior)
epsilon_df <- as.data.frame(epsilon_matrix)
write.csv(epsilon_df, file = "epsilon_school_samples_posterior.csv", row.names = FALSE)





# Lambda matrix for germany - school setting 

lambda_org_matrix_school_data <- "
1.327469823 0.110820079 3.98778E-16 0.241937929 0.010959182 0.012469806 0.139215904 0.070725805 0.006344758 0.028415309 0.005667512 4.42006E-86 8.6224E-144 2.0537E-105 3.5991E-133 5.2146E-123
0.195203643 0.640733012 0.023859611 0.057815688 0.021465159 0.039777214 0.04634958 0.045339453 0.053667798 0.03014935 0.014337831 6.25157E-60 3.9384E-92 1.50886E-95 7.8495E-101 1.5425E-131
3.69649E-20 0.078665294 0.955202078 0.041055368 9.70126E-26 0.009068625 0.012178365 0.022614694 0.035037175 0.02740654 0.018052895 0.006939438 0.004877761 0.001314323 3.52364E-73 3.3749E-110
1.35376E-52 0.041272862 0.703048211 1.542008202 0.010831603 0.016122589 0.012810264 0.026998983 0.03896117 0.037120664 0.042301351 0.005742941 0.004008851 0.001612423 2.3198E-97 4.1483E-113
1.56511E-19 3.31812E-17 0.002283544 0.379069112 0.162542752 0.05760461 0.006518835 0.01170136 0.001713691 0.008070398 0.007445625 0.001495781 9.84956E-35 4.36076E-52 2.4391E-118 1.4147E-129
0.014549806 0.015425193 0.005236412 5.15326E-16 0.158912243 0.138849469 0.0089449 0.014418231 0.016988045 0.010977506 0.010098245 0.020373763 0.002513839 7.18806E-65 2.80334E-93 2.1058E-119
0.036494619 1.31829E-15 4.43401E-60 4.70677E-19 0.018908987 1.88648E-35 5.15923E-24 0.05894952 4.90164E-24 3.7664E-42 0.025743217 2.48781E-29 1.76411E-93 2.1268E-78 5.3755E-135 2.168E-119
0.00516195 0.172829285 0.226469472 0.182528738 0.011755202 5.38822E-17 0.064514405 0.007710452 0.01526619 0.022599409 0.003171633 5.61043E-77 1.7678E-100 1.2914E-121 1.153E-121 1.8968E-113
3.83125E-45 0.044378426 0.169334482 0.647587616 1.47143E-44 5.79826E-71 0.014514936 9.39558E-27 0.063875866 0.031499773 3.4342E-36 1.34433E-56 8.3524E-153 2.0205E-122 7.7912E-114 1.6383E-121
3.55208E-55 1.46025E-14 0.28172326 1.34118036 1.14459E-61 2.34492E-74 3.35946E-80 5.08575E-36 0.060273612 0.046419907 0.009476457 7.40441E-78 1.2508E-96 3.041E-117 1.7096E-121 8.0541E-106
0.19503065 0.202400289 0.020488099 0.513156147 2.82903E-38 8.15262E-95 4.86055E-54 1.56122E-80 0.013360898 2.37911E-19 2.21808E-34 5.35088E-50 2.6827E-134 3.59E-113 1.72026E-90 1.18588E-71
5.25188E-47 5.79773E-38 5.79494E-70 2.08186E-74 4.35722E-68 0.01389495 2.94359E-65 0.014144835 0.017104694 0.049813071 0.01642562 0.039592864 4.26002E-48 5.20892E-66 0.007032424 0.007139283
1.55267E-66 0.162944581 6.11375E-92 1.17231E-98 9.29513E-91 2.03483E-93 1.14826E-53 2.39598E-86 3.31347E-71 3.98011E-82 6.85815E-50 9.61601E-48 1.34644E-82 0.023732656 2.42427E-70 1.80055E-65
3.96953E-72 2.27996E-75 8.77879E-61 1.2625E-137 7.7842E-133 6.3482E-100 2.8899E-148 1.3076E-104 4.2142E-122 1.4498E-122 2.1333E-121 3.7232E-131 1.28808E-96 1.23165E-93 2.54675E-86 7.7529E-127
1.3784E-154 3.49698E-85 1.3918E-139 1.1917E-127 1.3607E-112 5.0005E-111 2.4438E-126 3.0481E-111 1.3306E-116 3.3936E-109 1.5997E-116 6.6146E-127 5.9706E-117 3.42132E-96 2.0076E-130 1.64E-123
3.7378E-127 2.8228E-117 4.0725E-124 7.4605E-108 2.4853E-114 1.7614E-116 1.5275E-113 1.854E-114 1.4434E-116 1.7934E-114 2.1244E-113 5.5268E-111 2.5344E-117 5.5728E-118 8.8416E-118 2.0105E-110
"

# Convert the string data into a matrix
lambda_org_matrix_school <- as.matrix(read.table(text = lambda_org_matrix_school_data, header = FALSE))
lambda_org_matrix_school <- contact_matrix("Ireland", location = "school")


plot(c((irl_projection_matrix_school)),c((lambda_org_matrix_school)))


## Work matrices 

# Create the lambda matrix 
sampled_lambda_vector_posterior <- rep(0,256)
for (i in 1:256) {
  sampled_lambda_vector_posterior[i] <- exp(sum(sampled_epsilon_work_mean[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
}

# Create the matrix of lambdas 
lambda_work_matrix <- matrix(sampled_lambda_vector_posterior, nrow = 16, ncol = 16, byrow = TRUE) 

irl_wa <- workpopage$total[workpopage$total$iso3c == "IRL",6:21]
irl_wa[is.na(irl_wa)] <- 0
bel_wa <- (workpopage$total[workpopage$total$iso3c == "BEL",6:21])
germany_wa <- workpopage$total[workpopage$total$iso3c == "DEU",6:21]
finland_wa <- workpopage$total[workpopage$total$iso3c == "FIN",6:21]
gbp_wa <- workpopage$total[workpopage$total$iso3c == "GBR",6:21]
italy_wa <- workpopage$total[workpopage$total$iso3c == "ITA",6:21]
lux_wa <- workpopage$total[workpopage$total$iso3c == "LUX",6:21]
nld_wa <- workpopage$total[workpopage$total$iso3c == "NLD",6:21]
pol_wa <- workpopage$total[workpopage$total$iso3c == "POL",6:21]
polymod_wa <- rbind(bel_wa,germany_wa,finland_wa,gbp_wa,italy_wa,lux_wa,nld_wa,pol_wa)
polymod_wa[is.na(polymod_wa)] <- 0
polymod_wa_combined <- colMeans(polymod_wa)

wa_survey <- sum(output_work$w_i)/nrow(output_work)
ratio_val <- (irl_wa/polymod_wa_combined)
ratio_val[is.na(ratio_val)] <- 0

# Multiply each row of the lambda matrix by the population proportion ratio 

irl_projection_matrix_work <- matrix(0,nrow = 16, ncol = 16)
irl_projection_matrix_work <- NULL

for (i in 1:16) {
  print(i)
  print(lambda_work_matrix[i,])
  new_row <- lambda_work_matrix[i,]*(ratio_val*wa_survey)
  irl_projection_matrix_work <- rbind(irl_projection_matrix_work,new_row)
  
}



saveRDS(epsilon_work_samples_posterior, file = "epsilon_work_samples_posterior.rds")
epsilon_work_samples_posterior <- readRDS("epsilon_work_samples_posterior.rds")
epsilon_matrix <- as.matrix(epsilon_work_samples_posterior)
epsilon_df <- as.data.frame(epsilon_matrix)
write.csv(epsilon_df, file = "epsilon_work_samples_posterior.csv", row.names = FALSE)


# Effective sample size for epsilons and theta 
ess <- effectiveSize(epsilon_work_samples_posterior)
ess

# Autocorrelation for epsilons and theta 
autocorr <- autocorr.diag(epsilon_work_samples_posterior)
print(autocorr)

# Create an auto-correlation plot for saving the epsilon other samples from the posterior distribution
autocorr.plot(epsilon_work_samples_posterior, lag.max = 300)

# Traceplot for all epsilons 
plot(epsilon_work_samples_posterior, trace = TRUE)


# Create the matrix from the data provided in the image
irl_work_matrix <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8.20605E-92, 1.20585E-05, 3.1644E-125,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.34956E-05, 7.64591E-79, 2.38392E-65,
  0, 0, 0.017472116, 0.008174525, 0.010263278, 0.002995898, 0.024185415, 0.006662801, 0.023240317, 0.013628312, 0.005363341, 1.1586E-08, 7.16303E-18, 2.7978E-53, 4.95801E-06, 3.7772E-102,
  0, 0, 0.014569884, 0.539544235, 0.50836776, 0.29495766, 0.27160643, 0.244059927, 0.270194049, 0.215031012, 0.125501451, 0.052234092, 0.006232418, 8.347E-06, 2.85973E-06, 1.88926E-31,
  0, 0, 0.022421132, 0.342635495, 0.779832978, 0.754005065, 0.621604564, 0.662052691, 0.496742537, 0.401604203, 0.304761843, 0.122811708, 0.020182206, 9.86113E-06, 1.32609E-05, 3.74318E-06,
  0, 0, 0.029090721, 0.303836416, 0.748723819, 1.272022115, 0.899083129, 0.848545621, 0.779024308, 0.551343507, 0.450428982, 0.182523771, 0.02715853, 1.60675E-05, 1.01183E-05, 3.01443E-06,
  0, 0, 0.032557935, 0.162187376, 0.512448358, 0.852409681, 1.101862694, 0.934106017, 0.831893065, 0.685607633, 0.411018207, 0.210340363, 0.025118899, 1.63796E-05, 4.10101E-06, 3.49479E-06,
  0, 0, 0.019354313, 0.31271251, 0.410919986, 0.773944396, 0.818615971, 1.093479195, 1.057158453, 0.738490013, 0.534414031, 0.186510168, 0.017208458, 1.22989E-05, 9.13513E-06, 6.02097E-06,
  0, 0, 0.0217342, 0.194085065, 0.480646564, 0.760670497, 0.886223787, 0.916791812, 1.116183118, 0.905333239, 0.646708072, 0.196789825, 0.025075431, 1.43626E-05, 1.02722E-05, 1.29504E-05,
  0, 0, 0.029272421, 0.24489664, 0.330683364, 0.577224324, 0.731034032, 0.776919246, 0.798747612, 0.786933526, 0.503785268, 0.224840025, 0.020161475, 1.6281E-05, 1.08244E-05, 6.09172E-06,
  0, 0, 0.030867598, 0.178646523, 0.268442348, 0.567173298, 0.653046792, 0.657370957, 0.884596824, 0.86132809, 0.65035419, 0.276733254, 0.023499241, 1.1808E-05, 1.18227E-05, 1.01613E-05,
  0, 0, 0.042969767, 0.108458984, 0.163410573, 0.303239499, 0.41646127, 0.379587066, 0.48223388, 0.384007941, 0.353836834, 0.194194625, 0.02002738, 1.34978E-05, 6.5874E-06, 6.65717E-06,
  0, 0, 0.007639782, 0.007745665, 0.034207461, 0.061511664, 0.066931151, 0.076463487, 0.082161256, 0.080985429, 0.061498146, 0.041636029, 0.003618053, 2.0302E-05, 8.26102E-06, 1.48398E-05,
  7.603E-06, 3.36327E-06, 7.64855E-06, 2.27622E-05, 3.14933E-05, 7.89308E-05, 7.24213E-05, 2.91748E-05, 6.61874E-05, 5.95693E-05, 7.70713E-05, 5.30688E-05, 4.6603E-05, 1.41633E-05, 2.49066E-05, 1.19109E-05,
  5.78864E-55, 7.88785E-42, 2.5483E-06, 2.60648E-05, 1.68036E-05, 2.12447E-05, 3.57268E-05, 4.02377E-05, 3.56402E-05, 3.09769E-05, 2.13053E-05, 4.49709E-05, 2.61368E-05, 1.68266E-05, 1.66514E-05, 2.60823E-05,
  2.3572E-141, 9.06872E-97, 1.18637E-89, 9.39934E-22, 4.66E-05, 4.69664E-05, 4.69316E-05, 8.42184E-05, 2.77788E-05, 1.03294E-05, 1.06804E-05, 7.26342E-75, 1.10074E-65, 1.02832E-05, 5.16903E-49, 8.28041E-43
), nrow = 16, byrow = TRUE)

# Print the matrix
print(irl_work_matrix)
irl_work_matrix <- contact_matrix("Ireland", location = "work")

plot(c(t(irl_projection_matrix_work)),c(t(irl_work_matrix)))

# Other location category 
# Create the lambda matrix 
sampled_lambda_vector_posterior <- rep(0,256)
for (i in 1:256) {
  sampled_lambda_vector_posterior[i] <- exp(sum(sampled_epsilon_other_mean[neighbour_cells_matrix[i,1:total_neighbours[i]]])/total_neighbours[i])
}

# Create the matrix of lambdas 
lambda_other_matrix <- matrix(sampled_lambda_vector_posterior, nrow = 16, ncol = 16, byrow = TRUE) 
# For Ireland 
irl_pop <- popratio[popratio$iso3c == "IRL",][2,]
# For other POLYMOD countries 
bel_pop <- popratio[popratio$iso3c == "BEL",][1,]
germany_pop <- popratio[popratio$iso3c == "DEU",][2,]
finland_pop <- popratio[popratio$iso3c == "FIN",][2,]
gbp_pop <- popratio[popratio$iso3c == "GBR",][2,]
italy_pop <- popratio[popratio$iso3c == "ITA",][2,]
lux_pop <- popratio[popratio$iso3c == "LUX",][2,]
nld_pop <- popratio[popratio$iso3c == "NLD",][2,]
pol_pop <- popratio[popratio$iso3c == "POL",][2,]
  
polymod_pop <- rbind(bel_pop,germany_pop,finland_pop,gbp_pop,italy_pop,lux_pop,nld_pop,pol_pop)
polymod_pop <- polymod_pop[,4:24]
polymod_pop <- colMeans(polymod_pop)

pop_ratios <- irl_pop[,4:24] / polymod_pop

irl_projection_matrix_other <- matrix(0,nrow = 16, ncol = 16)
irl_projection_matrix_other <- NULL

for (i in 1:16) {
  print(i)
  print(lambda_other_matrix[i,])
  new_row <- lambda_other_matrix[i,]*(pop_ratios)
  irl_projection_matrix_other <- rbind(irl_projection_matrix_other,new_row)
  
}

irl_other_matrix <- contact_matrix("Ireland", location = "other")
plot(c(t(irl_projection_matrix_other)),c(t(irl_other_matrix)))

