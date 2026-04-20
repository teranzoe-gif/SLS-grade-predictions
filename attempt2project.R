library(dplyr)
library(stringr)

# 1. Grab the file from your local spot
file_path = "C:/STAT404/STAT_grades.csv"
all_lines = readLines(file_path)

# 2. Scope out the lines with 17 columns (16 commas)
comma_counts = str_count(all_lines, ",")
valid_lines = all_lines[comma_counts == 16]

# 3. Turn that mess into a clean data frame
df_clean = read.csv(text = valid_lines, header = FALSE, stringsAsFactors = FALSE)

colnames(df_clean) = c(
  "Dept", "Course", "Year", "Semester", "Prof", "GPA", 
  "Section", "A", "B", "C", "D", "F", "I", "Q", "S", "U", "X"
)

# Fix the types and toss the extra headers
df_final = df_clean %>%
  filter(Year != "Year") %>%
  mutate(across(c(Course, Year, GPA, Section, A, B, C, D, F, I, Q, S, U, X), as.numeric))

predict_distribution_advanced = function(target_dept, target_course, target_year, target_sem, target_prof) {
  
  # 1. Prep the history and set Semester/Prof as factors
  course_history = df_final %>%
    filter(Dept == target_dept, Course == target_course) %>%
    group_by(Year, Semester, Prof) %>%
    summarise(
      Total = sum(A + B + C + D + F),
      prop_A = sum(A) / Total,
      prop_B = sum(B) / Total,
      prop_C = sum(C) / Total,
      prop_D = sum(D) / Total,
      prop_F = sum(F) / Total,
      .groups = 'drop'
    ) %>%
    mutate(Semester = as.factor(Semester), Prof = as.factor(Prof))
  
  grades = c("prop_A", "prop_B", "prop_C", "prop_D", "prop_F")
  raw_preds = numeric(length(grades))
  names(raw_preds) = grades
  
  # 2. Run the math for every grade tier
  for(g in grades) {
    form = as.formula(paste(g, "~ Year + Semester + Prof"))
    model = lm(form, data = course_history)
    
    new_data = data.frame(
      Year = target_year, 
      Semester = as.factor(target_sem), 
      Prof = as.factor(target_prof)
    )
    
    # Try to predict, fallback if the Prof is a total stranger to the data
    pred_val = tryCatch({
      predict(model, newdata = new_data)
    }, error = function(e) {
      model_fallback = lm(as.formula(paste(g, "~ Year + Semester")), data = course_history)
      predict(model_fallback, newdata = new_data)
    })
    
    raw_preds[g] = max(0, pred_val)
  }
  
  # 3. Scale it to 100% so the math stays 100
  normalized_preds = raw_preds / sum(raw_preds)
  return(normalized_preds)
}

# --- STEP ONE: CREATE STUDENT PROFILE ---
calculate_profile = function(history) {
  ranks = c()
  
  for(i in 1:nrow(history)) {
    # 1. Match the class data exactly
    class_data = df_final %>%
      filter(Dept == history$Dept[i], 
             Course == history$Course[i], 
             Year == history$Year[i], 
             Semester == history$Semester[i], 
             Prof == history$Prof[i])
    
    # 2. Skip if the data is ghosting us
    if(nrow(class_data) == 0) {
      message("Warning: No historical data found for ", history$Course[i], " with ", history$Prof[i])
      next
    }
    
    counts = c(A=sum(class_data$A), B=sum(class_data$B), C=sum(class_data$C), D=sum(class_data$D), F=sum(class_data$F))
    total = sum(counts)
    
    # 3. Find your spot on the curve (midpoint logic)
    if(history$Grade[i] == "A") { 
      lower = 0; upper = counts['A'] 
    } else if(history$Grade[i] == "B") { 
      lower = counts['A']; upper = counts['A'] + counts['B'] 
    } else if(history$Grade[i] == "C") { 
      lower = sum(counts[c('A','B')]); upper = sum(counts[c('A','B','C')]) 
    } else if(history$Grade[i] == "D") { 
      lower = sum(counts[c('A','B','C')]); upper = sum(counts[c('A','B','C','D')]) 
    } else { 
      lower = sum(counts[c('A','B','C','D')]); upper = total 
    }
    
    ranks = c(ranks, ((lower + upper) / 2) / total)
  }
  
  return(mean(ranks, na.rm = TRUE))
}

# --- STEP TWO: PREDICT FUTURE CURVE ---
predict_curve = function(dept, course, year, sem, prof) {
  # 1. Filter for that specific Prof/Sem combo
  history = df_final %>% filter(Dept == dept, Course == course, Semester == sem, Prof == prof)
  
  # 2. Run the trend for every grade
  preds = sapply(c("A", "B", "C", "D", "F"), function(g) {
    model = lm(as.formula(paste(g, "/ (A+B+C+D+F) ~ Year")), data = history)
    max(0, predict(model, newdata = data.frame(Year = year)))
  })
  
  # 3. Normalize the vibe to 100%
  return(preds / sum(preds))
}

# --- STEP THREE: THE MAPPING ---
get_grade = function(rating, curve) {
  # 1. Stack the percentages, find where you land, and return the grade
  cum_curve = cumsum(curve)
  grade = names(which(cum_curve >= rating)[1])
  return(grade)
}

# --- RUNNING THE SYSTEM ---
my_history = data.frame(
  Dept     = c("STAT", "STAT"),
  Course   = c(182, 201),
  Year     = c(2024, 2025),
  Semester = c("FALL", "SPRING"),
  Prof     = c("DABNEY A", "LOCKHART M"), 
  Grade    = c("B", "A") 
)

my_rating = calculate_profile(my_history)
predicted_dist_2026 = predict_curve("STAT", 404, 2026, "SPRING", "WONG R")
final_prediction = get_grade(my_rating, predicted_dist_2026)

cat("Your Global Rating:", round(my_rating, 3), "\n")
cat("Predicted Grade:", final_prediction)