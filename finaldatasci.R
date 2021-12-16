setwd("C:/users/Ryan/downloads")

ms <- read.csv("shootingsmj.csv")

byState <- sort(table(ms$state), decreasing = TRUE)
byState

byState <- byState/c(37,18,25,5,6.7,12.7,5.6,12.8,19.5,11.5,3.5,9.6,5.7,4.3,9.8,5.3,5.9,2.7,9.5,3.8,4.6,6.3,8,6.3,2.9,0.6,1.3,6.4,3,2.8,4.5,6.5,2.9,1.8,8.7,3.7,2.7)

sort(byState, decreasing = TRUE)

mean(subset(ms$age_of_shooter, !(ms$age_of_shooter == "NA")))

grepl("handgun",ms$weapon_type[2], fixed = TRUE)
ms$usedhandgun <- ifelse(grepl("handgun",ms$weapon_type, fixed = TRUE), 1, 0)
# percent that used a handgun
table(ms$usedhandgun)[2]/length(ms$usedhandgun)

ms$usedAutomatic <- ifelse(grepl("automatic",ms$weapon_type, fixed = TRUE), 1, 0)
# percent that used an automatic weapon
table(ms$usedAutomatic)[2]/length(ms$usedhandgun)

ms$automaticHandgun <- ifelse(grepl("automatic",ms$weapon_type, fixed = TRUE) & grepl("handgun",ms$weapon_type, fixed = TRUE), 1, 0)
# percent of handguns that were automatic
table(ms$automaticHandgun)[2]/table(ms$usedhandgun)[2]

#percent of automatic handguns that were used
table(ms$automaticHandgun)[2]/length(ms$usedhandgun)

ms$notautomaticHandgun <- ifelse(grepl("automatic",ms$weapon_type, fixed = TRUE) & !grepl("handgun",ms$weapon_type, fixed = TRUE), 1, 0)
# percent of non-handguns used that were automatic
table(ms$notautomaticHandgun)[2]/table(ms$usedhandgun)[2]

# percent of shooters with mental illness
ms$mental <- ifelse(!grepl("NA",ms$mental_health_details, fixed = TRUE) & !grepl("No",ms$prior_signs_mental_health_issues, fixed = TRUE), 1, 0)
table(ms$mental)

# t tests
mean(ms$mental)
t.test(ms$mental, var.equal = TRUE)
t.test(ms$usedhandgun, var.equal = TRUE)
t.test(subset(ms$age_of_shooter, !(ms$age_of_shooter == "NA")), var.equal = TRUE)
