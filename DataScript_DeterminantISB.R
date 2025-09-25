# [1] Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
# [2] Anindya Mozumdar (2020). A guide to encoding categorical features using R, <https://www.r-bloggers.com/2020/02/a-guide-to-encoding-categorical-features-using-r/> 
# [3] Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
# [4] Iannone R, Cheng J, Schloerke B, Hughes E, Lauer A, Seo J, Brevoort K, Roy O (2024). _gt: Easily Create Presentation-Ready Display Tables_. R package version 0.11.1, <https://CRAN.R-project.org/package=gt>.
# [5] Sjoberg DD, Whiting K, Curry M, Lavery JA, Larmarange J. Reproducible summary tables with the gtsummary package. The R Journal 2021;13:570–80. <https://doi.org/10.32614/RJ-2021-053>.
# [6] Ugba E (2022). _gofcat: Goodness-of-Fit Measures for Categorical Response Models_. R package version 0.1.2, <https://CRAN.R-project.org/package=gofcat>.
# [7] UCLA: Statistical Consulting Group (2024). Ordinal logistic regression | R data analysis. UCLA Advanced Research Computing Statistical Methods and Data Analytics, <https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/examples>
# [8] Christensen R (2023). _ordinal-Regression Models for Ordinal Data_. R package version 2023.12-4.1, <https://CRAN.R-project.org/package=ordinal>.
# [9] Arnold J (2024). _ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'_. R package version 5.1.0, <https://CRAN.R-project.org/package=ggthemes>.
# [10] Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.
# [11] Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2009). “Somewhere over the Rainbow: How to Make Effective Use of Colors in Meteorological Visualizations.” _Bulletin of the American Meteorological Society_, *96*(2), 203-216. doi:10.1175/BAMS-D-13-00155.1 <https://doi.org/10.1175/BAMS-D-13-00155.1>.

library(tidyverse) #[1]

# import 2024 Great Lakes Regional Poll data
survey <- read_csv("2024_ICJ_Telephone_Poll.csv")

# subset data to only include demographic and geographic variables, perceptions, behavioural, recreational engagement
survey <- survey[,c(3,4, 7, 8, 10, 25, 27, 53, 54, 55, 56, 57, 58, 59, 117, 118, 120, 122)]
glimpse(survey) # see datatypes

# Rename columns
col_names <- c("JURISDICTION", "BASIN","Q3","Q4",
               "Q6A",
               "Q9",
               "Q10B",
               "Q18M1","Q18M2","Q18M3","Q18M4","Q18M5","Q18M6",
               "Q19",
               "D1",
               "D2","D4","D5"
)
colnames(survey) <- col_names

# Collapse oldest age categories to 55+
survey$D1[survey$D1 == "55-64"] <- "55+"
survey$D1[survey$D1 == "65-74"] <- "55+"
survey$D1[survey$D1 == "75 & older"] <- "55+"

# Encode data function (source: R-bloggers Anindya Mozumdar) [2]
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

# Jurisdiction
survey$JURISDICTION <- encode_ordinal(survey$JURISDICTION,
                                              order = c("Illinois", "Indiana", "Michigan", "Minnesota", "New York", "Ohio", "Ontario", "Pennsylvania", "Wisconsin"))
# Basin
survey$BASIN <- encode_ordinal(survey$BASIN,
                                       order = c("Lake Superior","Lake Michigan","Lake Huron","Lake Erie","Lake Ontario"))
# Q3
survey$Q3 <- encode_ordinal(survey$Q3,
                                    order = c("Don't know","Very poor","Poor","Neither poor nor good","Good","Very good"))-1
# Q4
survey$Q4 <- encode_ordinal(survey$Q4,
                                    order = c("Don't know","Deteriorating","Not changing","Improving"))-1
# Q6A
survey$Q6A <- encode_ordinal(survey$Q6A,
                                     order = c("Don't know","Not at all safe","Not safe","Neither safe nor unsafe","Safe","Very safe"))-1

# Q9
survey$Q9 <- encode_ordinal(survey$Q9,
                                    order = c("Don't know / unsure","Poor","Fair","Good"))-1
# Q10s
survey$Q10B <- encode_ordinal(survey$Q10B,
                                      order = c("Don't know / unsure","Poor","Fair","Good"))-1
# Q19
survey$Q19 <- encode_ordinal(survey$Q19,
                                     order = c("Don't know", "Never", "Rarely", "Sometimes","Most of the time","Always"))-1

# D1
survey$D1 <- encode_ordinal(survey$D1,
                                    order = c("Refused", "18-34", "35-44", "45-54","55+"))-1
# D2
survey$D2 <- encode_ordinal(survey$D2,
                                    order = c("Refused", "Some high school or less", "Graduated high school", "Some post secondary (college, university)","Graduated university / college"))-1

# D4
survey$D4 <- encode_ordinal(survey$D4,
                                    order = c("Refused","White / Caucasian / European origin", "Black / African American or Canadian / African", "Hispanic / Latinx", "South / SE Asian (India, Pakistan)","East Asian (China, Japan, Vietnam)","Middle Eastern / North African","Indigenous / Metis"))-1

#D5
survey$D5 <- encode_ordinal(survey$D5,
                                    order = c("Refused","Male","Female","Other"))-1

# Replace zeros with NA 
survey[survey == 0] <- NA # replace 0 with NA
survey[survey == -1] <- NA # replace -1 with NA

# Create binary columns where 0: minimal/no contact with water, 1: contact/indirect contact with water for each Q18M
survey$Q18_B1 <- survey$Q18M1
survey$Q18_B1[survey$Q18_B1 == "Birdwatching"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Walking / hiking"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Hunting"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Tent camping / RV camping"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Foraging"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Wild rice harvesting"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Cultural activities"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Political action"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "With friends/family/pets"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Don't know"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "-9998.00"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Swimming"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Fishing"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Paddleboarding"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Sailing / windsurfing"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Motorboating"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Kayaking"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Boating"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Canoeing"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "#NULL!"] <- 0 

survey$Q18_B2 <- survey$Q18M2
survey$Q18_B2[survey$Q18_B2 == "Birdwatching"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Walking / hiking"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Hunting"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Tent camping / RV camping"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Foraging"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Wild rice harvesting"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Cultural activities"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Political action"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "With friends/family/pets"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Don't know"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "-9998.00"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Swimming"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Fishing"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Paddleboarding"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Sailing / windsurfing"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Motorboating"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Kayaking"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Boating"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Canoeing"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "#NULL!"] <- 0 

survey$Q18_B3 <- survey$Q18M3
survey$Q18_B3[survey$Q18_B3 == "Birdwatching"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Walking / hiking"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Hunting"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Tent camping / RV camping"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Foraging"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Wild rice harvesting"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Cultural activities"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Political action"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "With friends/family/pets"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Don't know"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "-9998.00"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Swimming"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Fishing"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Paddleboarding"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Sailing / windsurfing"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Motorboating"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Kayaking"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Boating"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Canoeing"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "#NULL!"] <- 0 

survey$Q18_B4 <- survey$Q18M4
survey$Q18_B4[survey$Q18_B4 == "Birdwatching"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Walking / hiking"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Hunting"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Tent camping / RV camping"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Foraging"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Wild rice harvesting"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Cultural activities"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Political action"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "With friends/family/pets"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Don't know"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "-9998.00"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Swimming"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Fishing"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Paddleboarding"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Sailing / windsurfing"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Motorboating"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Kayaking"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Boating"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Canoeing"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "#NULL!"] <- 0 

survey$Q18_B5 <- survey$Q18M5
survey$Q18_B5[survey$Q18_B5 == "Birdwatching"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Walking / hiking"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Hunting"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Tent camping / RV camping"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Foraging"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Wild rice harvesting"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Cultural activities"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Political action"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "With friends/family/pets"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Don't know"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "-9998.00"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Swimming"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Fishing"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Paddleboarding"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Sailing / windsurfing"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Motorboating"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Kayaking"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Boating"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Canoeing"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "#NULL!"] <- 0 


survey$Q18_B6 <- survey$Q18M6
survey$Q18_B6[survey$Q18_B6 == "Birdwatching"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Walking / hiking"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Hunting"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Tent camping / RV camping"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Foraging"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Wild rice harvesting"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Cultural activities"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Political action"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "With friends/family/pets"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Don't know"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "-9998.00"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Swimming"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Fishing"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Paddleboarding"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Sailing / windsurfing"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Motorboating"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Kayaking"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Boating"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Canoeing"] <- 1
survey$Q18_B6[survey$Q18_B6 == "#NULL!"] <- 0 

# Save a csv file
write.csv(survey, file="Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# import cleaned data
survey <- read_csv("Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# combine all columns into one yes/no binary
survey$Q18_B1 <- as.numeric(survey$Q18_B1)
survey$Q18_B3 <- as.numeric(survey$Q18_B3)
survey$Q18_B4 <- as.numeric(survey$Q18_B4)
survey$Q18_B5 <- as.numeric(survey$Q18_B5)
survey$Q18_B6 <- as.numeric(survey$Q18_B6)
survey$Q18_B <- rowSums(survey[c(20, 21, 22, 23, 24, 25)])
survey$Q18_B <- with(survey, ifelse(survey$Q18_B > 0, 1, 0))

# categorize recreational activities
survey$Q18_C1 <- survey$Q18M1
survey$Q18_C1[survey$Q18_C1 == "Birdwatching"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Walking / hiking"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Hunting"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Tent camping / RV camping"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Foraging"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Wild rice harvesting"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Cultural activities"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Political action"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "With friends/family/pets"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Don't know"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "-9998.00"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Swimming"] <- 1 
survey$Q18_C1[survey$Q18_C1 == "Fishing"] <- 2 
survey$Q18_C1[survey$Q18_C1 == "Paddleboarding"] <- 3 
survey$Q18_C1[survey$Q18_C1 == "Sailing / windsurfing"] <- 3 
survey$Q18_C1[survey$Q18_C1 == "Motorboating"] <- 4 
survey$Q18_C1[survey$Q18_C1 == "Kayaking"] <- 4
survey$Q18_C1[survey$Q18_C1 == "Boating"] <- 4
survey$Q18_C1[survey$Q18_C1 == "Canoeing"] <- 4 
survey$Q18_C1[survey$Q18_C1 == "#NULL!"] <- 0 

survey$Q18_C2 <- survey$Q18M2
survey$Q18_C2[survey$Q18_C2 == "Birdwatching"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Walking / hiking"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Hunting"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Tent camping / RV camping"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Foraging"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Wild rice harvesting"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Cultural activities"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Political action"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "With friends/family/pets"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Don't know"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "-9998.00"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Swimming"] <- 1 
survey$Q18_C2[survey$Q18_C2 == "Fishing"] <- 2 
survey$Q18_C2[survey$Q18_C2 == "Paddleboarding"] <- 3 
survey$Q18_C2[survey$Q18_C2 == "Sailing / windsurfing"] <- 3 
survey$Q18_C2[survey$Q18_C2 == "Motorboating"] <- 4 
survey$Q18_C2[survey$Q18_C2 == "Kayaking"] <- 4
survey$Q18_C2[survey$Q18_C2 == "Boating"] <- 4
survey$Q18_C2[survey$Q18_C2 == "Canoeing"] <- 4 
survey$Q18_C2[survey$Q18_C2 == "#NULL!"] <- 0 

survey$Q18_C3 <- survey$Q18M3
survey$Q18_C3[survey$Q18_C3 == "Birdwatching"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Walking / hiking"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Hunting"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Tent camping / RV camping"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Foraging"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Wild rice harvesting"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Cultural activities"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Political action"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "With friends/family/pets"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Don't know"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "-9998.00"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Swimming"] <- 1 
survey$Q18_C3[survey$Q18_C3 == "Fishing"] <- 2 
survey$Q18_C3[survey$Q18_C3 == "Paddleboarding"] <- 3 
survey$Q18_C3[survey$Q18_C3 == "Sailing / windsurfing"] <- 3 
survey$Q18_C3[survey$Q18_C3 == "Motorboating"] <- 4 
survey$Q18_C3[survey$Q18_C3 == "Kayaking"] <- 4
survey$Q18_C3[survey$Q18_C3 == "Boating"] <- 4
survey$Q18_C3[survey$Q18_C3 == "Canoeing"] <- 4 
survey$Q18_C3[survey$Q18_C3 == "#NULL!"] <- 0 

survey$Q18_C4 <- survey$Q18M4
survey$Q18_C4[survey$Q18_C4 == "Birdwatching"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Walking / hiking"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Hunting"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Tent camping / RV camping"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Foraging"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Wild rice harvesting"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Cultural activities"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Political action"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "With friends/family/pets"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Don't know"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "-9998.00"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Swimming"] <- 1 
survey$Q18_C4[survey$Q18_C4 == "Fishing"] <- 2 
survey$Q18_C4[survey$Q18_C4 == "Paddleboarding"] <- 3 
survey$Q18_C4[survey$Q18_C4 == "Sailing / windsurfing"] <- 3 
survey$Q18_C4[survey$Q18_C4 == "Motorboating"] <- 4 
survey$Q18_C4[survey$Q18_C4 == "Kayaking"] <- 4
survey$Q18_C4[survey$Q18_C4 == "Boating"] <- 4
survey$Q18_C4[survey$Q18_C4 == "Canoeing"] <- 4 
survey$Q18_C4[survey$Q18_C4 == "#NULL!"] <- 0 

survey$Q18_C5 <- survey$Q18M5
survey$Q18_C5[survey$Q18_C5 == "Birdwatching"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Walking / hiking"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Hunting"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Tent camping / RV camping"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Foraging"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Wild rice harvesting"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Cultural activities"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Political action"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "With friends/family/pets"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Don't know"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "-9998.00"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Swimming"] <- 1 
survey$Q18_C5[survey$Q18_C5 == "Fishing"] <- 2 
survey$Q18_C5[survey$Q18_C5 == "Paddleboarding"] <- 3 
survey$Q18_C5[survey$Q18_C5 == "Sailing / windsurfing"] <- 3 
survey$Q18_C5[survey$Q18_C5 == "Motorboating"] <- 4 
survey$Q18_C5[survey$Q18_C5 == "Kayaking"] <- 4
survey$Q18_C5[survey$Q18_C5 == "Boating"] <- 4
survey$Q18_C5[survey$Q18_C5 == "Canoeing"] <- 4 
survey$Q18_C5[survey$Q18_C5 == "#NULL!"] <- 0 

survey$Q18_C6 <- survey$Q18M6
survey$Q18_C6[survey$Q18_C6 == "Birdwatching"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Walking / hiking"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Hunting"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Tent camping / RV camping"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Foraging"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Wild rice harvesting"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Cultural activities"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Political action"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "With friends/family/pets"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Don't know"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "-9998.00"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Swimming"] <- 1 
survey$Q18_C6[survey$Q18_C6 == "Fishing"] <- 2 
survey$Q18_C6[survey$Q18_C6 == "Paddleboarding"] <- 3 
survey$Q18_C6[survey$Q18_C6 == "Sailing / windsurfing"] <- 3 
survey$Q18_C6[survey$Q18_C6 == "Motorboating"] <- 4 
survey$Q18_C6[survey$Q18_C6 == "Kayaking"] <- 4
survey$Q18_C6[survey$Q18_C6 == "Boating"] <- 4
survey$Q18_C6[survey$Q18_C6 == "Canoeing"] <- 4 
survey$Q18_C6[survey$Q18_C6 == "#NULL!"] <- 0 


# binary yes/no for each type of water based recreational activity in question 18
survey$Q18_swimming <- with(survey, ifelse(Q18_C1 == 1 | Q18_C2 == 1 | Q18_C3 == 1 | Q18_C4 == 1 | Q18_C5 == 1 | Q18_C6 == 1, 1, 0))
survey$Q18_fishing <- with(survey, ifelse(Q18_C1 == 2 | Q18_C2 == 2 | Q18_C3 == 2 | Q18_C4 == 2 | Q18_C5 == 2 | Q18_C6 == 2, 1, 0))
survey$Q18_boarding <- with(survey, ifelse(Q18_C1 == 3 | Q18_C2 == 3 | Q18_C3 == 3 | Q18_C4 == 3  | Q18_C5 == 3 | Q18_C6 == 3, 1, 0))
survey$Q18_boating <- with(survey, ifelse(Q18_C1 == 4 | Q18_C2 == 4 | Q18_C3 == 4 | Q18_C4 == 4 | Q18_C5 == 4 | Q18_C6 == 4, 1, 0))

# subset data to remove the Q18s I don't need
survey <- survey[,c(2,3,4,5,6,7,8, 15,16,17,18,19,26,33,34,35,36)]

# Save a csv file 
write.csv(survey, file="Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

library(MASS) # [3]
library(tidyverse) # [1]
library(gt) # [4]
library(gtsummary) # [5]
library(gofcaf) # [6]

# import cleaned data
survey <- read_csv("Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# re-level reference baseline for univariable models
survey$JURISDICTION <- factor(survey$JURISDICTION, levels = c(7,1,2,3,4,5,6,8,9))
survey$BASIN <- factor(survey$BASIN, levels = c(4,1,2,3,5))
survey$Q3 <- factor(survey$Q3, levels = c(1,2,3,4,5))
survey$Q4 <- factor(survey$Q4, levels = c(1,2,3))
survey$Q6A <- factor(survey$Q6A, levels = c(1,2,3,4,5))
survey$Q9 <- factor(survey$Q9, levels = c(1,2,3))
survey$Q10B <- factor(survey$Q10B, levels = c(1,2,3))
survey$Q18_B <- factor(survey$Q18_B, levels = c(0,1))
survey$Q18_swimming <- factor(survey$Q18_swimming, levels = c(0,1))
survey$Q18_fishing <- factor(survey$Q18_fishing, levels = c(0,1))
survey$Q18_boarding <- factor(survey$Q18_boarding, levels = c(0,1))
survey$Q18_boating <- factor(survey$Q18_boating, levels = c(0,1))
survey$D1 <- factor(survey$D1, levels = c(1,2,3,4))
survey$D2 <- factor(survey$D2, levels = c(1,2,3,4))
survey$D4 <- factor(survey$D4, levels = c(1,2,3,4,5,6,7))
survey$D5 <- factor(survey$D5, levels = c(2,1,3))
survey$Q19 <- factor(survey$Q19)

# UCLA: Statistical Consulting Group Ordinal logistic regression [7] 
# univariable ordinal regression models
mQ_JURISDICTION <- polr(Q19 ~ JURISDICTION, data = survey, Hess = TRUE)
mQ_BASIN <- polr(Q19 ~ BASIN, data = survey, Hess = TRUE)
mQ3 <- polr(Q19 ~ Q3, data = survey, Hess = TRUE)
mQ4 <- polr(Q19 ~ Q4, data = survey, Hess = TRUE)
mQ6A <- polr(Q19 ~ Q6A, data = survey, Hess = TRUE)
mQ9 <- polr(Q19 ~ Q9, data = survey, Hess = TRUE)
mQ10B <- polr(Q19 ~ Q10B, data = survey, Hess = TRUE)
mQ18_B <- polr(Q19 ~ Q18_B, data = survey, Hess = TRUE)
mQ18_swimming <- polr(Q19 ~ Q18_swimming, data = survey, Hess = TRUE)
mQ18_fishing <- polr(Q19 ~ Q18_fishing, data = survey, Hess = TRUE)
mQ18_boarding <- polr(Q19 ~ Q18_boarding, data = survey, Hess = TRUE)
mQ18_boating <- polr(Q19 ~ Q18_boating, data = survey, Hess = TRUE)
mD1 <- polr(Q19 ~ D1, data = survey, Hess = TRUE)
mD2 <- polr(Q19 ~ D2, data = survey, Hess = TRUE)
mD5 <- polr(Q19 ~ D5, data = survey, Hess = TRUE)
mD4 <- polr(Q19 ~ D4, data = survey, Hess = TRUE)

# Calculate Confidence Intervals
ci_Q_JURISDICTION <- confint(mQ_JURISDICTION)
ci_Q_BASIN <- confint(mQ_BASIN)
ci_Q3 <- confint(mQ3)
ci_Q4 <- confint(mQ4)
ci_Q6A <- confint(mQ6A)
ci_Q9 <- confint(mQ9)
ci_Q10B <- confint(mQ10B)
ci_Q18_B <- confint(mQ18_B)
ci_Q18_swimming <- confint(mQ18_swimming)
ci_Q18_fishing <- confint(mQ18_fishing)
ci_Q18_boarding <- confint(mQ18_boarding)
ci_Q18_boating <- confint(mQ18_boating)
ci_D1 <- confint(mD1)
ci_D2 <- confint(mD2)
ci_D5 <- confint(mD5)
ci_D4 <- confint(mD4)

# Calculate Odds Ratio and Combine with confidence itervals
exp(cbind(OR = coef(mQ_JURISDICTION), ci_Q_JURISDICTION))
exp(cbind(OR = coef(mQ_BASIN), ci_Q_BASIN))
exp(cbind(OR = coef(mQ3), ci_Q3))
exp(cbind(OR = coef(mQ4), ci_Q4))
exp(cbind(OR = coef(mQ6A), ci_Q6A))
exp(cbind(OR = coef(mQ9), ci_Q9))
exp(cbind(OR = coef(mQ10B), ci_Q10B))
exp(cbind(OR = coef(mQ18_B), ci_Q18_B))
exp(cbind(OR = coef(mQ18_swimming), ci_Q18_swimming))
exp(cbind(OR = coef(mQ18_fishing), ci_Q18_fishing))
exp(cbind(OR = coef(mQ18_boarding), ci_Q18_boarding))
exp(cbind(OR = coef(mQ18_boating), ci_Q18_boating))
exp(cbind(OR = coef(mD1), ci_D1))
exp(cbind(OR = coef(mD2), ci_D2))
exp(cbind(OR = coef(mD5), ci_D5))
exp(cbind(OR = coef(mD4), ci_D4))

# Test the proportional odds assumption using the brant test
brant.test(mQ_JURISDICTION)
brant.test(mQ_BASIN)
brant.test(mQ3)
brant.test(mQ4)
brant.test(mQ6A)
brant.test(mQ9)
brant.test(mQ10B)
brant.test(mQ18_B)
brant.test(mQ18_swimming)
brant.test(mQ18_fishing)
brant.test(mQ18_boarding)
brant.test(mQ18_boating)
brant.test(mD1)
brant.test(mD2)
brant.test(mD5)
brant.test(mD4)

# make tables
mQ_JURISDICTION %>%
  tbl_regression(exponentiate = TRUE)
mQ_BASIN %>%
  tbl_regression(exponentiate = TRUE)
mQ3 %>%
  tbl_regression(exponentiate = TRUE)
mQ4 %>%
  tbl_regression(exponentiate = TRUE)
mQ6A %>%
  tbl_regression(exponentiate = TRUE)
mQ9 %>%
  tbl_regression(exponentiate = TRUE)
mQ10B %>%
  tbl_regression(exponentiate = TRUE)
mQ18_B %>%
  tbl_regression(exponentiate = TRUE)
mQ18_swimming %>%
  tbl_regression(exponentiate = TRUE)
mQ18_fishing %>%
  tbl_regression(exponentiate = TRUE)
mQ18_boarding %>%
  tbl_regression(exponentiate = TRUE)
mQ18_boating %>%
  tbl_regression(exponentiate = TRUE)
mD1 %>%
  tbl_regression(exponentiate = TRUE)
mD2 %>%
  tbl_regression(exponentiate = TRUE)
mD4 %>%
  tbl_regression(exponentiate = TRUE)
mD5 %>%
  tbl_regression(exponentiate = TRUE)

library(ordinal) # [8]

# adjusted models
surveym1 <- survey[,c(1,2,3,9,10,11,12,13)] # subset data for model 1

# complete cases model 1
surveym1 <- surveym1[complete.cases(surveym1),]

# Re-level all predictors for model 1
surveym1$JURISDICTION <- factor(surveym1$JURISDICTION, levels = c(7,1,2,3,4,5,6,8,9))
surveym1$BASIN <- factor(surveym1$BASIN, levels = c(4,1,2,3,5))
surveym1$D1 <- factor(surveym1$D1, levels = c(1,2,3,4))
surveym1$D2 <- factor(surveym1$D2, levels = c(1,2,3,4))
surveym1$D4 <- factor(surveym1$D4, levels = c(1,2,3,4,5,6,7))
surveym1$D5 <- factor(surveym1$D5, levels = c(2,1,3))
surveym1$Q19 <- factor(surveym1$Q19)


# model 1 - demographics + geography
m1_n <- clm(Q19 ~ D1 + D2 + D4 + D5 + JURISDICTION + BASIN, data = surveym1, Hess = TRUE) # no effects

nominal_test(m1_n) # nominal test 
scale_test(m1_n) # scale test

m1_e <- clm(Q19 ~ D1 + D4 + D5 + JURISDICTION + BASIN, scale = ~D1, nominal = ~D2, data = surveym1, Hess = TRUE) # with nominal and scale
m1 <- clm(Q19 ~ D1 + D2 + D4 + D5 + JURISDICTION + BASIN, scale = ~D1 + D2, data = surveym1, Hess = TRUE) # just scale

anova(m1_n, m1) # likelihood profile test - significant difference
anova(m1_e, m1) # non-significant difference

m1_or <- exp(coef(m1)) # Odds Ratio Model 1
m1_ci <- exp(confint(m1, level = 0.95)) # Confidence Intervals Model 1

# model 2 - all predictors ( including perceptions, behaviours)

# complete cases model 2
survey <- survey[complete.cases(survey),]

m2_n <- clm(Q19 ~ Q3 + Q4 + Q6A + Q9 + Q10B + Q18_B + Q18_swimming + Q18_fishing + Q18_boating + Q18_boarding + D1 + D2 + D4 + D5 + JURISDICTION + BASIN, data = survey, Hess = TRUE) # no effects

nominal_test(m2_n) # nominal tests
scale_test(m2_n) # scale tests

m2_e <- clm(Q19 ~ Q3 + Q4 + Q6A + Q9 + Q10B + Q18_B + Q18_swimming + Q18_fishing + Q18_boating + Q18_boarding + D1 + D4 + D5 + JURISDICTION + BASIN, scale = ~Q18_boarding + D1, data = survey, Hess = TRUE) # with scale effects

nominal_test(m2_e) # nominal tests
scale_test(m2_e) # scale tests

m2 <- clm(Q19 ~ Q3 + Q4 + Q6A + Q9 + Q10B + Q18_B + Q18_swimming + Q18_fishing + Q18_boarding + Q18_boating + D1 + D2 + D4 + D5 + JURISDICTION + BASIN, scale = ~Q18_boarding + Q18_boating + D1, data = survey, Hess = TRUE) # with just scale effects 2 (after some failed nominal effects)

anova(m2_n, m2) # likelihood profile test - significant difference
anova(m2_e, m2) # significant difference

library(ggthemes) # [9]
library(reshape2) # [10]
library(colorspace) # [11]

# UCLA: Statistical Consulting Group Ordinal logistic regression [6] 
# New Values for Predictions Age (D1) - Constant at Reference Levels
m1_d1 <- unique(surveym1$D1)
m1_d2 <- levels(surveym1$D2)[1] # reference level
m1_d4 <- levels(surveym1$D4)[1] # reference level
m1_d5 <- levels(surveym1$D5)[1] # reference level
m1_jurisdiction <- levels(surveym1$JURISDICTION)[1] # reference level
m1_basin <- levels(surveym1$BASIN)[1] # reference level

newdata_d1 <- data.frame(
  D1 = m1_d1,
  D2 = factor(rep(m1_d2, length(m1_d1)), levels = levels(surveym1$D2)),
  D4 = factor(rep(m1_d4, length(m1_d1)), levels = levels(surveym1$D4)),
  D5 = factor(rep(m1_d5, length(m1_d1)), levels = levels(surveym1$D5)),
  JURISDICTION = factor(rep(m1_jurisdiction, length(m1_d1)), levels = levels(surveym1$JURISDICTION)),
  BASIN = factor(rep(m1_basin, length(m1_d1)), levels = levels(surveym1$BASIN))
) # new data set

# predicted probabilities
predicted_probs_d1 <- cbind(newdata_d1, predict(m1, newdata_d1, type = "prob"))

newdat_m1d1 <- melt(predicted_probs_d1, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN"), variable.name = "Level", value.name="Probability") # reshape the data
glimpse(newdat_m1d1)

newdat_m1d1$Probability[newdat_m1d1$Level == "fit.3"] <- newdat_m1d1$Probability[newdat_m1d1$Level == "fit.3"] / 2 # divide sometimes value into 2
m1d1_duplicate <- newdat_m1d1[newdat_m1d1$Level == "fit.3", ] # duplicate "sometimes" rows
newdat_m1d1$Probability[newdat_m1d1$Level == "fit.1"] <- newdat_m1d1$Probability[newdat_m1d1$Level == "fit.1"] * -1 # multiple by -1 for negative behaviours
newdat_m1d1$Probability[newdat_m1d1$Level == "fit.2"] <- newdat_m1d1$Probability[newdat_m1d1$Level == "fit.2"] * -1
newdat_m1d1$Probability[newdat_m1d1$Level == "fit.3"] <- newdat_m1d1$Probability[newdat_m1d1$Level == "fit.3"] * -1
newdat_m1d1 <- rbind(newdat_m1d1, m1d1_duplicate) # combine data sets
newdat_m1d1$Percent <- newdat_m1d1$Probability * 100 # Get percent labels
newdat_m1d1$Percent2 <- newdat_m1d1$Percent[newdat_m1d1$Percent <= 0] * -1 # Make labels not negative
newdat_m1d1$Percent2 <- round(newdat_m1d1$Percent2)

newdat_m1d1$Level <- factor(newdat_m1d1$Level, levels = c("fit.1","fit.2", "fit.5","fit.4","fit.3"))

ord_colours <- c("fit.1" = "#533602", 
               "fit.2" = "#c9a36d", 
               "fit.3" = "#f6f6f6",
               "fit.4" = "#34bdaf",
               "fit.5" = "#004b40")

label_colours <- c("fit.1" = "white", 
                   "fit.2" = "black", 
                   "fit.3" = "black",
                   "fit.4" = "black",
                   "fit.5" = "white")

pp_D1 <- ggplot(newdat_m1d1, aes(fill = Level, y = Percent, x = D1 )) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Age Range",
    y = "Predicted Probability",
  ) +
  coord_flip() +
  scale_x_discrete(labels = c("18-34", "35-44", "45-54", "55+")) + # plot the data
  scale_fill_manual(values = ord_colours, 
                    breaks = c("fit.5", "fit.4", "fit.3", "fit.2", "fit.1"),
                    labels = c("Always", "Most of the Time", "Sometimes", "Rarely", "Never")) +
  scale_color_manual(values = label_colours, guide = "none" ) +
  scale_y_continuous(breaks = c(-50, 0, 50),
                     labels = c("50%","0%","50%")) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = paste0(Percent2, "%"), color = Level),
            size = 3,
            position = position_stack(vjust = 0.5))

# Repeat for Education (D2) 
# New Values for Predictions - Constant at Reference Levels
d2m1_d1 <- levels(surveym1$D1)[1] # reference level 
d2m1_d2 <- unique(surveym1$D2) 
d2m1_d4 <- levels(surveym1$D4)[1] # reference level
d2m1_d5 <- levels(surveym1$D5)[1] # reference level
d2m1_jurisdiction <- levels(surveym1$JURISDICTION)[1] # reference level
d2m1_basin <- levels(surveym1$BASIN)[1] # reference level

newdata_d2 <- data.frame(
  D1 = factor(rep(d2m1_d1, length(d2m1_d2)), levels = levels(surveym1$D1)),
  D2 = d2m1_d2,
  D4 = factor(rep(d2m1_d4, length(d2m1_d2)), levels = levels(surveym1$D4)),
  D5 = factor(rep(d2m1_d5, length(d2m1_d2)), levels = levels(surveym1$D5)),
  JURISDICTION = factor(rep(d2m1_jurisdiction, length(d2m1_d2)), levels = levels(surveym1$JURISDICTION)),
  BASIN = factor(rep(d2m1_basin, length(d2m1_d2)), levels = levels(surveym1$BASIN))
) # new data set

# predicted probabilities D2
predicted_probs_d2 <- cbind(newdata_d2, predict(m1, newdata_d2, type = "prob"))

newdat_m1d2 <- melt(predicted_probs_d2, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN"), variable.name = "Level", value.name="Probability") # reshape the data

newdat_m1d2$Probability[newdat_m1d2$Level == "fit.3"] <- newdat_m1d2$Probability[newdat_m1d2$Level == "fit.3"] / 2 # divide sometimes value into 2
m1d2_duplicate <- newdat_m1d2[newdat_m1d2$Level == "fit.3", ] # duplicate "sometimes" rows
newdat_m1d2$Probability[newdat_m1d2$Level == "fit.1"] <- newdat_m1d2$Probability[newdat_m1d2$Level == "fit.1"] * -1 # multiple by -1 for negative behaviours
newdat_m1d2$Probability[newdat_m1d2$Level == "fit.2"] <- newdat_m1d2$Probability[newdat_m1d2$Level == "fit.2"] * -1
newdat_m1d2$Probability[newdat_m1d2$Level == "fit.3"] <- newdat_m1d2$Probability[newdat_m1d2$Level == "fit.3"] * -1
newdat_m1d2 <- rbind(newdat_m1d2, m1d2_duplicate) # combine data sets
newdat_m1d2$Percent <- newdat_m1d2$Probability * 100 # Get percent labels
newdat_m1d2$Percent2 <- newdat_m1d2$Percent[newdat_m1d2$Percent <= 0] * -1 # Make labels not negative
newdat_m1d2$Percent2 <- round(newdat_m1d2$Percent2)

newdat_m1d2$Level <- factor(newdat_m1d2$Level, levels = c("fit.1","fit.2", "fit.5","fit.4","fit.3"))

pp_D2 <- ggplot(newdat_m1d2, aes(fill = Level, y = Percent, x = D2 )) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Education Level",
    y = "Predicted Probability",
  ) +
  coord_flip() +
  scale_x_discrete(labels = c("Some high school or less", "Graduated High school", "Some post secondary", "Graduated university / college")) + # plot the data
  scale_fill_manual(values = ord_colours, 
                    breaks = c("fit.5", "fit.4", "fit.3", "fit.2", "fit.1"),
                    labels = c("Always", "Most of the Time", "Sometimes", "Rarely", "Never")) +
  scale_color_manual(values = label_colours, guide = "none" ) +
  scale_y_continuous(breaks = c(-50, 0, 50),
                     labels = c("50%","0%","50%")) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = paste0(Percent2, "%"), color = Level),
            size = 3,
            position = position_stack(vjust = 0.5))




# Repeat for JURISDICTION 
# New Values for Predictions - Constant at Reference Levels
jm1_d1 <- levels(surveym1$D1)[1] # reference level 
jm1_d2 <- levels(surveym1$D2)[1] # reference level 
jm1_d4 <- levels(surveym1$D4)[1] # reference level
jm1_d5 <- levels(surveym1$D5)[1] # reference level
jm1_jurisdiction <- unique(surveym1$JURISDICTION)
jm1_basin <- levels(surveym1$BASIN)[1] # reference level

newdata_j <- data.frame(
  D1 = factor(rep(jm1_d1, length(jm1_jurisdiction)), levels = levels(surveym1$D1)),
  D2 = factor(rep(jm1_d2, length(jm1_jurisdiction)), levels = levels(surveym1$D2)),
  D4 = factor(rep(jm1_d4, length(jm1_jurisdiction)), levels = levels(surveym1$D4)),
  D5 = factor(rep(jm1_d5, length(jm1_jurisdiction)), levels = levels(surveym1$D5)),
  JURISDICTION = jm1_jurisdiction,
  BASIN = factor(rep(jm1_basin, length(jm1_jurisdiction)), levels = levels(surveym1$BASIN))
) # new data set

# predicted probabilities jurisdiction
predicted_probs_j <- cbind(newdata_j, predict(m1, newdata_j, type = "prob"))

newdat_m1j <- melt(predicted_probs_j, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN"), variable.name = "Level", value.name="Probability") # reshape the data

newdat_m1j$Probability[newdat_m1j$Level == "fit.3"] <- newdat_m1j$Probability[newdat_m1j$Level == "fit.3"] / 2 # divide sometimes value into 2
m1j_duplicate <- newdat_m1j[newdat_m1j$Level == "fit.3", ] # duplicate "sometimes" rows
newdat_m1j$Probability[newdat_m1j$Level == "fit.1"] <- newdat_m1j$Probability[newdat_m1j$Level == "fit.1"] * -1 # multiple by -1 for negative behaviours
newdat_m1j$Probability[newdat_m1j$Level == "fit.2"] <- newdat_m1j$Probability[newdat_m1j$Level == "fit.2"] * -1
newdat_m1j$Probability[newdat_m1j$Level == "fit.3"] <- newdat_m1j$Probability[newdat_m1j$Level == "fit.3"] * -1
newdat_m1j <- rbind(newdat_m1j, m1j_duplicate) # combine data sets
newdat_m1j$Percent <- newdat_m1j$Probability * 100 # Get percent labels
newdat_m1j$Percent2 <- newdat_m1j$Percent[newdat_m1j$Percent <= 0] * -1 # Make labels not negative
newdat_m1j$Percent2 <- round(newdat_m1j$Percent2)

newdat_m1j$Level <- factor(newdat_m1j$Level, levels = c("fit.1","fit.2", "fit.5","fit.4","fit.3"))

pp_j <- ggplot(newdat_m1j, aes(fill = Level, y = Percent, x = JURISDICTION )) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Education Level",
    y = "Predicted Probability",
  ) +
  coord_flip() +
  scale_x_discrete(labels = c("Ontario", "Illinois", "Indiana", "Michigan", "Minnesota", "New York", "Ohio", "Pennsylvania", "Wisconsin")) + # plot the data
  scale_fill_manual(values = ord_colours, 
                    breaks = c("fit.5", "fit.4", "fit.3", "fit.2", "fit.1"),
                    labels = c("Always", "Most of the Time", "Sometimes", "Rarely", "Never")) +
  scale_color_manual(values = label_colours, guide = "none" ) +
  scale_y_continuous(breaks = c(-50, 0, 50),
                     labels = c("50%","0%","50%")) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = paste0(Percent2, "%"), color = Level),
            size = 3,
            position = position_stack(vjust = 0.5))


# Repeat for BASIN
# New Values for Predictions - Constant at Reference Levels
bm1_d1 <- levels(surveym1$D1)[1] # reference level 
bm1_d2 <- levels(surveym1$D2)[1] # reference level 
bm1_d4 <- levels(surveym1$D4)[1] # reference level
bm1_d5 <- levels(surveym1$D5)[1] # reference level
bm1_jurisdiction <- levels(surveym1$JURISDICTION)[1] # reference level
bm1_basin <- unique(surveym1$BASIN) # reference level

newdata_b <- data.frame(
  D1 = factor(rep(bm1_d1, length(bm1_basin)), levels = levels(surveym1$D1)),
  D2 = factor(rep(bm1_d2, length(bm1_basin)), levels = levels(surveym1$D2)),
  D4 = factor(rep(bm1_d4, length(bm1_basin)), levels = levels(surveym1$D4)),
  D5 = factor(rep(bm1_d5, length(bm1_basin)), levels = levels(surveym1$D5)),
  JURISDICTION = factor(rep(bm1_jurisdiction, length(bm1_basin)), levels = levels(surveym1$JURISDICTION)),
  BASIN = bm1_basin
) # new data set

# predicted probabilities basin
predicted_probs_b <- cbind(newdata_b, predict(m1, newdata_b, type = "prob"))

newdat_m1b <- melt(predicted_probs_b, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN"), variable.name = "Level", value.name="Probability") # reshape the data

pp_b <- ggplot(newdat_m1b, aes(fill = Level, y = Probability, x = BASIN)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Closest Lake",
    y = "Predicted Probability",
    color = "Level",
    shape = "Level"
  ) +
  scale_x_discrete(labels = c("Erie", "Superior","Michigan","Huron","Ontario")) + # plot the data
  scale_fill_discrete_diverging("Green-Brown", labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always"),
                                rev = TRUE,
                                guide = guide_legend(reverse = TRUE))
pp_b <- pp_b + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Repeat for Q18_B Engagement in Water Recreation
# New Values for Predictions - Constant at Reference Levels
bm2_d1 <- levels(survey$D1)[1] # reference level 
bm2_d2 <- levels(survey$D2)[1] # reference level 
bm2_d4 <- levels(survey$D4)[1] # reference level
bm2_d5 <- levels(survey$D5)[1] # reference level
bm2_jurisdiction <- levels(survey$JURISDICTION)[1] # reference level
bm2_basin <- levels(survey$BASIN)[1] # reference level
bm2_q3 <- levels(survey$Q3)[1] # reference level 
bm2_q4 <- levels(survey$Q4)[1] # reference level 
bm2_q6 <- levels(survey$Q6A)[1] # reference level
bm2_q9 <- levels(survey$Q9)[1] # reference level 
bm2_q10 <- levels(survey$Q10B)[1] # reference level
bm2_q18b <- unique(survey$Q18_B) # reference level
bm2_q18s <- levels(survey$Q18_swimming)[1] # reference level
bm2_q18bb <- levels(survey$Q18_boating)[1] # reference level
bm2_q18w <- levels(survey$Q18_boarding)[1] # reference level
bm2_q18f <- levels(survey$Q18_fishing)[1] # reference level

newdata_bm2 <- data.frame(
  D1 = factor(rep(bm2_d1, length(bm2_q18b)), levels = levels(survey$D1)),
  D2 = factor(rep(bm2_d2, length(bm2_q18b)), levels = levels(survey$D2)),
  D4 = factor(rep(bm2_d4, length(bm2_q18b)), levels = levels(survey$D4)),
  D5 = factor(rep(bm2_d5, length(bm2_q18b)), levels = levels(survey$D5)),
  JURISDICTION = factor(rep(bm2_jurisdiction, length(bm2_q18b)), levels = levels(survey$JURISDICTION)),
  BASIN = factor(rep(bm2_basin, length(bm2_q18b)), levels = levels(survey$BASIN)), 
  Q3 = factor(rep(bm2_q3, length(bm2_q18b)), levels = levels(survey$Q3)),
  Q4 = factor(rep(bm2_q4, length(bm2_q18b)), levels = levels(survey$Q4)),
  Q6A = factor(rep(bm2_q6, length(bm2_q18b)), levels = levels(survey$Q6A)),
  Q9 = factor(rep(bm2_q9, length(bm2_q18b)), levels = levels(survey$Q9)),
  Q10B = factor(rep(bm2_q10, length(bm2_q18b)), levels = levels(survey$Q10B)),
  Q18_B = bm2_q18b,
  Q18_swimming = factor(rep(bm2_q18s, length(bm2_q18b)), levels = levels(survey$Q18_swimming)),
  Q18_boating = factor(rep(bm2_q18bb, length(bm2_q18b)), levels = levels(survey$Q18_boating)),
  Q18_boarding = factor(rep(bm2_q18w, length(bm2_q18b)), levels = levels(survey$Q18_boarding)),
  Q18_fishing = factor(rep(bm2_q18f, length(bm2_q18b)), levels = levels(survey$Q18_fishing))
) # new data set

# predicted probabilities engagement in water based recreation
predicted_probs_bm2 <- cbind(newdata_bm2, predict(m2, newdata_bm2, type = "prob"))

newdat_bm2 <- melt(predicted_probs_bm2, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN", 
                                                    "Q3", "Q4", "Q6A", "Q9", "Q10B",
                                                    "Q18_B","Q18_swimming", "Q18_boating", "Q18_boarding", "Q18_fishing"), variable.name = "Level", value.name="Probability") # reshape the data

pp_bm2 <- ggplot(newdat_bm2, aes(fill = Level, y = Probability, x = Q18_B)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Engagement with Water Based Recreation",
    y = "Predicted Probability",
    color = "Level",
    shape = "Level"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) + # plot the data
  scale_fill_discrete_diverging("Green-Brown", labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always"),
                                rev = TRUE,
                                guide = guide_legend(reverse = TRUE))

# Repeat for Q18_Boating
# New Values for Predictions - Constant at Reference Levels
bbm2_d1 <- levels(survey$D1)[1] # reference level 
bbm2_d2 <- levels(survey$D2)[1] # reference level 
bbm2_d4 <- levels(survey$D4)[1] # reference level
bbm2_d5 <- levels(survey$D5)[1] # reference level
bbm2_jurisdiction <- levels(survey$JURISDICTION)[1] # reference level
bbm2_basin <- levels(survey$BASIN)[1] # reference level
bbm2_q3 <- levels(survey$Q3)[1] # reference level 
bbm2_q4 <- levels(survey$Q4)[1] # reference level 
bbm2_q6 <- levels(survey$Q6A)[1] # reference level
bbm2_q9 <- levels(survey$Q9)[1] # reference level 
bbm2_q10 <- levels(survey$Q10B)[1] # reference level
bbm2_q18b <- levels(survey$Q18_B)[1] # reference level
bbm2_q18s <- levels(survey$Q18_swimming)[1] # reference level
bbm2_q18bb <- unique(survey$Q18_boating) # reference level
bbm2_q18w <- levels(survey$Q18_boarding)[1] # reference level
bbm2_q18f <- levels(survey$Q18_fishing)[1] # reference level

newdata_bbm2 <- data.frame(
  D1 = factor(rep(bbm2_d1, length(bbm2_q18bb)), levels = levels(survey$D1)),
  D2 = factor(rep(bbm2_d2, length(bbm2_q18bb)), levels = levels(survey$D2)),
  D4 = factor(rep(bbm2_d4, length(bbm2_q18bb)), levels = levels(survey$D4)),
  D5 = factor(rep(bbm2_d5, length(bbm2_q18bb)), levels = levels(survey$D5)),
  JURISDICTION = factor(rep(bbm2_jurisdiction, length(bbm2_q18bb)), levels = levels(survey$JURISDICTION)),
  BASIN = factor(rep(bbm2_basin, length(bbm2_q18bb)), levels = levels(survey$BASIN)), 
  Q3 = factor(rep(bbm2_q3, length(bbm2_q18bb)), levels = levels(survey$Q3)),
  Q4 = factor(rep(bbm2_q4, length(bbm2_q18bb)), levels = levels(survey$Q4)),
  Q6A = factor(rep(bbm2_q6, length(bbm2_q18bb)), levels = levels(survey$Q6A)),
  Q9 = factor(rep(bbm2_q9, length(bbm2_q18bb)), levels = levels(survey$Q9)),
  Q10B = factor(rep(bbm2_q10, length(bbm2_q18bb)), levels = levels(survey$Q10B)),
  Q18_B = factor(rep(bbm2_q18b, length(bbm2_q18bb)), levels = levels(survey$Q18_B)),
  Q18_swimming = factor(rep(bbm2_q18s, length(bbm2_q18bb)), levels = levels(survey$Q18_swimming)),
  Q18_boating = bbm2_q18bb,
  Q18_boarding = factor(rep(bbm2_q18w, length(bbm2_q18bb)), levels = levels(survey$Q18_boarding)),
  Q18_fishing = factor(rep(bbm2_q18f, length(bbm2_q18bb)), levels = levels(survey$Q18_fishing))
) # new data set

# predicted probabilities Boating
predicted_probs_bbm2 <- cbind(newdata_bbm2, predict(m2, newdata_bbm2, type = "prob"))

newdat_bbm2 <- melt(predicted_probs_bbm2, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN", 
                                                      "Q3", "Q4", "Q6A", "Q9", "Q10B",
                                                      "Q18_B","Q18_swimming", "Q18_boating", "Q18_boarding", "Q18_fishing"), variable.name = "Level", value.name="Probability") # reshape the data

pp_bbm2 <- ggplot(newdat_bbm2, aes(fill = Level, y = Probability, x = Q18_boating)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Engagement with Boat Recreation",
    y = "Predicted Probability",
    color = "Level",
    shape = "Level"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) + # plot the data
  scale_fill_discrete_diverging("Green-Brown", labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always"),
                                rev = TRUE,
                                guide = guide_legend(reverse = TRUE))

# Repeat for Q3 Water Quality Rating
# New Values for Predictions - Constant at Reference Levels
q3m2_d1 <- levels(survey$D1)[1] # reference level 
q3m2_d2 <- levels(survey$D2)[1] # reference level 
q3m2_d4 <- levels(survey$D4)[1] # reference level
q3m2_d5 <- levels(survey$D5)[1] # reference level
q3m2_jurisdiction <- levels(survey$JURISDICTION)[1] # reference level
q3m2_basin <- levels(survey$BASIN)[1] # reference level
q3m2_q3 <- unique(survey$Q3) # reference level 
q3m2_q4 <- levels(survey$Q4)[1] # reference level 
q3m2_q6 <- levels(survey$Q6A)[1] # reference level
q3m2_q9 <- levels(survey$Q9)[1] # reference level 
q3m2_q10 <- levels(survey$Q10B)[1] # reference level
q3m2_q18b <- levels(survey$Q18_B)[1] # reference level
q3m2_q18s <- levels(survey$Q18_swimming)[1] # reference level
q3m2_q18bb <- levels(survey$Q18_boating)[1] # reference level
q3m2_q18w <- levels(survey$Q18_boarding)[1] # reference level
q3m2_q18f <- levels(survey$Q18_fishing)[1] # reference level

newdata_q3m2 <- data.frame(
  D1 = factor(rep(q3m2_d1, length(q3m2_q3)), levels = levels(survey$D1)),
  D2 = factor(rep(q3m2_d2, length(q3m2_q3)), levels = levels(survey$D2)),
  D4 = factor(rep(q3m2_d4, length(q3m2_q3)), levels = levels(survey$D4)),
  D5 = factor(rep(q3m2_d5, length(q3m2_q3)), levels = levels(survey$D5)),
  JURISDICTION = factor(rep(q3m2_jurisdiction, length(q3m2_q3)), levels = levels(survey$JURISDICTION)),
  BASIN = factor(rep(q3m2_basin, length(q3m2_q3)), levels = levels(survey$BASIN)), 
  Q3 = q3m2_q3,
  Q4 = factor(rep(q3m2_q4, length(q3m2_q3)), levels = levels(survey$Q4)),
  Q6A = factor(rep(q3m2_q6, length(q3m2_q3)), levels = levels(survey$Q6A)),
  Q9 = factor(rep(q3m2_q9, length(q3m2_q3)), levels = levels(survey$Q9)),
  Q10B = factor(rep(q3m2_q10, length(q3m2_q3)), levels = levels(survey$Q10B)),
  Q18_B = factor(rep(q3m2_q18b, length(q3m2_q3)), levels = levels(survey$Q18_B)),
  Q18_swimming = factor(rep(q3m2_q18s, length(q3m2_q3)), levels = levels(survey$Q18_swimming)),
  Q18_boating = factor(rep(q3m2_q18bb, length(q3m2_q3)), levels = levels(survey$Q18_boating)),
  Q18_boarding = factor(rep(q3m2_q18w, length(q3m2_q3)), levels = levels(survey$Q18_boarding)),
  Q18_fishing = factor(rep(q3m2_q18f, length(q3m2_q3)), levels = levels(survey$Q18_fishing))
) # new data set

# predicted probabilities Q3
predicted_probs_q3m2 <- cbind(newdata_q3m2, predict(m2, newdata_q3m2, type = "prob"))

newdat_q3m2 <- melt(predicted_probs_q3m2, id.vars = c("D1", "D2", "D4", "D5","JURISDICTION", "BASIN", 
                                                      "Q3", "Q4", "Q6A", "Q9", "Q10B",
                                                      "Q18_B","Q18_swimming", "Q18_boating", "Q18_boarding", "Q18_fishing"), variable.name = "Level", value.name="Probability") # reshape the data

pp_q3m2 <- ggplot(newdat_q3m2, aes(fill = Level, y = Probability, x = Q3)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(
    x = "Water Quality Rating",
    y = "Predicted Probability",
    color = "Level",
    shape = "Level"
  ) +
  scale_x_discrete(labels = c("Very poor","Poor","Neither poor nor good","Good","Very good")) + # plot the data
  scale_fill_discrete_diverging("Green-Brown", labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always"),
                                rev = TRUE,
                                guide = guide_legend(reverse = TRUE))
pp_q3m2 <- pp_q3m2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export plots
ggsave("pp_b.pdf", pp_b,
       width = 6,
       height = 6.5,
       dpi = 300)
ggsave("pp_bbm2.pdf", pp_bbm2,
       width = 6,
       height = 6.5,
       dpi = 300)
ggsave("pp_bm2.pdf", pp_bm2,
       width = 6,
       height = 6.5,
       dpi = 300)
ggsave("pp_D1.pdf", pp_D1,
       width = 6,
       height = 6.5,
       dpi = 300)
ggsave("pp_D2.pdf", pp_D2,
       width = 6,
       height = 6.5,
       dpi = 300)
ggsave("pp_j.pdf", pp_j,
       width = 6,
       height = 6.5,
       dpi = 300)
ggsave("pp_q3m2.pdf", pp_q3m2,
       width = 6,
       height = 6.5,

       dpi = 300)
