# (a) Semester Dataの整形

# 1. 2. 
pacman::p_load(tidyverse)
pacman::p_load(dplyr)

Sem_dummy_1 <- read_csv("semester_dummy/semester_data_1.csv", skip = 1)
Sem_dummy_2 <- read_csv("semester_dummy/semester_data_2.csv")
Sem_dummy_2 <- rename(Sem_dummy_2,
                      unitid   = x1,
                      instnm   = x2,
                      semester = x3,
                      quarter  = x4,
                      year     = x5,
                      Y        = x6)

# 3.
Binded_Sd <- bind_rows(Sem_dummy_1, Sem_dummy_2)

# 4. 
Binded_Sd <- Binded_Sd[, -6]

# 5. 
Binded_Sd$Sem_Year <- rep(NA, nrow(Binded_Sd))
for (i in 1:nrow(Binded_Sd)) {
  if (Binded_Sd$year[i] == 1991 &
      Binded_Sd$semester[i] == 1) {
    Binded_Sd$Sem_Year[i] <- Binded_Sd$year[i]
  }
}
for (i in 2:nrow(Binded_Sd)) {
  if (Binded_Sd$semester[i-1] == 0 &
      Binded_Sd$semester[i]   == 1) {
    Binded_Sd$Sem_Year[i] <- Binded_Sd$year[i]
  } else if (Binded_Sd$unitid[i]     == Binded_Sd$unitid[i-1] &
             Binded_Sd$semester[i]   == 1 &
             Binded_Sd$semester[i-1] == 1) {
    Binded_Sd$Sem_Year[i] <- Binded_Sd$Sem_Year[i-1]
  }
}
for (i in nrow(Binded_Sd):2) {
  if (Binded_Sd$unitid[i-1] == Binded_Sd$unitid[i]) {
    Binded_Sd$Sem_Year[i-1] <- Binded_Sd$Sem_Year[i]
  }
}

# 6.
Binded_Sd$After_Sem_Year <- Binded_Sd$semester



# (b) Gradrate Dataの整形

# 1.
pacman::p_load(readxl)
Binded_Gr <- read_xlsx("outcome/1991.xlsx")
for (year in 1992:1993) {
  Gr_data <- read_xlsx(sprintf("outcome/%d.xlsx", year))
  Binded_Gr <- bind_rows(Binded_Gr, Gr_data)
}
for (year in 1995:2016) {
  Gr_data <- read_xlsx(sprintf("outcome/%d.xlsx", year))
  Binded_Gr <- bind_rows(Binded_Gr, Gr_data)
}

# 2. 
Binded_Gr$women_gradrate_4yr <- Binded_Gr$women_gradrate_4yr * 0.01

# 3.
Binded_Gr$totcohortsize <- as.numeric(Binded_Gr$totcohortsize)
Binded_Gr$m_4yrgrads    <- as.numeric(Binded_Gr$m_4yrgrads)
Binded_Gr$tot_gradrate_4yr <- Binded_Gr$tot4yrgrads / Binded_Gr$totcohortsize
Binded_Gr$men_gradrate_4yr <- Binded_Gr$m_4yrgrads / Binded_Gr$m_cohortsize

# 4. 
Binded_Gr$tot_gradrate_4yr <- signif(Binded_Gr$tot_gradrate_4yr, digits = 3)
Binded_Gr$men_gradrate_4yr <- signif(Binded_Gr$men_gradrate_4yr, digits = 3)

# 5. 
for (year in 2011:2016) {
  for (i in 1:nrow(Binded_Gr)) {
    if (Binded_Gr$year[i] == year) {
      Binded_Gr <- Binded_Gr[-i, ]
    }
  }
}
