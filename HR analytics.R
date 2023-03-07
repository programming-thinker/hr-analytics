library("readxl")
#Export data file

#install.packages("openxlsx")
#library(openxlsx)
#write.xlsx(df_v_leaving_staff, "data_voluntarily_leaving.xlsx", rowNames = FALSE)
#write.xlsx(df_c_leaving_staff, "data_cause_leaving.xlsx", rowNames = FALSE)
#write.xlsx(df_active_staff, "data_active.xlsx", rowNames = FALSE)


#Analysis 1 : Regaring to people who worked a long time in the company vs New staff.

library(lubridate)
library(magrittr)
library(dplyr)

df = read_excel("data.xlsx")

df_1 <- df %>%
  mutate(DateofTermination = ifelse(is.na(DateofTermination), "12/31/2022", DateofTermination))
#Translate the data in to Date form and calculate the difference between hire and leaving
df_1$DateofHire <- as.Date(df_1$DateofHire, format = "%m/%d/%Y")
df_1$DateofTermination <- as.Date(df_1$DateofTermination, format = "%m/%d/%Y")
df <- df_1 %>%
  mutate(DateDiff = round(time_length(interval(DateofHire, DateofTermination), "years")))


#Find voluntarily leaved people
df_v_leaving_staff = df[df$EmploymentStatus == 'Voluntarily Terminated',]

#Find people leaved by cause
df_c_leaving_staff = df[df$EmploymentStatus == 'Terminated for Cause',]

#Find people still work in the company.
df_active_staff = df[df$EmploymentStatus == 'Active',]
df_active_staff = df_active_staff[,-c(17)]


#Draw a bar chart 
library(ggplot2)
ggplot(df_v_leaving_staff, aes(x=DateDiff)) +
  geom_bar(fill="blue", alpha=0.7) +
  labs(title="Voluntary Termination Count and Years Worked at Company",
       x="Years Worked at Company",
       y="Count") +
  scale_x_continuous(limits=c(3,16), breaks=seq(3,15,1)) +
  theme_bw(base_size=18) +
  theme(plot.title = element_text(face="bold", size=22, hjust=0.5),
        axis.title = element_text(face="bold", size=20),
        axis.text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette="Blues")

#Percentage of leaving with years
freq <- as.data.frame(table(df_v_leaving_staff$DateDiff))
freq$pct <- with(freq, prop.table(Freq)*100)
freq <- freq[order(-freq$Freq),]

freq$cum_pct <- cumsum(freq$pct)


#Reasons for employees who resigned themselves to leave.
library(dplyr)
library(tidyr)


df_count_reason <- df_v_leaving_staff %>%
  count(TermReason) %>%
  mutate(`Relative Frequency` = round(n/sum(n) *100, 2)) %>%
  arrange(desc(n)) %>%
  mutate(`Cumulative Frequency` = cumsum(n),
         `Cumulative Frequency` = round(`Cumulative Frequency`/sum(n) *100, 2))


df_count_reason <- df_count_reason %>%
  pivot_longer(cols = -TermReason, names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = "Measure", values_from = "Value")


df_count_reason$`Relative Frequency` <- paste0(formatC(df_count_reason$`Relative Frequency`, digits = 2, format = "f"), "%")
df_count_reason$`Cumulative Frequency` <- paste0(formatC(df_count_reason$`Cumulative Frequency`, digits = 2, format = "f"), "%")

#PS: About 70% of the people left voluntarily because of career prospects (money, changing jobs) and psychological factors (unhappy, hours). 
#Although the remaining 30% left voluntarily, it was basically due to force majeure.
#Therefore, the focus of follow-up improvement should be on how to retain talents who leave due to defensible factors 
#(such as improving salary promotion mechanism, improving working environment, etc.)


#Analysis 2: Whether there exists salary difference between new staff and old one.
# working years >=6 old.
df_old = df[df$DateDiff >= 6,]
df_new = df[df$DateDiff <= 5,]

t_test_result <- t.test(df_old$Salary, df_new$Salary)

t_test_result

if (t_test_result$p.value < 0.05) {
  print("Significant")
} else {
  print("Not Significant")
}

# Which may indicate that the there is a problem with the promotion and salary increase mechanism. 
#There is no significant wage difference between veteran employees and new employees.




