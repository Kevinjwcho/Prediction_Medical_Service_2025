library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

# set the wd for plot
if(Sys.info()[["sysname"]] == "Linux"){
  plot_dir <- "/mnt/c/Users/kevin/Dropbox/Desktop/Prediction_medical_service/plot"
}else{
  plot_dir <- "~/Dropbox/Desktop/Prediction_medical_service/plot"
}


# Check the supplying assumptions -----------------------------------------

## Load the number of doctors data
read_excel_firstsheet <- function(filename, tibble = TRUE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  # x <- lapply(sheets, function(X) readxl::read_xlsx(filename, sheet = X, col_types = "numeric"))
  x = readxl::read_xlsx(filename, sheet = sheets[1], col_types = "numeric")
  if(!tibble) x <- as.data.frame(x)
  sex_age_var = expand.grid(age = c("total", "age2029", "age3039", "age4049", "age5059", "age6069", "age70"),sex = c("total", "m", "f"))
  colnames(x) <- c("sex", "age", paste0(2010:2020))
  x$sex = sex_age_var$sex
  x$age = sex_age_var$age
  # names(x) <- sheets
  x %>% melt(id.vars = c("sex", "age"), variable.name = "year", value.name = "value")
}


if(Sys.info()[["sysname"]] == "Linux"){
  path <- c("/mnt/c/Users/kevin/Dropbox/Desktop/Prediction_medical_service/data/nomed_doc_10yrs.xlsx")
  dat_nomed_doc<- read_excel_firstsheet(path)
  path <- c("/mnt/c/Users/kevin/Dropbox/Desktop/Prediction_medical_service/data/med_doc_10yrs.xlsx")
  dat_med_doc<- read_excel_firstsheet(path)
  path <- c("/mnt/c/Users/kevin/Dropbox/Desktop/Prediction_medical_service/data/inactive_doc_10yrs.xlsx")
  dat_inactive_doc<- read_excel_firstsheet(path)  
}else{
  path <- c("~/Dropbox/Desktop/Prediction_medical_service/data/nomed_doc_10yrs.xlsx")
  dat_nomed_doc<- read_excel_firstsheet(path)
  path <- c("~/Dropbox/Desktop/Prediction_medical_service/data/med_doc_10yrs.xlsx")
  dat_med_doc<- read_excel_firstsheet(path)
  path <- c("~/Dropbox/Desktop/Prediction_medical_service/data/inactive_doc_10yrs.xlsx")
  dat_inactive_doc<- read_excel_firstsheet(path)  
}

colnames(dat_nomed_doc)[ncol(dat_nomed_doc)] = "nomed_doc"
colnames(dat_med_doc)[ncol(dat_med_doc)] = "med_doc"
colnames(dat_inactive_doc)[ncol(dat_inactive_doc)] = "inactive_doc"

dat_doc = dat_nomed_doc %>% left_join(dat_med_doc) %>% left_join(dat_inactive_doc)

dat_doc = dat_doc %>% mutate(total_doc = nomed_doc + med_doc + inactive_doc,
                             nomed_doc_prop = nomed_doc/total_doc,
                             med_doc_prop = med_doc/total_doc,
                             inactive_doc_prop = inactive_doc/total_doc,
                             year = year %>% as.character %>% as.numeric)


## Check the proportion of each doctor type
ggplot(dat_doc %>% filter(sex == "total", age != "total"), aes(x = year, y = nomed_doc_prop, col = age)) + geom_line() + ggtitle("Proportion of non-medical doctors over 10 years") + ylab("non-medical/total")
ggplot(dat_doc %>% filter(sex == "total", age != "total"), aes(x = year, y = inactive_doc_prop, col = age)) + geom_line() + ggtitle("Proportion of inactive doctors over 10 years") + ylab("inactive/total")
ggplot(dat_doc %>% filter(sex == "total", age != "total"), aes(x = year, y = med_doc_prop, col = age)) + geom_line() + ggtitle("Proportion of medical doctors over 10 years") + ylab("medical/total")

lm.fit= lm(med_doc_prop ~ year, data = dat_doc %>% filter(sex == "total", age == "age70"))
summary(lm.fit)
predict(lm.fit) %>% plot()



# Define supply function --------------------------------------------------

supply_pred<- function(year, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                       admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                       age_init, age_med_prop, mid_yr = NULL, reduce_rate){
  
  if(is.null(mid_yr)){
    k= year - 2022 + 1
    yr = c(2022:year)
    
    age20_ago = numeric(k)
    age30_ago = numeric(k)
    age40_ago = numeric(k)
    age50_ago = numeric(k)
    age60_ago = numeric(k)
    age70_ago = numeric(k)
    
    age20_ago[1] = age_init[1]
    age30_ago[1] = age_init[2]
    age40_ago[1] = age_init[3]
    age50_ago[1] = age_init[4]
    age60_ago[1] = age_init[5]
    age70_ago[1] = age_init[6]
    
    age20 = numeric(k)
    age30 = numeric(k)
    age40 = numeric(k)
    age50 = numeric(k)
    age60 = numeric(k)
    age70 = numeric(k)
    
    pass_20 = numeric(k)
    pass_30 = numeric(k)
    
    med_admin_1 = c(med_admin_p_1, rep(admin_set_1, k-length(med_admin_p_1)))
    med_admin_2 = c(med_admin_p_2, rep(admin_set_2, k-length(med_admin_p_2)))
    med_admin_3 = c(med_admin_p_3, rep(admin_set_3, k-length(med_admin_p_3)))
    
    applicant_20 = numeric(k)
    applicant_30 = numeric(k)
    
    fail_20 = numeric(k)
    fail_30 = numeric(k)
    
    
    for(i in 1:k){
      if(applicant_20[i] == 0) {
        applicant_20[i] <- med_admin_1[i]
        applicant_30[i] <- med_admin_2[i] + med_admin_3[i]
      }
      pass_20[i] = applicant_20[i]*pass_rate
      pass_30[i] = applicant_30[i]*pass_rate
      fail_20[i] = applicant_20[i]*(1-pass_rate)
      fail_30[i] = applicant_30[i]*(1-pass_rate)
      
      # age20[i] = (age20_ago[i]-pass_5yr[i])*(1-dead_rate[1]/10^5)+pass[i]
      # age30[i] = pass_5yr[i]*(1-dead_rate[1]/10^5) + age30_ago[i]*.9*(1-dead_rate[2]/10^5)
      age20[i] = (age20_ago[i]*.8)*(1-dead_rate[1]/10^5)+pass_20[i]
      age30[i] = (age20_ago[i]*.2)*(1-dead_rate[1]/10^5) + (age30_ago[i]*.9 + pass_30[i])*(1-dead_rate[2]/10^5)
      age40[i] = age30_ago[i]*.1*(1-dead_rate[2]/10^5) + age40_ago[i]*.9*(1-dead_rate[3]/10^5)
      age50[i] = age40_ago[i]*.1*(1-dead_rate[3]/10^5) + age50_ago[i]*.9*(1-dead_rate[4]/10^5)
      age60[i] = age50_ago[i]*.1*(1-dead_rate[4]/10^5) + age60_ago[i]*.9*(1-dead_rate[5]/10^5)
      age70[i] = age60_ago[i]*.1*(1-dead_rate[4]/10^5) + age70_ago[i]*.9*(1-dead_rate[5]/10^5)
      
      age20_ago[i+1] = age20[i]
      age30_ago[i+1] = age30[i]
      age40_ago[i+1] = age40[i]
      age50_ago[i+1] = age50[i]
      age60_ago[i+1] = age60[i]
      age70_ago[i+1] = age70[i]
    } 
  }else{
    k = year - 2022 + 1
    k1= mid_yr - 2022 + 1
    k2= year - mid_yr + 1
    yr1 = c(2022:(mid_yr-1))
    yr2 = c(mid_yr:year)
    yr = c(2022:year)
    
    age20_ago = numeric(k)
    age30_ago = numeric(k)
    age40_ago = numeric(k)
    age50_ago = numeric(k)
    age60_ago = numeric(k)
    age70_ago = numeric(k)
    
    age20_ago[1] = age_init[1]
    age30_ago[1] = age_init[2]
    age40_ago[1] = age_init[3]
    age50_ago[1] = age_init[4]
    age60_ago[1] = age_init[5]
    age70_ago[1] = age_init[6]
    
    age20 = numeric(k)
    age30 = numeric(k)
    age40 = numeric(k)
    age50 = numeric(k)
    age60 = numeric(k)
    age70 = numeric(k)
    
    pass_20 = numeric(k)
    pass_30 = numeric(k)
    
    # med_admin = c(med_admin_p, rep(admin_set, k1-length(med_admin_p)), rep(mid_admin_set, k2))
    med_admin_1 = c(med_admin_p_1, rep(admin_set_1, k1-length(med_admin_p_1)), admin_set_1*reduce_rate^seq(1,k2,1))
    med_admin_2 = c(med_admin_p_2, rep(admin_set_2, k-length(med_admin_p_2)))
    med_admin_3 = c(med_admin_p_3, rep(admin_set_3, k-length(med_admin_p_3)))
    
    applicant_20 = numeric(k)
    applicant_30 = numeric(k)
    
    fail_20 = numeric(k)
    fail_30 = numeric(k)
    
    
    for(i in 1:k){
      if(applicant_20[i] == 0){
        applicant_20[i] <- med_admin_1[i]
        applicant_30[i] <- med_admin_2[i] + med_admin_3[i] 
      }
      pass_20[i] = applicant_20[i]*pass_rate
      pass_30[i] = applicant_30[i]*pass_rate
      fail_20[i] = applicant_20[i]*(1-pass_rate)
      fail_30[i] = applicant_30[i]*(1-pass_rate)
      
      # age20[i] = (age20_ago[i]-pass_5yr[i])*(1-dead_rate[1]/10^5)+pass[i]
      # age30[i] = pass_5yr[i]*(1-dead_rate[1]/10^5) + age30_ago[i]*.9*(1-dead_rate[2]/10^5)
      age20[i] = (age20_ago[i]*.8)*(1-dead_rate[1]/10^5)+pass_20[i]
      age30[i] = (age20_ago[i]*.2)*(1-dead_rate[1]/10^5) + (age30_ago[i]*.9 + pass_30[i])*(1-dead_rate[2]/10^5)
      age40[i] = age30_ago[i]*.1*(1-dead_rate[2]/10^5) + age40_ago[i]*.9*(1-dead_rate[3]/10^5)
      age50[i] = age40_ago[i]*.1*(1-dead_rate[3]/10^5) + age50_ago[i]*.9*(1-dead_rate[4]/10^5)
      age60[i] = age50_ago[i]*.1*(1-dead_rate[4]/10^5) + age60_ago[i]*.9*(1-dead_rate[5]/10^5)
      age70[i] = age50_ago[i]*.1*(1-dead_rate[5]/10^5) + age70_ago[i]*.9*(1-dead_rate[6]/10^5)
      
      age20_ago[i+1] = age20[i]
      age30_ago[i+1] = age30[i]
      age40_ago[i+1] = age40[i]
      age50_ago[i+1] = age50[i]
      age60_ago[i+1] = age60[i]
      age70_ago[i+1] = age70[i]
    } 
  }
  
  result <- cbind(yr, pass_20, pass_30, fail_20, fail_30, age20, age30, age40, age50, age60, age70) %>% as.data.frame
  result <- result %>% mutate(
    age20_med = age20*age_med_prop[1],
    age30_med = age30*age_med_prop[2],
    age40_med = age40*age_med_prop[3],
    age50_med = age50*age_med_prop[4],
    age60_med = age60*age_med_prop[5],
    age70_med = age70*(age_med_prop[6]*((yr-2022)*0.005128+1)), # 회귀 계수만큼 증가
    med_total = age20_med+age30_med+age40_med+age50_med+age60_med+age70_med,
    license_total = age20+age30+age40+age50+age60+age70,
    year = yr
  )
  return(result)
}

## Result of supply_pred function
med_admin_p_1 = c(2533, 3045, 3118, 3309, 3236, 2978, 3018, 3018, 3018) # 의대생: 20대로 가정 (2022년 보다 6년 전 데이터 (2016년) 부터 시작)
med_admin_p_2 = c(1242, 218, 198, 198, 149, 129, 129, 80, 80) # 의전원: 30대로 가정
med_admin_p_3 = c(100, 100, 100, 100, 100, 67, 85, 89, 85) # 편입생: 30세로 가정 -> 김영편입학원에서 데이터 가지고 옴 (의대 + 치의대)
admin_set_1 = 3018 # 2025년부터 2977명으로 고정 (의대생)
admin_set_2 = 80 # 2025년부터 80명으로 고정 (의전원)
admin_set_3 = 0 # 2025년부터 0명으로 고정 (편입생)
# apply_initial = 3319
# dead_rate = c(36.6, 66.5, 147, 322.4, 692.6) # 2017 사망률
dead_rate = c(35.5, 56.5, 114.7, 248.6, 547.4, 1491.4) # 2022 사망률
pass_rate = .958 # 2022 합격률
age_med_doc_2022 = c(4775, 25637, 29250, 25415, 10373, 4042) # 2022년 의료기관 의사수 (KOSIS 보건의료통계)
age_nonmed_doc_2022 = c(329, 2955, 1020, 1155, 817, 436) # 2022년 비의료기관 의사수 (KOSIS 보건의료통계)
age_init = age_med_doc_2022 + age_nonmed_doc_2022 # 2022년 의사수
age_med_prop = age_med_doc_2022/age_init # 2022년 의료기관 의사 비율 
# 그런데 여기 의료기관에 종사하는 의사 비율이 70세 이상이 linear하게 증가한다는 가정.


result_sup<- supply_pred(year = 2072, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                         admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                         age_init, age_med_prop)

## plot for the total number of license.
ggplot(result_sup, aes(x = yr, y = license_total))+geom_line() + ylim(c(0, 150000))


age_sup <- result_sup %>% dplyr::select(yr, age20, age30, age40, age50 ,age60, age70)
age_sup <- age_sup %>% melt(id.vars = "yr", variable.name = "age", value.name = "med_number")
ggplot(age_sup, aes( x = yr, y= med_number, fill = age))+geom_area(position = 'stack') + ggtitle("The number of doctors by age group")
ggplot(age_sup, aes( x = yr, y= med_number, fill = age))+geom_area(position = 'fill') + ggtitle("The proportion of doctors by age group")



# Scenario: different increasing ------------------------------------------

increase = c(0, 250, 500, 750, 1000)
yr_candi = increase+med_admin_p_1[length(med_admin_p_1)]
# yr_candi <- c(2977, 3477, 3977, 4477, 4977)
result <- matrix(ncol = 2, nrow = length(yr_candi))
for(i in 1:length(yr_candi)){
  test<- supply_pred(year = 2072, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                     admin_set_1 = yr_candi[i], admin_set_2, admin_set_3, dead_rate, pass_rate,
                     age_init, age_med_prop)
  result[i,] <- test$license_total[which(test$yr %in% c(2040, 2050))]
}

result <- format(result, digits = 6, decimal.mark = ",", big.mark = ",")

# Check the demand assumptions --------------------------------------------

read_excel_allsheets <- function(filename, tibble = TRUE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_xlsx(filename, sheet = X, col_types = "numeric"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# path <- c("C:/Users/kevin/Dropbox/SNU_medical/의사추계연구/Macro_trend/origin_data/demand/성별연령별_입원외래방문횟수_2003_2018.xlsx")
if(Sys.info()[["sysname"]] == "Linux"){
  path <- c("/mnt/c/Users/kevin/Dropbox/Desktop/Prediction_medical_service/data/성별연령별_입원외래방문횟수_2022_new.xlsx")
}else{
  path <- c("~/Dropbox/Desktop/Prediction_medical_service/data/성별연령별_입원외래방문횟수_2022_new.xlsx")  
}

demand_data <- read_excel_allsheets(path)

for(i in 1:length(demand_data)){
  demand_data[[i]] <- demand_data[[i]] %>% dplyr::select(-`...1`) %>% mutate_all(as.numeric)%>%
    mutate(year = names(demand_data)[i] %>% as.numeric,
           var = c("total", "total_m", "total_f",
                   "age00", "age00_m", "age00_f",
                   "age0104", "age0104_m", "age0104_f",
                   "age0509", "age0509_m", "age0509_f",
                   "age1014", "age1014_m", "age1014_f",
                   "age1519", "age1519_m", "age1519_f",
                   "age2024", "age2024_m", "age2024_f",
                   "age2529", "age2529_m", "age2529_f",
                   "age3034", "age3034_m", "age3034_f",
                   "age3539", "age3539_m", "age3539_f",
                   "age4044", "age4044_m", "age4044_f",
                   "age4549", "age4549_m", "age4549_f",
                   "age5054", "age5054_m", "age5054_f",
                   "age5559", "age5559_m", "age5559_f",
                   "age6064", "age6064_m", "age6064_f",
                   "age6569", "age6569_m", "age6569_f",
                   "age7074", "age7074_m", "age7074_f",
                   "age7579", "age7579_m", "age7579_f",
                   "age8084", "age8084_m", "age8084_f",
                   "age85", "age85_m", "age85_f")
    ) 
}
demand_data <- do.call("rbind", demand_data)
demand_data <- demand_data %>% as.data.frame
rownames(demand_data) <- NULL
str(demand_data)
colnames(demand_data) <- c("out_p", "in_p", "insurance", "out_per", "in_per", "year", "sex_age")


demand_10yrs = demand_data %>% filter(year %in% 2013:2022)
# filter data when sex_age variable do not have "_m" and "_f"
demand_10yrs_total = demand_10yrs %>% filter(!grepl("_m", sex_age) & !grepl("_f", sex_age) & sex_age != "total") %>% 
  mutate(year = as.numeric(year))

demand_10yrs_total_age20 = demand_10yrs_total %>% filter(sex_age == "age2024") %>% 
  transmute(year = as.numeric(year),
            out_p2024 = out_p,
            out_per2024 = out_per,
            in_p2024 = in_p,
            in_per2024 = in_per)
demand_10yrs_total_prop = demand_10yrs_total %>% left_join(demand_10yrs_total_age20, by = "year") %>%
  mutate(out_per_p = out_per/out_per2024,
         in_per_p = in_per/in_per2024,
         out_p_p = out_p/out_p2024,
         in_p_p = in_p/in_p2024)

demand_10yrs_only_total = demand_10yrs %>% filter(sex_age == "total")

p = ggplot(demand_10yrs_only_total, aes(x = year, y = out_p))+geom_line()+ggtitle("Total outpatient visits (annual) over 10 years")
ggsave("demand_outp.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)
p = ggplot(demand_10yrs_only_total, aes(x = year, y = in_p))+geom_line()+ggtitle("Total inpatient visits (annual) over 10 years")
ggsave("demand_inp.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)
p = ggplot(demand_10yrs_only_total, aes(x = year, y = (out_p+3*in_p)))+geom_line()+ggtitle("Total visits (annual) over 10 years")
ggsave("demand_total.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)

# Configure demand prediction data --------------------------------------------------

demand_22yr <- demand_data %>% filter(year == 2022)
str(demand_22yr)

demand_22yr_1 <- demand_22yr %>% mutate(pop_num = insurance) %>% dplyr::select(-insurance, -out_per, -in_per, -year)
demand_22yr_1<- demand_22yr_1 %>% mutate(sex = tstrsplit(sex_age, "_", keep = 2) %>% unlist,
                                         age = tstrsplit(sex_age, "_", keep = 1) %>% unlist)

demand_22yr_2 <- demand_22yr_1 %>% filter(!is.na(sex), age != "total") %>% dplyr::select(-sex_age)

sub_m<- c(demand_22yr_2[1, 1:3] + demand_22yr_2[3, 1:3], "m", "age0004") %>% unlist
sub_f<- c(demand_22yr_2[2, 1:3] + demand_22yr_2[4, 1:3], "f", "age0004") %>% unlist
# with(demand_18yr_2, sum(out_p[sex == "m" & age %in% c("age00", "age0104")]))

demand_22yr_3 <- rbind(sub_m, sub_f, demand_22yr_2[-c(1:4), ])

str(demand_22yr_3)

demand_22yr_3[,1:3] <- demand_22yr_3[,1:3] %>% mutate_all((as.numeric))

demand_22yr_3 <- demand_22yr_3 %>% mutate(out_per = out_p/ pop_num,
                                          in_per = in_p/pop_num)

# setwd("C:/Users/kevin/Dropbox")
# pop_estimate <- fread("D:/Dropbox/SNU_medical/의사추계연구/Macro_trend/Total_modeling/demand/pop_estimate.csv")
if(Sys.info()[["sysname"]] == "Linux"){
  pop_estimate <- fread("/mnt/c/Users/kevin/Dropbox/Desktop/Prediction_medical_service/data/장래인구추계/pop_estimate.csv", header = T)
}else{
  pop_estimate <- fread("~/Dropbox/Desktop/Prediction_medical_service/data/장래인구추계/pop_estimate.csv", header = T)  
}


# str(pop_estimate)
colnames(pop_estimate)[1:3] <- c("scean", "sex", "age")

# table(pop_estimate$시나리오별)
pop_est1 <- pop_estimate %>% filter(scean == "중위 추계(기본 추계: 출산율-중위 / 기대수명-중위 / 국제순이동-중위)",
                                    sex != "전체",
                                    age != "계") %>% dplyr::select(-scean)

pop_est2 <- pop_est1 %>% reshape2::melt(id = 1:2)
colnames(pop_est2)[3:4] <- c("year", "pop_est")
str(pop_est2)

pop_est_85 <- pop_est2 %>% filter(age %in% c("85 - 89세", "90 - 94세", "95 - 99세", "100세 이상")) %>% group_by(sex, year) %>% summarise(sum(pop_est))

pop_est_85$age <- c("age85")

pop_est_85_2 <- pop_est_85[,c(1,4,2,3)] %>% as.data.frame
colnames(pop_est_85_2)[4] <- "pop_est"

pop_est3 <- rbind(pop_est2 %>% filter(!(age %in% c("85 - 89세", "90 - 94세", "95 - 99세", "100세 이상"))),
                  pop_est_85_2)

pop_est4<- pop_est3 %>% mutate(sex = ifelse(sex == "남자", "m", "f"),
                               age = case_when(
                                 age == "0 - 4세" ~ "age0004",
                                 age == "5 - 9세" ~ "age0509",
                                 age == "10 - 14세" ~ "age1014",
                                 # age == "15-19세" ~ "age1519",
                                 age == "20 - 24세" ~ "age2024",
                                 age == "25 - 29세" ~ "age2529",
                                 age == "30 - 34세" ~ "age3034",
                                 age == "35 - 39세" ~ "age3539",
                                 age == "40 - 44세" ~ "age4044",
                                 age == "45 - 49세" ~ "age4549",
                                 age == "50 - 54세" ~ "age5054",
                                 age == "55 - 59세" ~ "age5559",
                                 age == "60 - 64세" ~ "age6064",
                                 age == "65 - 69세" ~ "age6569",
                                 age == "70 - 74세" ~ "age7074",
                                 age == "75 - 79세" ~ "age7579",
                                 age == "80 - 84세" ~ "age8084",
                                 age == "age85" ~ "age85",
                                 TRUE ~ "age1519"
                               ),
                               year = tstrsplit(year, " ", keep = 1) %>% unlist %>% as.numeric)


demand_22yr_fin<- demand_22yr_3 %>% dplyr::select(sex, age, out_per, in_per)

demand_est <- pop_est4 %>%  left_join(demand_22yr_fin) %>%
  mutate(out_p_est = round(pop_est*out_per),
         in_p_est = round(in_per*pop_est)) %>% arrange(year, sex)

demand_est_yr <- demand_est %>% group_by(year) %>% summarise(out_p_est = sum(out_p_est),
                                                             in_p_est = sum(in_p_est))

demand_est_yr1 <- demand_est_yr %>% mutate(total_demand = out_p_est + in_p_est * 3)

demand_est_yr2 <- demand_est_yr1
demand_est_yr2$out_p_est <- format(demand_est_yr2$out_p_est, digits = 10, big.mark = ",")
demand_est_yr2$in_p_est <- format(demand_est_yr2$in_p_est, digits = 10, big.mark = ",")

demand_est_yr1$year[which.max(demand_est_yr1$out_p_est)]
demand_est_yr2$out_p_est[which.max(demand_est_yr1$out_p_est)]
demand_est_yr1$year[which.max(demand_est_yr1$in_p_est)]
demand_est_yr2$in_p_est[which.max(demand_est_yr1$in_p_est)]


demand_est_yr_age <- demand_est %>% group_by(year, age) %>% summarise(out_p_est = sum(out_p_est),
                                                                      in_p_est = sum(in_p_est))

# Define the difference btw supply and demand -----------------------------

sup_demand<- function(admin_set_1, admin_set_2, admin_set_3, demand_est_yr, mid_yr = NULL, reduce_rate, tech_rate = 0.005){
  med_admin_p_1 = c(2533, 3045, 3118, 3309, 3236, 2978, 3018, 3018, 3018) # 의대생: 20대로 가정 (2022년 보다 6년 전 데이터 (2016년) 부터 시작)
  med_admin_p_2 = c(1242, 218, 198, 198, 149, 129, 129, 80, 80) # 의전원: 30대로 가정
  med_admin_p_3 = c(100, 100, 100, 100, 100, 67, 85, 89, 85) # 편입생: 30세로 가정 -> 김영편입학원에서 데이터 가지고 옴 (의대 + 치의대)
  # admin_set_1 = 4977
  # admin_set_2 = 80
  # admin_set_3 = 0
  # apply_initial = 3319
  dead_rate = c(35.5, 56.5, 114.7, 248.6, 547.4, 1491.4) # 2022 사망률
  pass_rate = .958 # 2022 합격률
  age_med_doc_2022 = c(4775, 25637, 29250, 25415, 10373, 4042) # 2022년 의료기관 의사수 (KOSIS 보건의료통계)
  age_nonmed_doc_2022 = c(329, 2955, 1020, 1155, 817, 436) # 2022년 비의료기관 의사수 (KOSIS 보건의료통계)
  age_init = age_med_doc_2022 + age_nonmed_doc_2022 # 2022년 의사수
  age_med_prop = age_med_doc_2022/age_init # 2022년 의료기관 의사 비율
  result_sup<- supply_pred(year = 2072, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                           admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                           age_init, age_med_prop, mid_yr, reduce_rate)
  
  # new_med_total은 60대는 75% 그리고 70대 이상은 50% 정도만 일을 한다고 가정.
  result_sup = result_sup %>% mutate(new_med_total = age20_med + age30_med + age40_med + age50_med + age60_med*0.75 + age70_med*0.5)
  result <- demand_est_yr %>% left_join(result_sup) %>% dplyr::select(year, out_p_est, in_p_est, license_total, med_total, new_med_total)
  
  result <- result %>% mutate(doc_capa = (out_p_est+in_p_est*3)/265/license_total,
                              doc_capa_med = (out_p_est+in_p_est*3)/265/new_med_total,
                              doc_demand_med = (out_p_est+in_p_est*3)/265/51,
                              doc_demand_med_tech = (out_p_est+in_p_est*3)/265/seq(51, by = 51*tech_rate, length.out = (2072 - 2022 + 1)),
                              sup_demand_med = new_med_total - doc_demand_med,
                              sup_demand_med_tech = new_med_total - doc_demand_med_tech,
                              
  )
  return(result)
}


# 시나리오3 -------------------------------------------------------------------
old_age_lst = c("age6569", "age7074", "age7579", "age8084", "age85")
demand_est_yr_age_s3 = demand_est_yr_age %>% mutate(out_p_est = case_when(year == 2025 & age %in% old_age_lst ~ out_p_est*(1-0.2*0.1),
                                                                          year == 2026 & age %in% old_age_lst ~ out_p_est*(1-0.3*0.1),
                                                                          year == 2027 & age %in% old_age_lst ~ out_p_est*(1-0.4*0.1),
                                                                          year == 2028 & age %in% old_age_lst ~ out_p_est*(1-0.5*0.1),
                                                                          year >= 2029 & age %in% old_age_lst ~ out_p_est*(1-0.6*0.1),
                                                                          TRUE ~ out_p_est),
                                                    in_p_est = case_when(year == 2025 & age %in% old_age_lst ~ in_p_est*(1-0.2*0.2),
                                                                         year == 2026 & age %in% old_age_lst ~ in_p_est*(1-0.3*0.2),
                                                                         year == 2027 & age %in% old_age_lst ~ in_p_est*(1-0.4*0.2),
                                                                         year == 2028 & age %in% old_age_lst ~ in_p_est*(1-0.5*0.2),
                                                                         year >= 2029 & age %in% old_age_lst ~ in_p_est*(1-0.6*0.2),
                                                                         TRUE ~ in_p_est)
)

demand_est_yr_s3 = demand_est_yr_age_s3 %>% group_by(year) %>% summarise(out_p_est = sum(out_p_est),
                                                                         in_p_est = sum(in_p_est))

sup_demand_s3 <- function(admin_set_1, admin_set_2, admin_set_3, demand_est_yr_s3, mid_yr = NULL, reduce_rate, tech_rate = 0.005){
  med_admin_p_1 = c(2533, 3045, 3118, 3309, 3236, 2978, 3018, 3018, 3018) # 의대생: 20대로 가정 (2022년 보다 6년 전 데이터 (2016년) 부터 시작)
  med_admin_p_2 = c(1242, 218, 198, 198, 149, 129, 129, 80, 80) # 의전원: 30대로 가정
  med_admin_p_3 = c(100, 100, 100, 100, 100, 67, 85, 89, 85) # 편입생: 30세로 가정 -> 김영편입학원에서 데이터 가지고 옴 (의대 + 치의대)
  # admin_set_1 = 4977
  # admin_set_2 = 80
  # admin_set_3 = 0
  # apply_initial = 3319
  dead_rate = c(35.5, 56.5, 114.7, 248.6, 547.4, 1491.4) # 2022 사망률
  pass_rate = .958 # 2022 합격률
  age_med_doc_2022 = c(4775, 25637, 29250, 25415, 10373, 4042) # 2022년 의료기관 의사수 (KOSIS 보건의료통계)
  age_nonmed_doc_2022 = c(329, 2955, 1020, 1155, 817, 436) # 2022년 비의료기관 의사수 (KOSIS 보건의료통계)
  age_init = age_med_doc_2022 + age_nonmed_doc_2022 # 2022년 의사수
  age_med_prop = age_med_doc_2022/age_init # 2022년 의료기관 의사 비율
  result_sup<- supply_pred(year = 2072, med_admin_p_1, med_admin_p_2, med_admin_p_3,
                           admin_set_1, admin_set_2, admin_set_3, dead_rate, pass_rate,
                           age_init, age_med_prop, mid_yr, reduce_rate)
  
  # new_med_total은 60대는 75% 그리고 70대 이상은 50% 정도만 일을 한다고 가정.
  result_sup = result_sup %>% mutate(new_med_total = age20_med + age30_med + age40_med + age50_med + age60_med*0.75 + age70_med*0.5)
  result <- demand_est_yr_s3 %>% left_join(result_sup) %>% dplyr::select(year, out_p_est, in_p_est, license_total, med_total, new_med_total)
  
  result <- result %>% mutate(doc_capa = (out_p_est+in_p_est*3)/265/license_total,
                              doc_capa_med = (out_p_est+in_p_est*3)/265/new_med_total,
                              doc_demand_med = (out_p_est+in_p_est*3)/265/51,
                              doc_demand_med_tech = (out_p_est+in_p_est*3)/265/seq(51, by = 51*tech_rate, length.out = (2072 - 2022 + 1)),
                              sup_demand_med = new_med_total - doc_demand_med,
                              sup_demand_med_tech = new_med_total - doc_demand_med_tech,
                              
  )
  return(result)
}

### Visualization

library(RColorBrewer)
ggplot(demand_est_yr_age, aes(x = year, y = out_p_est, fill = age)) + geom_area(position = 'stack') + ggtitle("Prediction of outpatient visits from")+
  ylim(c(0, 1.1*10^9))
ggplot(demand_est_yr_age_s3, aes(x = year, y = out_p_est, fill = age)) + geom_area(position = 'stack') + ggtitle("Prediction of outpatient visits from S3")+
  ylim(c(0, 1.1*10^9))
demand_est_yr_s3 %>% mutate(total = out_p_est + in_p_est*3) %>% left_join(demand_est_yr1, by ="year") %>% 
  transmute(year = year,
            prop_total = 1-total/total_demand,
            prop_out = 1-out_p_est.x/out_p_est.y,
            prop_in = 1-in_p_est.x/in_p_est.y) %>% as.data.frame()
ggplot(demand_est_yr_age, aes(x = year, y = in_p_est, fill = age)) + geom_area(position = 'stack')+ ggtitle("Prediction of inpatient visits")+
  ylim(c(0, 3.1*10^8))
ggplot(demand_est_yr_age_s3, aes(x = year, y = in_p_est, fill = age)) + geom_area(position = 'stack')+ ggtitle("Prediction of inpatient visits from S3")+
  ylim(c(0, 3.1*10^8))

demand_est_yr_age_s3 = demand_est_yr_age_s3 %>% mutate(total_est = out_p_est + in_p_est*3)
p = ggplot(demand_est_yr_age_s3, aes(x = year, y = total_est, fill = age)) + geom_area(position = 'stack')+ ggtitle("Prediction of total visits from S3")+
  ylim(0, 2*10^9)
ggsave("total_demand_age_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)

# S3: tech_rate: 0.25% -----------------------------------------------------
### results

result_0_s3<- sup_demand_s3(yr_candi[1], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97, tech_rate = 0.0025)
result_250_s3<- sup_demand_s3(yr_candi[2], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97, tech_rate = 0.0025)
result_500_s3<- sup_demand_s3(yr_candi[3], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97, tech_rate = 0.0025)
result_750_s3<- sup_demand_s3(yr_candi[4], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97, tech_rate = 0.0025)
result_1000_s3<- sup_demand_s3(yr_candi[5], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97, tech_rate = 0.0025)

## colnames changing
colnames(result_0_s3)[-c(1:3)] <-paste0(colnames(result_0_s3)[-c(1:3)], "_0" )
colnames(result_250_s3)[-c(1:3)] <-paste0(colnames(result_250_s3)[-c(1:3)], "_250" )
colnames(result_500_s3)[-c(1:3)] <-paste0(colnames(result_500_s3)[-c(1:3)], "_500" )
colnames(result_750_s3)[-c(1:3)] <-paste0(colnames(result_750_s3)[-c(1:3)], "_750" )
colnames(result_1000_s3)[-c(1:3)] <-paste0(colnames(result_1000_s3)[-c(1:3)], "_1000" )

result_s3_5y <- result_0_s3 %>% left_join(result_250_s3, by = "year") %>%
  left_join(result_500_s3, by = "year") %>% 
  left_join(result_750_s3, by = "year") %>%
  left_join(result_1000_s3, by = "year")

result_0_s3<- sup_demand_s3(yr_candi[1], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97, tech_rate = 0.0025)
result_250_s3<- sup_demand_s3(yr_candi[2], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97, tech_rate = 0.0025)
result_500_s3<- sup_demand_s3(yr_candi[3], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97, tech_rate = 0.0025)
result_750_s3<- sup_demand_s3(yr_candi[4], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97, tech_rate = 0.0025)
result_1000_s3<- sup_demand_s3(yr_candi[5], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97, tech_rate = 0.0025)

## colnames changing
colnames(result_0_s3)[-c(1:3)] <-paste0(colnames(result_0_s3)[-c(1:3)], "_0" )
colnames(result_250_s3)[-c(1:3)] <-paste0(colnames(result_250_s3)[-c(1:3)], "_250" )
colnames(result_500_s3)[-c(1:3)] <-paste0(colnames(result_500_s3)[-c(1:3)], "_500" )
colnames(result_750_s3)[-c(1:3)] <-paste0(colnames(result_750_s3)[-c(1:3)], "_750" )
colnames(result_1000_s3)[-c(1:3)] <-paste0(colnames(result_1000_s3)[-c(1:3)], "_1000" )

result_s3_10y <- result_0_s3 %>% left_join(result_250_s3, by = "year") %>%
  left_join(result_500_s3, by = "year") %>% 
  left_join(result_750_s3, by = "year") %>%
  left_join(result_1000_s3, by = "year")

result_0_s3<- sup_demand_s3(yr_candi[1], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97, tech_rate = 0.0025)
result_250_s3<- sup_demand_s3(yr_candi[2], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97, tech_rate = 0.0025)
result_500_s3<- sup_demand_s3(yr_candi[3], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97, tech_rate = 0.0025)
result_750_s3<- sup_demand_s3(yr_candi[4], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97, tech_rate = 0.0025)
result_1000_s3<- sup_demand_s3(yr_candi[5], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97, tech_rate = 0.0025)

## colnames changing
colnames(result_0_s3)[-c(1:3)] <-paste0(colnames(result_0_s3)[-c(1:3)], "_0" )
colnames(result_250_s3)[-c(1:3)] <-paste0(colnames(result_250_s3)[-c(1:3)], "_250" )
colnames(result_500_s3)[-c(1:3)] <-paste0(colnames(result_500_s3)[-c(1:3)], "_500" )
colnames(result_750_s3)[-c(1:3)] <-paste0(colnames(result_750_s3)[-c(1:3)], "_750" )
colnames(result_1000_s3)[-c(1:3)] <-paste0(colnames(result_1000_s3)[-c(1:3)], "_1000" )

result_s3_15y <- result_0_s3 %>% left_join(result_250_s3, by = "year") %>%
  left_join(result_500_s3, by = "year") %>% 
  left_join(result_750_s3, by = "year") %>%
  left_join(result_1000_s3, by = "year")

p <- ggplot(result_s3_5y, aes(x = year))+geom_line(aes(y = sup_demand_med_tech_0, col = "+0 (3018)")) +
  geom_line(aes(y = sup_demand_med_tech_250, col = "+250 (3268)")) + 
  geom_line(aes(y = sup_demand_med_tech_500, col = "+500 (3518)")) +
  geom_line(aes(y = sup_demand_med_tech_750, col = "+750 (3768)")) +
  geom_line(aes(y = sup_demand_med_tech_1000, col = "+1000 (4018)")) +
  ylab("") + 
  scale_color_manual(
    values = setNames(colorRampPalette(gradient_colors)(5), 
                      c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")),
    breaks = c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(color = "Supply Levels") +  # Custom legend title 
  ggtitle("5 years, 3% decreasing, 0.25% increasing") + ylim(c(-60000, 40000))
ggsave("sup_dem_5yr_025tech_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)

result_s3_5y$year[which(result_s3_5y$sup_demand_med_tech_0 < 0)]
result_s3_5y$sup_demand_med_tech_0[which.min(result_s3_5y$sup_demand_med_tech_0)]
result_s3_5y$year[which.min(result_s3_5y$sup_demand_med_tech_0)]

p <- ggplot(result_s3_10y, aes(x = year))+geom_line(aes(y = sup_demand_med_tech_0, col = "+0 (3018)")) +
  geom_line(aes(y = sup_demand_med_tech_250, col = "+250 (3268)")) + 
  geom_line(aes(y = sup_demand_med_tech_500, col = "+500 (3518)")) +
  geom_line(aes(y = sup_demand_med_tech_750, col = "+750 (3768)")) +
  geom_line(aes(y = sup_demand_med_tech_1000, col = "+1000 (4018)")) +
  ylab("") + 
  scale_color_manual(
    values = setNames(colorRampPalette(gradient_colors)(5), 
                      c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")),
    breaks = c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(color = "Supply Levels") +  # Custom legend title 
  ggtitle("10 years, 3% decreasing, 0.25% increasing") + ylim(c(-60000, 40000))
ggsave("sup_dem_10yr_025tech_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)


result_s3_10y$year[which(result_s3_10y$sup_demand_med_tech_0 < 0)]
result_s3_10y$sup_demand_med_tech_0[which.min(result_s3_10y$sup_demand_med_tech_0)]
result_s3_10y$year[which.min(result_s3_10y$sup_demand_med_tech_0)]

p <- ggplot(result_s3_15y, aes(x = year))+geom_line(aes(y = sup_demand_med_tech_0, col = "+0 (3018)")) +
  geom_line(aes(y = sup_demand_med_tech_250, col = "+250 (3268)")) + 
  geom_line(aes(y = sup_demand_med_tech_500, col = "+500 (3518)")) +
  geom_line(aes(y = sup_demand_med_tech_750, col = "+750 (3768)")) +
  geom_line(aes(y = sup_demand_med_tech_1000, col = "+1000 (4018)")) +
  ylab("") + 
  scale_color_manual(
    values = setNames(colorRampPalette(gradient_colors)(5), 
                      c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")),
    breaks = c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(color = "Supply Levels") +  # Custom legend title 
  ggtitle("15 years, 3% decreasing, 0.25% increasing") + ylim(c(-60000, 40000))
ggsave("sup_dem_15yr_025tech_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)


result_s3_15y$year[which(result_s3_15y$sup_demand_med_tech_0 < 0)]
result_s3_15y$sup_demand_med_tech_0[which.min(result_s3_15y$sup_demand_med_tech_0)]
result_s3_15y$year[which.min(result_s3_15y$sup_demand_med_tech_0)]




# S3: tech_rate: 0.5% -----------------------------------------------------
### results

result_0_s3<- sup_demand_s3(yr_candi[1], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97)
result_250_s3<- sup_demand_s3(yr_candi[2], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97)
result_500_s3<- sup_demand_s3(yr_candi[3], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97)
result_750_s3<- sup_demand_s3(yr_candi[4], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97)
result_1000_s3<- sup_demand_s3(yr_candi[5], 80, 0, demand_est_yr_s3, mid_yr = start_yr+5, reduce_rate = 0.97)

## colnames changing
colnames(result_0_s3)[-c(1:3)] <-paste0(colnames(result_0_s3)[-c(1:3)], "_0" )
colnames(result_250_s3)[-c(1:3)] <-paste0(colnames(result_250_s3)[-c(1:3)], "_250" )
colnames(result_500_s3)[-c(1:3)] <-paste0(colnames(result_500_s3)[-c(1:3)], "_500" )
colnames(result_750_s3)[-c(1:3)] <-paste0(colnames(result_750_s3)[-c(1:3)], "_750" )
colnames(result_1000_s3)[-c(1:3)] <-paste0(colnames(result_1000_s3)[-c(1:3)], "_1000" )

result_s3_5y <- result_0_s3 %>% left_join(result_250_s3, by = "year") %>%
  left_join(result_500_s3, by = "year") %>% 
  left_join(result_750_s3, by = "year") %>%
  left_join(result_1000_s3, by = "year")

result_0_s3<- sup_demand_s3(yr_candi[1], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97)
result_250_s3<- sup_demand_s3(yr_candi[2], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97)
result_500_s3<- sup_demand_s3(yr_candi[3], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97)
result_750_s3<- sup_demand_s3(yr_candi[4], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97)
result_1000_s3<- sup_demand_s3(yr_candi[5], 80, 0, demand_est_yr_s3, mid_yr = start_yr+10, reduce_rate = 0.97)

## colnames changing
colnames(result_0_s3)[-c(1:3)] <-paste0(colnames(result_0_s3)[-c(1:3)], "_0" )
colnames(result_250_s3)[-c(1:3)] <-paste0(colnames(result_250_s3)[-c(1:3)], "_250" )
colnames(result_500_s3)[-c(1:3)] <-paste0(colnames(result_500_s3)[-c(1:3)], "_500" )
colnames(result_750_s3)[-c(1:3)] <-paste0(colnames(result_750_s3)[-c(1:3)], "_750" )
colnames(result_1000_s3)[-c(1:3)] <-paste0(colnames(result_1000_s3)[-c(1:3)], "_1000" )

result_s3_10y <- result_0_s3 %>% left_join(result_250_s3, by = "year") %>%
  left_join(result_500_s3, by = "year") %>% 
  left_join(result_750_s3, by = "year") %>%
  left_join(result_1000_s3, by = "year")

result_0_s3<- sup_demand_s3(yr_candi[1], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97)
result_250_s3<- sup_demand_s3(yr_candi[2], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97)
result_500_s3<- sup_demand_s3(yr_candi[3], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97)
result_750_s3<- sup_demand_s3(yr_candi[4], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97)
result_1000_s3<- sup_demand_s3(yr_candi[5], 80, 0, demand_est_yr_s3, mid_yr = start_yr+15, reduce_rate = 0.97)

## colnames changing
colnames(result_0_s3)[-c(1:3)] <-paste0(colnames(result_0_s3)[-c(1:3)], "_0" )
colnames(result_250_s3)[-c(1:3)] <-paste0(colnames(result_250_s3)[-c(1:3)], "_250" )
colnames(result_500_s3)[-c(1:3)] <-paste0(colnames(result_500_s3)[-c(1:3)], "_500" )
colnames(result_750_s3)[-c(1:3)] <-paste0(colnames(result_750_s3)[-c(1:3)], "_750" )
colnames(result_1000_s3)[-c(1:3)] <-paste0(colnames(result_1000_s3)[-c(1:3)], "_1000" )

result_s3_15y <- result_0_s3 %>% left_join(result_250_s3, by = "year") %>%
  left_join(result_500_s3, by = "year") %>% 
  left_join(result_750_s3, by = "year") %>%
  left_join(result_1000_s3, by = "year")

p <- ggplot(result_s3_5y, aes(x = year))+geom_line(aes(y = sup_demand_med_tech_0, col = "+0 (3018)")) +
  geom_line(aes(y = sup_demand_med_tech_250, col = "+250 (3268)")) + 
  geom_line(aes(y = sup_demand_med_tech_500, col = "+500 (3518)")) +
  geom_line(aes(y = sup_demand_med_tech_750, col = "+750 (3768)")) +
  geom_line(aes(y = sup_demand_med_tech_1000, col = "+1000 (4018)")) +
  ylab("") + 
  scale_color_manual(
    values = setNames(colorRampPalette(gradient_colors)(5), 
                      c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")),
    breaks = c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(color = "Supply Levels") +  # Custom legend title 
  ggtitle("5 years, 3% decreasing, 0.5% increasing") + ylim(c(-60000, 40000))
ggsave("sup_dem_5yr_05tech_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)

result_s3_5y$year[which(result_s3_5y$sup_demand_med_tech_0 < 0)]
result_s3_5y$sup_demand_med_tech_0[which.min(result_s3_5y$sup_demand_med_tech_0)]
result_s3_5y$year[which.min(result_s3_5y$sup_demand_med_tech_0)]

p <- ggplot(result_s3_10y, aes(x = year))+geom_line(aes(y = sup_demand_med_tech_0, col = "+0 (3018)")) +
  geom_line(aes(y = sup_demand_med_tech_250, col = "+250 (3268)")) + 
  geom_line(aes(y = sup_demand_med_tech_500, col = "+500 (3518)")) +
  geom_line(aes(y = sup_demand_med_tech_750, col = "+750 (3768)")) +
  geom_line(aes(y = sup_demand_med_tech_1000, col = "+1000 (4018)")) +
  ylab("") + 
  scale_color_manual(
    values = setNames(colorRampPalette(gradient_colors)(5), 
                      c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")),
    breaks = c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(color = "Supply Levels") +  # Custom legend title 
  ggtitle("10 years, 3% decreasing, 0.5% increasing") + ylim(c(-60000, 40000))
ggsave("sup_dem_10yr_05tech_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)


result_s3_10y$year[which(result_s3_10y$sup_demand_med_tech_0 < 0)]
result_s3_10y$sup_demand_med_tech_0[which.min(result_s3_10y$sup_demand_med_tech_0)]
result_s3_10y$year[which.min(result_s3_10y$sup_demand_med_tech_0)]

p <- ggplot(result_s3_15y, aes(x = year))+geom_line(aes(y = sup_demand_med_tech_0, col = "+0 (3018)")) +
  geom_line(aes(y = sup_demand_med_tech_250, col = "+250 (3268)")) + 
  geom_line(aes(y = sup_demand_med_tech_500, col = "+500 (3518)")) +
  geom_line(aes(y = sup_demand_med_tech_750, col = "+750 (3768)")) +
  geom_line(aes(y = sup_demand_med_tech_1000, col = "+1000 (4018)")) +
  ylab("") + 
  scale_color_manual(
    values = setNames(colorRampPalette(gradient_colors)(5), 
                      c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")),
    breaks = c("+0 (3018)", "+250 (3268)", "+500 (3518)", "+750 (3768)", "+1000 (4018)")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  labs(color = "Supply Levels") +  # Custom legend title 
  ggtitle("15 years, 3% decreasing, 0.5% increasing") + ylim(c(-60000, 40000))
ggsave("sup_dem_15yr_05tech_s3.png", plot = p, path = plot_dir, width = 8, height = 5, dpi = 600)


result_s3_15y$year[which(result_s3_15y$sup_demand_med_tech_0 < 0)]
result_s3_15y$sup_demand_med_tech_0[which.min(result_s3_15y$sup_demand_med_tech_0)]
result_s3_15y$year[which.min(result_s3_15y$sup_demand_med_tech_0)]