library(haven)
library(dplyr)
library(rstatix)


data1 = read_dta(file.choose())
df1 = read_dta(file.choose())
df2 = df1
df1 = df2[,c("HH_ID","psrl_no","sex")]
result <- merge(data1, df1, by = c("HH_ID", "psrl_no"))
data1 = result
data1$lang_home = as_factor(data1$lang_home)
data1$medium_instruction = as_factor(data1$medium_instruction)
data1$sector = as_factor(data1$sector)
data1$indicator = ifelse(as.numeric(data1$medium_instruction) == as.numeric(data1$lang_home),
                         1,
                         0)
data1$sex = as_factor(data1$sex)
data1 <- data1 %>% arrange(HH_ID)

dat_cha = subset(data1, state_cd == "22")

estimate = function(data) {
  sum(data$wgt_combined)
}

data1R = subset(data1, sector == "Rural")
data1U = subset(data1, sector == "Urban")
dat_chaR = subset(dat_cha, sector == "Rural")
dat_chaU = subset(dat_cha, sector == "Urban")
data1M = subset(data1, sex == "Male")
data1F = subset(data1, sex == "Female")
dat_chaM = subset(dat_cha, sex == "Male")
dat_chaF = subset(dat_cha, sex == "Female")
data1RM = subset(data1R, sex == "Male")
data1RF = subset(data1R, sex == "Female")
data1UM = subset(data1U, sex == "Male")
data1UF = subset(data1U, sex == "Female")
dat_chaRM = subset(dat_chaR, sex == "Male")
dat_chaRF = subset(dat_chaR, sex == "Female")
dat_chaUM = subset(dat_chaU, sex == "Male")
dat_chaUF = subset(dat_chaU, sex == "Female")


alllang_cha = union(unique(dat_cha$lang_home),
                    unique(dat_cha$medium_instruction))

alllang_ind = union(unique(data1$lang_home),
                    unique(data1$medium_instruction))

estimatelang = function(data, alllang){
  A = matrix(0,nrow=length(alllang),ncol=length(alllang))
  for(i in 1:length(alllang)){
    for(j in 1:length(alllang)){
      A[i,j] = estimate(subset(data, lang_home == alllang[i] & medium_instruction == alllang[j]))
    }
  }
  colnames(A)<- rownames(A) <- alllang
  return(A/sum(A))
}

variancelang1lang2 = function(data, lang1, lang2){
  R = (estimate(subset(data, lang_home == lang1 & medium_instruction == lang2))/estimate(data))
  data_split <- data %>%
    df_split_by(stratum) #stratum
  var = 0
  for(s in 1:length(data_split$data)){
    vars = 0
    data_s = data_split$data[[s]]
    data_s_split = data_s %>% df_split_by(sstratum) #substratum
    for(t in 1:length(data_s_split$data)){
      varst = 0
      data_st = data_s_split$data[[t]]
      data_st_split = data_st %>% df_split_by(ssample) #sub sample
      yest1 = estimate(subset(data_st_split$data[[1]], lang_home == lang1 & medium_instruction == lang2))
      xest1 = estimate(data_st_split$data[[1]])
      if(length(data_st_split$data) < 2) yest2 = xest2 = 0
      else{
        yest2 = estimate(subset(data_st_split$data[[2]], lang_home == lang1 & medium_instruction == lang2))
        xest2 = estimate(data_st_split$data[[2]])
      }
      yest = yest1 - yest2
      xest = xest1 - xest2
      varst = yest^2 +R^2*xest^2-2*R*yest*xest
      vars = vars + varst
    }
    var = var + vars 
  }
  return(var/(4*estimate(data)^2))
}


variancelang = function(data, alllang){
  A = matrix(0,nrow=length(alllang),ncol=length(alllang))
  for(i in 1:length(alllang)){
    for(j in 1:length(alllang)){
      A[i,j] = variancelang1lang2(data, alllang[i], alllang[j])
    }
    print(i)
  }
  colnames(A)<- rownames(A) <- alllang
  return(A)
}


est_ind = estimatelang(data1, alllang_ind)
write.csv(est_ind, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind.csv")

est_ind_M = estimatelang(data1M, alllang_ind)
write.csv(est_ind_M, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_M.csv")

est_ind_F = estimatelang(data1F, alllang_ind)
write.csv(est_ind_F, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_F.csv")

est_ind_R = estimatelang(data1R, alllang_ind)
write.csv(est_ind_R, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_R.csv")

est_ind_U = estimatelang(data1U, alllang_ind)
write.csv(est_ind_U, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_U.csv")

est_ind_RM = estimatelang(data1RM, alllang_ind)
write.csv(est_ind_RM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_RM.csv")

est_ind_RF = estimatelang(data1RF, alllang_ind)
write.csv(est_ind_RF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_RF.csv")

est_ind_UM = estimatelang(data1UM, alllang_ind)  # Note: dataUM is not defined in your provided context
write.csv(est_ind_UM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_UM.csv")

est_ind_UF = estimatelang(data1UF, alllang_ind)
write.csv(est_ind_UF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_ind_UF.csv")

est_cha = estimatelang(dat_cha, alllang_cha)
write.csv(est_cha, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha.csv")

est_cha_M = estimatelang(dat_chaM, alllang_cha)
write.csv(est_cha_M, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_M.csv")

est_cha_F = estimatelang(dat_chaF, alllang_cha)
write.csv(est_cha_F, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_F.csv")

est_cha_R = estimatelang(dat_chaR, alllang_cha)
write.csv(est_cha_R, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_R.csv")

est_cha_U = estimatelang(dat_chaU, alllang_cha)
write.csv(est_cha_U, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_U.csv")

est_cha_RM = estimatelang(dat_chaRM, alllang_cha)
write.csv(est_cha_RM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_RM.csv")

est_cha_RF = estimatelang(dat_chaRF, alllang_cha)
write.csv(est_cha_RF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_RF.csv")

est_cha_UM = estimatelang(dat_chaUM, alllang_cha)
write.csv(est_cha_UM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_UM.csv")

est_cha_UF = estimatelang(dat_chaUF, alllang_cha)
write.csv(est_cha_UF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/Estimates/est_cha_UF.csv")


######## For variances

var_ind = variancelang(data1, alllang_ind)
write.csv(var_ind, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind.csv")

var_ind_M = variancelang(data1M, alllang_ind)
write.csv(var_ind_M, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_M.csv")

var_ind_F = variancelang(data1F, alllang_ind)
write.csv(var_ind_F, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_F.csv")

var_ind_R = variancelang(data1R, alllang_ind)
write.csv(var_ind_R, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_R.csv")

var_ind_U = variancelang(data1U, alllang_ind)
write.csv(var_ind_U, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_U.csv")

var_ind_RM = variancelang(data1RM, alllang_ind)
write.csv(var_ind_RM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_RM.csv")

var_ind_RF = variancelang(data1RF, alllang_ind)
write.csv(var_ind_RF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_RF.csv")

var_ind_UM = variancelang(data1UM, alllang_ind)  # Note: dataUM is not defined in your provided context
write.csv(var_ind_UM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_UM.csv")

var_ind_UF = variancelang(data1UF, alllang_ind)
write.csv(var_ind_UF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_ind_UF.csv")

var_cha = variancelang(dat_cha, alllang_cha)
write.csv(var_cha, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha.csv")

var_cha_M = variancelang(dat_chaM, alllang_cha)
write.csv(var_cha_M, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_M.csv")

var_cha_F = variancelang(dat_chaF, alllang_cha)
write.csv(var_cha_F, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_F.csv")

var_cha_R = variancelang(dat_chaR, alllang_cha)
write.csv(var_cha_R, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_R.csv")

var_cha_U = variancelang(dat_chaU, alllang_cha)
write.csv(var_cha_U, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_U.csv")

var_cha_RM = variancelang(dat_chaRM, alllang_cha)
write.csv(var_cha_RM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_RM.csv")

var_cha_RF = variancelang(dat_chaRF, alllang_cha)
write.csv(var_cha_RF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_RF.csv")

var_cha_UM = variancelang(dat_chaUM, alllang_cha)
write.csv(var_cha_UM, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_UM.csv")

var_cha_UF = variancelang(dat_chaUF, alllang_cha)
write.csv(var_cha_UF, "F:/B3_resources/ass_or_hw/Sample Survey/Proj2/Results/variances/var_cha_UF.csv")



data1$private_coaching = as_factor(data1$private_coaching)

private = subset(data1, private_coaching == "Yes")

reasons = na.omit(private$reason_private)


private_cha = subset(private, state_cd == "22")

reasons_cha = na.omit(private_cha$reason_private)


private_R = subset(private, sector == "Rural")
reasons_cha_R = na.omit(private_R$reason_private)

reason_labels <- c("Government institution is not available nearby",
                   "Better environment of learning",
                   "English is the medium of instruction",
                   "Quality of education in govt. institution not satisfactory",
                   "Tried for government institution but could not get admission",
                   "Cannot say")

# Create a data frame with counts for each reason
reasons_df <- data.frame(table(reasons_ind_R))

# Map labels to reasons
reasons_df$reasons <- factor(reasons_df$reasons, levels = 1:6, labels = reason_labels)

private_U = subset(private, sector == "Urban")
reasons_cha_U = na.omit(private_U$reason_private)

ggplot(reasons_df, aes(x = reasons, y = Freq, fill = reasons)) +
  geom_bar(stat  = "identity", color = "black") +
  labs(title = "Reasons for Preferring Private Institution (Rural India)",
       x = "Reasons",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_brewer(palette = "Set3")  # You can choose a different color palette if needed


private_R = subset(private_R, sector == "Rural")
reasons_ind_R = na.omit(private_R$reason_private)
