library(haven)
library(dplyr)
library(rstatix)


data1 = read_dta(file.choose())
data1$lang_home = as_factor(data1$lang_home)
data1$medium_instruction = as_factor(data1$medium_instruction)
data1$sector = as_factor(data1$sector)
data1$indicator = ifelse(as.numeric(data1$medium_instruction) == as.numeric(data1$lang_home),
                         1,
                         0)
# Rural + Urban, Country and State
dat_1 = subset(data1, age %in% c(5:10))
dat_2 = subset(data1, age %in% c(11:15))
dat_3 = subset(data1, age %in% c(16:20))
dat_4 = subset(data1, age %in% c(21:25))
dat_5 = subset(data1, age %in% c(26:29))

data1 <- data1 %>% arrange(HH_ID)

dat_cha = subset(data1, state_cd == "22")

dat_cha_1 = subset(dat_cha, age %in% c(5:10))
dat_cha_2 = subset(dat_cha, age %in% c(11:15))
dat_cha_3 = subset(dat_cha, age %in% c(16:20))
dat_cha_4 = subset(dat_cha, age %in% c(21:25))
dat_cha_5 = subset(dat_cha, age %in% c(26:29))


# Rural, Country and State
data1R = subset(data1, sector == "Rural")
dat_1R = subset(data1R, age %in% c(5:10))
dat_2R = subset(data1R, age %in% c(11:15))
dat_3R = subset(data1R, age %in% c(16:20))
dat_4R = subset(data1R, age %in% c(21:25))
dat_5R = subset(data1R, age %in% c(26:29))

dat_chaR = subset(dat_cha, sector == "Rural")

dat_cha_1R = subset(dat_chaR, age %in% c(5:10))
dat_cha_2R = subset(dat_chaR, age %in% c(11:15))
dat_cha_3R = subset(dat_chaR, age %in% c(16:20))
dat_cha_4R = subset(dat_chaR, age %in% c(21:25))
dat_cha_5R = subset(dat_chaR, age %in% c(26:29))

# Urban, Country and State
data1U = subset(data1, sector == "Urban")
dat_1U = subset(data1U, age %in% c(5:10))
dat_2U = subset(data1U, age %in% c(11:15))
dat_3U = subset(data1U, age %in% c(16:20))
dat_4U = subset(data1U, age %in% c(21:25))
dat_5U = subset(data1U, age %in% c(26:29))

dat_chaU = subset(dat_cha, sector == "Urban")

dat_cha_1U = subset(dat_chaR, age %in% c(5:10))
dat_cha_2U = subset(dat_chaR, age %in% c(11:15))
dat_cha_3U = subset(dat_chaR, age %in% c(16:20))
dat_cha_4U = subset(dat_chaR, age %in% c(21:25))
dat_cha_5U = subset(dat_chaR, age %in% c(26:29))

#Estimate and Variance of the number of same

estimate = function(dat_cha) {
  sum(dat_cha$indicator * dat_cha$wgt_combined)
}


variance = function(dat_cha) {
  dat_cha_split <- dat_cha %>%
    df_split_by(stratum) #stratum
  e = 0
  for (s in 1:length(dat_cha_split$data)) {
    es = 0
    dat_cha_s = dat_cha_split$data[[s]]
    dat_cha_s_split = dat_cha_s %>% df_split_by(sstratum) #substratum
    for (t in 1:length(dat_cha_s_split$data)) {
      est = 0
      dat_cha_st = dat_cha_s_split$data[[t]]
      dat_cha_st_split = dat_cha_st %>% df_split_by(ssample) #sub sample
      for (m in 1:length(dat_cha_st_split$data)) {
        estm = 0
        dat_cha_stm = dat_cha_st_split$data[[m]]
        dat_cha_stm_split = dat_cha_stm %>% df_split_by(sss) #second stage stratum
        for (j in 1:length(dat_cha_stm_split$data)) {
          estmj = 0
          dat_cha_stmj = dat_cha_stm_split$data[[j]]
          dat_cha_stmj_split = dat_cha_stmj %>% df_split_by(fsu) #fsu
          for (i in 1:length(dat_cha_stmj_split$data)) {
            estmji = 0
            dat_cha_stmji = dat_cha_stmj_split$data[[i]]
            dat_cha_stmji_split = dat_cha_stmji %>% df_split_by(hamlet_grp) #hamlet_grp
            for (d in 1:length(dat_cha_stmji_split$data)) {
              estmjid = 0
              dat_cha_stmjid = dat_cha_stmji_split$data[[d]]
              estmjid = sum(dat_cha_stmjid$indicator * dat_cha_stmjid$wgt_ss) #all households
              estmji = estmji + estmjid
            }
            estmj = estmj + estmji
          }
          estm = estm + estmj
        }
        est = (est - estm) ^ 2
      }
      es = es + est
    }
    e = e + es
  }
  return(e / 4)
}

#Estimate and Variance of the Percentages

estimatelang = function(dat_cha) {
  sum(dat_cha$wgt_combined)
}


variancelanghome = function(dat_cha, lang) {
  dat_cha_split <- dat_cha %>%
    df_split_by(stratum) #stratum
  ve = 0
  for (s in 1:length(dat_cha_split$data)) {
    ves = 0
    dat_cha_s = dat_cha_split$data[[s]]
    dat_cha_s_split = dat_cha_s %>% df_split_by(sstratum) #substratum
    for (t in 1:length(dat_cha_s_split$data)) {
      vest = 0
      est = 0
      xest = 0
      est1 = 0
      est2 = 0
      xest1 = 0
      xest2 = 0
      dat_cha_st = dat_cha_s_split$data[[t]]
      dat_cha_st_split = dat_cha_st %>% df_split_by(ssample) #sub sample
        xest1 = 0
        estm1= 0
        dat_cha_st1 = dat_cha_st_split$data[[1]]
        dat_cha_st1_split = dat_cha_st1 %>% df_split_by(sss) #second stage stratum
        for (j in 1:length(dat_cha_st1_split$data)) {
          xest1j = 0
          est1j = 0
          dat_cha_st1j = dat_cha_st1_split$data[[j]]
          dat_cha_st1j_split = dat_cha_st1j %>% df_split_by(fsu) #fsu
          for (i in 1:length(dat_cha_st1j_split$data)) {
            xest1ji = 0
            est1ji = 0
            dat_cha_st1ji = dat_cha_st1j_split$data[[i]]
            dat_cha_st1ji_split = dat_cha_st1ji %>% df_split_by(hamlet_grp) #hamlet_grp
            for (d in 1:length(dat_cha_st1ji_split$data)) {
              xest1jid = 0
              est1jid = 0
              dat_cha_st1jid = dat_cha_st1ji_split$data[[d]]
              est1jid = sum(dat_cha_st1jid$wgt_ss) #all households
              est1ji = est1ji + est1jid
              datlang = subset(dat_cha_st1jid, lang_home == lang)
              if (dim(datlang)[1] == 0)
                xest1jd = 0
              else
                xest1jd = sum(datlang$wgt_ss)
            }
            est1j = est1j + est1ji
            xest1j = xest1j + xest1jd
          }
          est1 = est1 + est1j
          xest1 = xest1 + xest1j
        }
        if(length(dat_cha_st_split$data) == 2){
          xest2 = 0
          estm2= 0
          dat_cha_st2 = dat_cha_st_split$data[[2]]
          dat_cha_st2_split = dat_cha_st2 %>% df_split_by(sss) #second stage stratum
          for (j in 1:length(dat_cha_st2_split$data)) {
            xest2j = 0
            est2j = 0
            dat_cha_st2j = dat_cha_st2_split$data[[j]]
            dat_cha_st2j_split = dat_cha_st2j %>% df_split_by(fsu) #fsu
            for (i in 1:length(dat_cha_st2j_split$data)) {
              xest2ji = 0
              est2ji = 0
              dat_cha_st2ji = dat_cha_st2j_split$data[[i]]
              dat_cha_st2ji_split = dat_cha_st2ji %>% df_split_by(hamlet_grp) #hamlet_grp
              for (d in 1:length(dat_cha_st2ji_split$data)) {
                xest2jid = 0
                est2jid = 0
                dat_cha_st2jid = dat_cha_st2ji_split$data[[d]]
                est2jid = sum(dat_cha_st2jid$wgt_ss) #all households
                est2ji = est2ji + est2jid
                datlang = subset(dat_cha_st2jid, lang_home == lang)
                if (dim(datlang)[1] == 0)
                  xest2jd = 0
                else
                  xest2jd = sum(datlang$wgt_ss)
              }
              est2j = est2j + est2ji
              xest2j = xest2j + xest2jd
            }
            est2 = est2 + est2j
            xest2 = xest2 + xest2j
          }
        }
        
        else est2 = xest2 = 0
        
        xest = (xest1 - xest2)
        est = (est1 - est2)
        
      
        vest = vest + estimatelang(datlang) ^ 2 * est^2 + xest^2
           - 2 * estimatelang(datlang) * xest * est
      ves = ves + vest
    }
    ve = ve + ves
  }
  return(ve / 4)
}


variancelangschool = function(dat_cha, lang) {
  dat_cha_split <- dat_cha %>%
    df_split_by(stratum) #stratum
  ve = 0
  for (s in 1:length(dat_cha_split$data)) {
    ves = 0
    dat_cha_s = dat_cha_split$data[[s]]
    dat_cha_s_split = dat_cha_s %>% df_split_by(sstratum) #substratum
    for (t in 1:length(dat_cha_s_split$data)) {
      vest = 0
      est = 0
      xest = 0
      est1 = 0
      est2 = 0
      xest1 = 0
      xest2 = 0
      dat_cha_st = dat_cha_s_split$data[[t]]
      dat_cha_st_split = dat_cha_st %>% df_split_by(ssample) #sub sample
      xest1 = 0
      estm1= 0
      dat_cha_st1 = dat_cha_st_split$data[[1]]
      dat_cha_st1_split = dat_cha_st1 %>% df_split_by(sss) #second stage stratum
      for (j in 1:length(dat_cha_st1_split$data)) {
        xest1j = 0
        est1j = 0
        dat_cha_st1j = dat_cha_st1_split$data[[j]]
        dat_cha_st1j_split = dat_cha_st1j %>% df_split_by(fsu) #fsu
        for (i in 1:length(dat_cha_st1j_split$data)) {
          xest1ji = 0
          est1ji = 0
          dat_cha_st1ji = dat_cha_st1j_split$data[[i]]
          dat_cha_st1ji_split = dat_cha_st1ji %>% df_split_by(hamlet_grp) #hamlet_grp
          for (d in 1:length(dat_cha_st1ji_split$data)) {
            xest1jid = 0
            est1jid = 0
            dat_cha_st1jid = dat_cha_st1ji_split$data[[d]]
            est1jid = sum(dat_cha_st1jid$wgt_ss) #all households
            est1ji = est1ji + est1jid
            datlang = subset(dat_cha_st1jid, lang_home == lang)
            if (dim(datlang)[1] == 0)
              xest1jd = 0
            else
              xest1jd = sum(datlang$wgt_ss)
          }
          est1j = est1j + est1ji
          xest1j = xest1j + xest1jd
        }
        est1 = est1 + est1j
        xest1 = xest1 + xest1j
      }
      if(length(dat_cha_st_split$data) == 2){
        xest2 = 0
        estm2= 0
        dat_cha_st2 = dat_cha_st_split$data[[2]]
        dat_cha_st2_split = dat_cha_st2 %>% df_split_by(sss) #second stage stratum
        for (j in 1:length(dat_cha_st2_split$data)) {
          xest2j = 0
          est2j = 0
          dat_cha_st2j = dat_cha_st2_split$data[[j]]
          dat_cha_st2j_split = dat_cha_st2j %>% df_split_by(fsu) #fsu
          for (i in 1:length(dat_cha_st2j_split$data)) {
            xest2ji = 0
            est2ji = 0
            dat_cha_st2ji = dat_cha_st2j_split$data[[i]]
            dat_cha_st2ji_split = dat_cha_st2ji %>% df_split_by(hamlet_grp) #hamlet_grp
            for (d in 1:length(dat_cha_st2ji_split$data)) {
              xest2jid = 0
              est2jid = 0
              dat_cha_st2jid = dat_cha_st2ji_split$data[[d]]
              est2jid = sum(dat_cha_st2jid$wgt_ss) #all households
              est2ji = est2ji + est2jid
              datlang = subset(dat_cha_st2jid, medium_instruction == lang)
              if (dim(datlang)[1] == 0)
                xest2jd = 0
              else
                xest2jd = sum(datlang$wgt_ss)
            }
            est2j = est2j + est2ji
            xest2j = xest2j + xest2jd
          }
          est2 = est2 + est2j
          xest2 = xest2 + xest2j
        }
      }
      
      else est2 = xest2 = 0
      
      xest = (xest1 - xest2)
      est = (est1 - est2)
      
      
      vest = vest + estimatelang(datlang) ^ 2 * est^2 + xest^2
      - 2 * estimatelang(datlang) * xest * est
      ves = ves + vest
    }
    ve = ve + ves
  }
  return(ve / 4)
}


#Extracting all the languages

alllang_cha = union(unique(dat_cha$lang_home),
                    unique(dat_cha$medium_instruction))

alllang_ind = union(unique(data1$lang_home),
                    unique(data1$medium_instruction))


#Function for giving the percentages language wise
langhomestatistics = function(data, alllang) {
  vect = numeric(length(alllang))
  k = 0
  for (lang in alllang) {
    k = k + 1
    datlang = subset(data, lang_home == lang)
    if (dim(datlang)[1] == 0)
      vect[k] = 0
    else
      vect[k] = estimatelang(datlang)
  }
  return (vect)
}

langschoolstatistics = function(data, alllang) {
  vect = numeric(length(alllang))
  k = 0
  for (lang in alllang) {
    k = k + 1
    datlang = subset(data, medium_instruction == lang)
    if (dim(datlang)[1] == 0)
      vect[k] = 0
    else
      vect[k] = estimatelang(datlang)
  }
  return (vect)
}


#Making the data frames
#Rural + Urban (Country)

langhome_ind = as.data.frame(
  cbind(
    alllang_ind,
    langhomestatistics(dat_1, alllang_ind)/sum(langhomestatistics(dat_1, alllang_ind)),
    langhomestatistics(dat_2, alllang_ind)/sum(langhomestatistics(dat_2, alllang_ind)),
    langhomestatistics(dat_3, alllang_ind)/sum(langhomestatistics(dat_3, alllang_ind)),
    langhomestatistics(dat_4, alllang_ind)/sum(langhomestatistics(dat_4, alllang_ind)),
    langhomestatistics(dat_5, alllang_ind)/sum(langhomestatistics(dat_5, alllang_ind))
  )
)

langschool_ind = as.data.frame(
  cbind(
    alllang_ind,
    langschoolstatistics(dat_1, alllang_ind)/sum(langschoolstatistics(dat_1, alllang_ind)),
    langschoolstatistics(dat_2, alllang_ind)/sum(langschoolstatistics(dat_2, alllang_ind)),
    langschoolstatistics(dat_3, alllang_ind)/sum(langschoolstatistics(dat_3, alllang_ind)),
    langschoolstatistics(dat_4, alllang_ind)/sum(langschoolstatistics(dat_4, alllang_ind)),
    langschoolstatistics(dat_5, alllang_ind)/sum(langschoolstatistics(dat_5, alllang_ind))
  )
)

colnames(langhome_ind) = colnames(langschool_ind) = c("languages", "age_5_10" , "age_11_15" ,"age_16_20", "age_21_25", "age_26_29")

#Rural (Country)

langhome_indR = as.data.frame(
  cbind(
    alllang_ind,
    langhomestatistics(dat_1R, alllang_ind)/sum(langhomestatistics(dat_1R, alllang_ind)),
    langhomestatistics(dat_2R, alllang_ind)/sum(langhomestatistics(dat_2R, alllang_ind)),
    langhomestatistics(dat_3R, alllang_ind)/sum(langhomestatistics(dat_3R, alllang_ind)),
    langhomestatistics(dat_4R, alllang_ind)/sum(langhomestatistics(dat_4R, alllang_ind)),
    langhomestatistics(dat_5R, alllang_ind)/sum(langhomestatistics(dat_5R, alllang_ind))
  )
)

langschool_indR = as.data.frame(
  cbind(
    alllang_ind,
    langschoolstatistics(dat_1R, alllang_ind)/sum(langschoolstatistics(dat_1R, alllang_ind)),
    langschoolstatistics(dat_2R, alllang_ind)/sum(langschoolstatistics(dat_2R, alllang_ind)),
    langschoolstatistics(dat_3R, alllang_ind)/sum(langschoolstatistics(dat_3R, alllang_ind)),
    langschoolstatistics(dat_4R, alllang_ind)/sum(langschoolstatistics(dat_4R, alllang_ind)),
    langschoolstatistics(dat_5R, alllang_ind)/sum(langschoolstatistics(dat_5R, alllang_ind))
  )
)

colnames(langhome_indR) = colnames(langschool_indR) = c("languages", "age_5_10" , "age_11_15" ,"age_16_20", "age_21_25", "age_26_29")

#Urban (Country)

langhome_indU = as.data.frame(
  cbind(
    alllang_ind,
    langhomestatistics(dat_1U, alllang_ind)/sum(langhomestatistics(dat_1U, alllang_ind)),
    langhomestatistics(dat_2U, alllang_ind)/sum(langhomestatistics(dat_2U, alllang_ind)),
    langhomestatistics(dat_3U, alllang_ind)/sum(langhomestatistics(dat_3U, alllang_ind)),
    langhomestatistics(dat_4U, alllang_ind)/sum(langhomestatistics(dat_4U, alllang_ind)),
    langhomestatistics(dat_5U, alllang_ind)/sum(langhomestatistics(dat_5U, alllang_ind))
  )
)

langschool_indU = as.data.frame(
  cbind(
    alllang_ind,
    langschoolstatistics(dat_1U, alllang_ind)/sum(langschoolstatistics(dat_1U, alllang_ind)),
    langschoolstatistics(dat_2U, alllang_ind)/sum(langschoolstatistics(dat_2U, alllang_ind)),
    langschoolstatistics(dat_3U, alllang_ind)/sum(langschoolstatistics(dat_3U, alllang_ind)),
    langschoolstatistics(dat_4U, alllang_ind)/sum(langschoolstatistics(dat_4U, alllang_ind)),
    langschoolstatistics(dat_5U, alllang_ind)/sum(langschoolstatistics(dat_5U, alllang_ind))
  )
)

colnames(langhome_indU) = colnames(langschool_indU) = c("languages", "age_5_10" , "age_11_15" ,"age_16_20", "age_21_25", "age_26_29")


#Rural + Urban (State)
langhome_cha = as.data.frame(
  cbind(
    alllang_cha,
    langhomestatistics(dat_cha_1, alllang_cha)/sum(langhomestatistics(dat_cha_1, alllang_cha)),
    langhomestatistics(dat_cha_2, alllang_cha)/sum(langhomestatistics(dat_cha_2, alllang_cha)),
    langhomestatistics(dat_cha_3, alllang_cha)/sum(langhomestatistics(dat_cha_3, alllang_cha)),
    langhomestatistics(dat_cha_4, alllang_cha)/sum(langhomestatistics(dat_cha_4, alllang_cha)),
    langhomestatistics(dat_cha_5, alllang_cha)/sum(langhomestatistics(dat_cha_5, alllang_cha))
  )
)

langschool_cha = as.data.frame(
  cbind(
    alllang_cha,
    langschoolstatistics(dat_cha_1, alllang_cha) / sum(langschoolstatistics(dat_cha_1, alllang_cha)),
    langschoolstatistics(dat_cha_2, alllang_cha) / sum(langschoolstatistics(dat_cha_2, alllang_cha)),
    langschoolstatistics(dat_cha_3, alllang_cha) / sum(langschoolstatistics(dat_cha_3, alllang_cha)),
    langschoolstatistics(dat_cha_4, alllang_cha) / sum(langschoolstatistics(dat_cha_4, alllang_cha)),
    langschoolstatistics(dat_cha_5, alllang_cha) / sum(langschoolstatistics(dat_cha_5, alllang_cha))
  )
)


colnames(langhome_cha) = colnames(langschool_cha) = c("languages", "age_5_10" , "age_11_15" ,"age_16_20", "age_21_25", "age_26_29")

#Rural (State)

langhome_chaR = as.data.frame(
  cbind(
    alllang_cha,
    langhomestatistics(dat_cha_1R, alllang_cha) / sum(langhomestatistics(dat_cha_1R, alllang_cha)),
    langhomestatistics(dat_cha_2R, alllang_cha) / sum(langhomestatistics(dat_cha_2R, alllang_cha)),
    langhomestatistics(dat_cha_3R, alllang_cha) / sum(langhomestatistics(dat_cha_3R, alllang_cha)),
    langhomestatistics(dat_cha_4R, alllang_cha) / sum(langhomestatistics(dat_cha_4R, alllang_cha)),
    langhomestatistics(dat_cha_5R, alllang_cha) / sum(langhomestatistics(dat_cha_5R, alllang_cha))
  )
)


langschool_chaR = as.data.frame(
  cbind(
    alllang_cha,
    langschoolstatistics(dat_cha_1R, alllang_cha) / sum(langschoolstatistics(dat_cha_1R, alllang_cha)),
    langschoolstatistics(dat_cha_2R, alllang_cha) / sum(langschoolstatistics(dat_cha_2R, alllang_cha)),
    langschoolstatistics(dat_cha_3R, alllang_cha) / sum(langschoolstatistics(dat_cha_3R, alllang_cha)),
    langschoolstatistics(dat_cha_4R, alllang_cha) / sum(langschoolstatistics(dat_cha_4R, alllang_cha)),
    langschoolstatistics(dat_cha_5R, alllang_cha) / sum(langschoolstatistics(dat_cha_5R, alllang_cha))
  )
)

colnames(langhome_chaR) = colnames(langschool_chaR) = c("languages", "age_5_10" , "age_11_15" ,"age_16_20", "age_21_25", "age_26_29")

#Urban (Country)

langhome_chaU = as.data.frame(
  cbind(
    alllang_cha,
    langhomestatistics(dat_cha_1U, alllang_cha) / sum(langhomestatistics(dat_cha_1U, alllang_cha)),
    langhomestatistics(dat_cha_2U, alllang_cha) / sum(langhomestatistics(dat_cha_2U, alllang_cha)),
    langhomestatistics(dat_cha_3U, alllang_cha) / sum(langhomestatistics(dat_cha_3U, alllang_cha)),
    langhomestatistics(dat_cha_4U, alllang_cha) / sum(langhomestatistics(dat_cha_4U, alllang_cha)),
    langhomestatistics(dat_cha_5U, alllang_cha) / sum(langhomestatistics(dat_cha_5U, alllang_cha))
  )
)


langschool_chaU = as.data.frame(
  cbind(
    alllang_cha,
    langschoolstatistics(dat_cha_1U, alllang_cha) / sum(langschoolstatistics(dat_cha_1U, alllang_cha)),
    langschoolstatistics(dat_cha_2U, alllang_cha) / sum(langschoolstatistics(dat_cha_2U, alllang_cha)),
    langschoolstatistics(dat_cha_3U, alllang_cha) / sum(langschoolstatistics(dat_cha_3U, alllang_cha)),
    langschoolstatistics(dat_cha_4U, alllang_cha) / sum(langschoolstatistics(dat_cha_4U, alllang_cha)),
    langschoolstatistics(dat_cha_5U, alllang_cha) / sum(langschoolstatistics(dat_cha_5U, alllang_cha))
  )
)


colnames(langhome_chaU) = colnames(langschool_chaU) = c("languages", "age_5_10" , "age_11_15" ,"age_16_20", "age_21_25", "age_26_29")


### variances

langhomevar = function(data, alllang) {
  vect = numeric(length(alllang))
  k = 0
  for (lang in alllang) {
    k = k + 1
    datlang = subset(data, lang_home == lang)
    if (dim(datlang)[1] == 0)
      vect[k] = 0
    else
      vect[k] = variancelanghome(data, lang)/(sum(langhomestatistics(data, alllang)))^2
  }
  return (vect)
}

langschoolvar = function(data, alllang) {
  vect = numeric(length(alllang))
  k = 0
  for (lang in alllang) {
    k = k + 1
    datlang = subset(data, medium_instruction == lang)
    if (dim(datlang)[1] == 0)
      vect[k] = 0
    else
      vect[k] = variancelangschool(data, lang)/(sum(langschoolstatistics(data, alllang)))^2
  }
  return (vect)
}



