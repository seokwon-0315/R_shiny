# ML FCST와 KAM FCST 비교

#KAM FCST DATA Load & preprocessing ---------------------------------------------------------------------------------------------------------------------
kam_fcst <- vroom('./KAM_FCST/W47 Sell-Out Plan_Detail.csv')
kam_fcst <- data.table(kam_fcst)

#account 이름 변경
kam_fcst[`P Unit` == 'JOHN LEWIS', `P Unit` := 'JOHNLEWIS']
kam_fcst[`P Unit` == 'Carrefour', `P Unit` := 'CARREFOUR']
kam_fcst[PG == 'W/M', PG := 'WM']

#wide format -> long format으로 변경
kam_fcst2 <- kam_fcst[, c('P Unit','PG','Model.Suffix','Measure',str_subset(colnames(kam_fcst),'W[0-9]+')), with = F]
kam_fcst_melt <- reshape2::melt(kam_fcst2, id=c('P Unit','PG','Model.Suffix','Measure'))

#Measure 명칭 변경 및 time추가
kam_fcst_melt[Measure == "Sell-Out FCST_ML [F]", Measure := 'ML_FCST']
kam_fcst_melt[Measure == "Sell-Out FCST_Override [F]", Measure := 'USER_FCST']
kam_fcst_melt[Measure == "Sell-Out FCST_KAM [R+F]", Measure := 'GDMI_FCST']

kam_fcst_melt[,weeknum := as.numeric(str_remove(str_extract(variable,'W[0-9]+'),'W'))]
kam_fcst_melt[,target_week_date := as.Date(paste0('20',str_extract(variable,'[0-9]+-[0-9]+-[0-9]+')))]
kam_fcst_melt[,year := as.factor(year(target_week_date))]
kam_fcst_melt[,variable := NULL]
setnames(kam_fcst_melt, c('P Unit','PG','Model.Suffix'), c('account_name','product','model_suffix_code'))

#wide format 형식 추가
kam_fcst_dcast <- data.table(reshape2::dcast(kam_fcst_melt, account_name + product + model_suffix_code + target_week_date + weeknum ~ Measure))
kam_fcst_dcast[,gap := ML_FCST - USER_FCST]
kam_fcst_dcast[,abs_gap := abs(gap)]

#ML FCST는 있는데 USER FCST는 없는 제품
no_user_fcst_list <- unique(kam_fcst_dcast[is.na(USER_FCST), model_suffix_code])
#분석 대상에서 제외
kam_fcst_melt <- kam_fcst_melt[!model_suffix_code %in% no_user_fcst_list]
kam_fcst_dcast <- kam_fcst_dcast[!model_suffix_code %in% no_user_fcst_list]

#------------------------------------------------------------------------------------------------------------------------------
# Compare ML FCST and KAM FCST
#------------------------------------------------------------------------------------------------------------------------------
kam_fcst_melt <- kam_fcst_melt[Measure != 'GDMI_FCST']

#account 단위 주차별 Gap
gap_by_account <- kam_fcst_dcast[,.('ML_FCST_sum' = sum(ML_FCST, na.rm = T),'USER_FCST_sum' = sum(USER_FCST, na.rm = T),'Gap_sum' = sum(abs_gap, na.rm = T)), 
                                 by=.(account_name, product, target_week_date)]

# reshape2::melt(gap_by_account, id =c('account_name','product', 'target_week_date'))


#Gap 상위 제품
kam_fcst_dcast[,.('Gap_sum' = sum(abs_gap)), by=.(account_name, product, model_suffix_code)][order(-Gap_sum)]


# visualization -----------------------------------------------------------------------------------------------------------------------
accounts = 'DIXONS'
products = 'WM'

#전체 모델 기준
fcst_sum <- kam_fcst_melt[,sum(value, na.rm = T), by=.(account_name, product,Measure ,target_week_date)]
gg <- ggplot() + geom_line(data = fcst_sum[account_name == accounts & product == products], aes(x=target_week_date, y=V1, group = Measure, col = Measure)) + 
  geom_line(data = gap_by_account[account_name == accounts & product == products], aes(x=target_week_date, y=Gap_sum, colour = 'Gap')) +
  theme_bw() + labs(x = 'target_week', y = 'fcst_sum',title = str_interp('${accounts} ${products}')) +
  scale_x_date(date_labels = '%V',date_breaks = '1 week') 

ggplotly(gg)


#개별모델 기준
model_list <- kam_fcst_melt[account_name == accounts & product == products & Measure == 'USER_FCST', sum(value, na.rm = T), by=.(model_suffix_code)][order(-V1)]
tmp_model_name <- model_list[,model_suffix_code][1]
tmp_model_name <- '32LM6300PLA.AEK'
ggplot(kam_fcst_melt[model_suffix_code == tmp_model_name], aes(x = target_week_date, y = value, group = Measure, col = Measure)) + 
  geom_line() + scale_x_date(date_labels = '%V',date_breaks = '1 week') + 
  theme_bw() + labs(x = 'target_week', y = 'sellout_qty fcst',title = str_interp('${accounts} ${products}\nMODEL : ${tmp_model_name}'))

