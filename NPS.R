
#df <- read.csv(file='../data/out-201402.csv', header=TRUE, sep=',')
setwd("~/Projects/Hyatt/code")
df <- read.csv(file='../data/out-201412.csv', header=TRUE, sep=',')

View(df)
colnames(df)

drops_c <- c("MARKET_CODE_C","MARKET_DESC_C","MAJOR_MARKET_CODE_C","CHANNEL_CODE_C","RATE_PLAN_C","SPLIT_RATE_PLAN_C","CONS_GUEST_ID_C","RESERVATION_CONFIRMATION_NUM_C","RESERVATION_CONFIRMATION_PAGE_C","RESERVATION_STATUS_C", "PMS_ROOM_REV_C","PMS_TOTAL_REV_C","PMS_FOOD_BEVERAGE_REV_C","PMS_OTHER_REV_C","WALK_FLAG_C","DUP_INDEX_C")
df <- df[ , !(names(df) %in% drops_c)]
ncol(df)

drops_r <- c("CONFIRMATION_NUM_R","CONFIRMATION_PAGE_R","RESERVATION_DATE_R","ARRIVAL_FLG_R","ARRIVAL_DATE_R","DEPARTURE_DATE_R","DIRECT_NIGHTS_NUM_R","RESERVATION_STATUS_R","ENTRY_HOTEL_CODE_R","ROOM_TYPE_CODE_R","ROOM_TYPE_DESCRIPTION_R","MAJOR_MARKET_CODE_R","PMS_RATE_CATEGORY_CODE_R","NUM_ROOMS_R","ADULT_NUM_R","CHILDREN_NUM_R","NT_RATE_R","FLIGHTT_INFO_R","LENGTH_OF_STAY_R","LENGTH_OF_STAY_CATEGORY_R","CALCULATED_NIGHTS_NUM_R","ROOM_NIGHTS_R","STATUS_CALCULATION_R","PACE_CATEGORY_R","PAST_VS_FUTURE_R","REVENUE_R","REVENUE_USD_R","CHANNEL_CODE_R")
df <- df[ , !(names(df) %in% drops_r)]
ncol(df)

drops_i <- c("e_delivereddate_adj_I","e_hy_gss_tier_I","e_hy_gss_conf_page_I","e_hy_gss_rate_plan_code_I","e_country_I","e_hy_gss_check_in_time_text_I","e_hy_gss_check_out_time_text_I","e_hy_gss_check_in_time_I","e_hy_gss_check_out_time_I","e_checkin_I","e_checkout_I")
df <- df[ , !(names(df) %in% drops_i)]
ncol(df)

drops_h <- c("Response_Date_H","Guest_Checkin_Date_H","Guest_Checkout_Date_H","Length_Stay_H","Guest_State_H","Guest_Country_H","Gender_H","POV_H","Language_H","DOE_H","Booking_Location_H","Num_Adults_H","Num_Kids_H","Conf_Num_Orig_H","Conf_Num_H","Conf_Page_H","Rate_Plan_H","Gross_Rev_H","Net_Rev_H","Room_Rev_H","CC_Type_H","Currency_H","GDS_Source_H","Checkin_Length_H","Room_Num_H","Room_Type_H","GP_Tier_H","Status_H","Feedback_Type_H")
df <- df[ , !(names(df) %in% drops_h)]
ncol(df)

drops_pl <- c("Hotel Name-Short_PL","Award Category_PL","Ops Region_PL","Property DMA_PL","Currency_PL","Dom Int'l_PL","STR Number_PL","STR Market_PL","Brand Initial_PL","G#Region_PL","D#Region_PL","Region_PL","Category_PL","Scope of Service_PL","Type_PL","Class_PL","Location_PL","Bucket_PL","Relationship_PL")
df <- df [,!(names(df) %in% drops_pl)]
ncol(df)

drops_none <- c("Sub_Channel_Category","Channel_Category","GP_Tier","Stay_Sequence","Stay_Sequence_Brand","Completed_Survey_Sequence","Days_Since_Last_Stay","Days_Until_Next_Stay")
df <- df [,!(names(df) %in% drops_none)]
ncol(df)

View(df)
colnames(df)
drops_col <- c("LAST_CHANGE_DATE_R","STATE_R","COUNTRY_CODE_R","ETA_R","QUOTED_RATE_C","PMS_ROOM_REV_USD_C","PMS_TOTAL_REV_USD_C","PMS_FOOD_BEVERAGE_REV_USD_C","PMS_OTHER_REV_USD_C","MARKET_GROUP_C","NO_SHOW_FLAG_C","ROOM_NUM_C","PACE_R","e_delivereddate_I","e_status_I","e_hy_gss_tier_I","e_hy_gss_conf_page_I","e_country_I","e_hy_feedback_type_I","e_hy_gss_check_in_by_I","e_hy_gss_check_out_by_I","e_hy_gss_marketing_emails_ok_yn_I","e_hy_gss_promo_emails_ok_yn_I","e_hy_gss_title_text_I","e_hy_gss_language_I","e_hy_gss_room_floor_I","a_last_seen_page_name_I","a_last_submitted_page_name_I","Survey_ID_H","Num_Rooms_H","Mobile_H","Mobile_First_H","F.B_FREQ_H","Internet_Dissat_Lobby_H","Internet_Dissat_Slow_H","Internet_Dissat_Connectivity_H","Internet_Dissat_Wired_H","TV_Internet_General_H","Internet_Dissat_Expensive_H","Internet_Dissat_Billing_H","Internet_Dissat_Other_H","Room_Dissat_Internet_H","average_daily_rate_CC","Spirit_PL","Hotel.Name.Short_PL","Award.Category_PL","Property.DMA_PL","Dom.Int.l_PL","STR.Number_PL","STR.Market_PL","Brand.Initial_PL","Club.Type_PL","Hotel.Inventory_PL","Floors_PL","Union_PL","G.Region_PL","D.Region_PL","Scope.of.Service_PL","All.Suites_PL","Indoor.Corridors_PL","Spa.services.in.fitness.center_PL","Booking_Channel")
df <- df [,!(names(df) %in% drops_col)]
ncol(df)

View(df)
ncol(df)

# remove all rows with na in classifier - liklihood to recommend
complete_cases <- complete.cases(df[,"Likelihood_Recommend_H"])
df_na <- df[complete_cases,]
nrow(df_na)
View(df_na)

library(ggplot2)
install.packages('plotrix')
library(plotrix)

# % of people - promoters/ detractors
types_count <- tapply(df_na$NPS_Type, df_na$NPS_Type, length)
types_df <- data.frame(c("Detractor","Passive","Promoter"), c(types_count["Detractor"], types_count["Passive"], types_count["Promoter"]))
colnames(types_df) <- c("labels","value")
types_df
rownames(types_df) <- NULL
pie3D(types_df$value, labels = types_df$labels, main = "Types of people", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)

plot(tapply(df_na$Likelihood_Recommend_H, df_na$Likelihood_Recommend_H, sum))
# this shows that most of the people are having a high NPS score. Now to convert the detractors to promoters



