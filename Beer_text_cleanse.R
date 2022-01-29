library(stringr)
total1 <- read.csv("/Users/yemoonkim/R/Beer project/Beer_Review/Beer_parsing_total1.csv")
total2 <- read.csv("/Users/yemoonkim/R/Beer project/Beer_Review/Beer_parsing_total2.csv")
total3 <- read.csv("/Users/yemoonkim/R/Beer project/Beer_Review/Beer_parsing_total3.csv")
total4 <- read.csv("/Users/yemoonkim/R/Beer project/Beer_Review/Beer_parsing_total4.csv")
total5 <- read.csv("/Users/yemoonkim/R/Beer project/Beer_Review/Beer_parsing_total5.csv")

total = rbind(total1, total2, total3, total4, total5)

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------


#숫자 평점, 날짜04-Dec 제거(total_7 for text analysis 만들 때 포함)
#total_2 <- total[- grep("^\\d.\\d\\d$|^\\d.\\d$|^\\d$", total$review), ]

#total_2$overall <- total[grep("^\\d.\\d\\d$|^\\d.\\d$|^\\d$", total$review), ]
#total_2$overall <- total$review
total_3 <- total[- grep("^\\d\\d-", total$review), ]

#look: 평점 제거
total_4 <- total_3[ -grep("^look: \\d", total_3$review), ]

#요일로 시작하는 날짜 제거
total_5 <- total_4[ -grep("^Wednesday|^Tuesday|^Monday|^Sunday|^Saturday|^Friday|^Thursday", total_4$review), ]

#알콜도수 제거 
total_5$review <- as.character(total_5$review)
total_6 <- total_5[ -grep("(^-\\d)+%|(^-\\d\\d)+%|(^-\\d.\\d)+%|(^-\\d\\d.\\d)+%", total_5$review), ]
total_7 <- total_6[ -grep("(^.\\d)+%|(^.\\d\\d)+%|(^.\\d.\\d)+%|(^.\\d\\d.\\d)+%", total_6$review), ]
#write.csv(total_7, "total_7 for text analysis")


total_spl <- strsplit(total_7$review, " ")
head(total_spl)
length(total_spl[[1]])

total_spl_df <- data.frame(colnames(c("user", "location")))

df <- data.frame(1,2,3,4,5,6)
colnames(df) <- c("beer","company", "user", "text", "location", "overall")

to <- total[grep("^\\d.\\d\\d$|^\\d.\\d$|^\\d$", total$review), ]
total_8 <- read.csv("total_7 for text analysis")
total_8[(total_8$name == total_7$name[3]) & (total_8$review != total_7$review[3]), "review"]


j=0
for(i in 1:43088){
  if (is.na(total_spl[[i]][2]) == F & total_spl[[i]][2] == "from" & length(total_spl[[i]])< 5 ){
    j = j+1
    df[j,"beer"]<- total_7$name[i]
    df[j,"company"]<- total_7$company[i]
    df[j,"user"] <- total_spl[[i]][1]
    df[j,"text"] <- paste((total_8[(total_8$name == total_7$name[i]) & (total_8$review != total_7$review[i]), "review"])[1],
                          (total_8[(total_8$name == total_7$name[i]) & (total_8$review != total_7$review[i]), "review"])[2],
                          (total_8[(total_8$name == total_7$name[i]) & (total_8$review != total_7$review[i]), "review"])[3])
    df[j,"location"] <- paste(total_spl[[i]][3], total_spl[[i]][4])
    df[j,"overall"] <- to[(to$name == total_7$name[i] & to$company == total_7$company[i]),3][1]
    
  }
}
write.csv(df, "new text dataset.csv")
# from 주이름/국가명 으로 끝나는 행 제거
#total_7 <- total_6[ grep("(from)+country_list", total_6$review), ]

#country_list <- c("United States", "Canada", "Afghanistan", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and/or Barbuda", "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", "British Indian Ocean Territory", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros", "Congo", "Cook Islands", "Costa Rica", "Croatia (Hrvatska)", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecudaor", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands (Malvinas)", "Faroe Islands", "Fiji", "Finland", "France", "France, Metropolitan", "French Guiana", "French Polynesia", "French Southern Territories", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard and Mc Donald Islands", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea, Democratic People's Republic of", "Korea, Republic of", "Kosovo", "Kuwait", "Kyrgyzstan", "Lao People's Democratic Republic", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia, Federated States of", "Moldova, Republic of", "Monaco", "Mongolia", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "Netherlands Antilles", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfork Island", "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn", "Poland", "Portugal", "Puerto Rico", "Qatar", "Reunion", "Romania", "Russian Federation", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia South Sandwich Islands", "South Sudan", "Spain", "Sri Lanka", "St. Helena", "St. Pierre and Miquelon", "Sudan", "Suriname", "Svalbarn and Jan Mayen Islands", "Swaziland", "Sweden", "Switzerland", "Syrian Arab Republic", "Taiwan", "Tajikistan", "Tanzania, United Republic of", "Thailand", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States minor outlying islands", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City State", "Venezuela", "Vietnam", "Virigan Islands (British)", "Virgin Islands (U.S.)", "Wallis and Futuna Islands", "Western Sahara", "Yemen", "Yugoslavia", "Zaire", "Zambia", "Zimbabwe")

