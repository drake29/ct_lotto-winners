library(tidyr)
library(dplyr) 
library(readr)
library(data.table)
library(ggplot2)
library(leaflet)
library(ggmap)
library(ggthemes)
library(scales)

#Load & Clean Some of the raw-data
lotto = fread(file = '~/data/test_crawl.csv')
lotto.df = fread(file = '~/data/cleaned_lotto.csv')
towns_latlon = fread(file = '~/data/towns.csv')
lotto.df$prize = as.numeric(gsub("[\\,]", "", lotto.df$prize))
pop_town = fread(file = '~/data/pop_towns2015.csv')
pop_geo = fread(file = '~/data/pop_geo.csv')

retail_locale = lotto %>% 
  group_by(retailer_location) %>% 
  arrange(desc(retailer_location)) %>% 
  summarise('count'= n()) %>% 
  arrange(desc(count))
retail_locale$retailer_location = paste(retail_locale$retailer_location, "CT", sep=", ")
towns = bind_cols(retail_locale, towns_latlon)

#Use 'geocode' to get the lats/longs for each town &save so we dont call the API everytime
#get_geo = geocode(retail_locale$retailer_location)

#Add a commma after town, so geocode knows the town is for CT
#pop_town$Town = paste(pop_town$Town, "CT", sep=", ")
#geo_popgeo = geocode(pop_town$Town)

#bind DataFrames: CT towns Population with CT towns Lat/long coordinates 
town_pop = bind_cols(pop_town, pop_geo)

#check the classes for each variable in the dataset we'll be using:
sapply(lotto.df, class)
sapply(town_pop, class)
#convert "population" from character to numeric:
town_pop$`Est. Pop.` = as.numeric(gsub("[\\,]", "", town_pop$`Est. Pop.`))





######---DATA VISUALIZATION--#######

#Leaflet map to show CT population density (from towns that had winners)
lf = leaflet(town_pop) %>% addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = ~sqrt(town_pop$`Est. Pop.`) * 30, popup = ~Town)


#inter= left_join(retail_locale, pop_towns, by=c("retailer_location"='Town')) #take out the CT in retail_locale to join

#Leaflet map to visualize which towns people are winning the most:
Mapwinners_all= leaflet(towns) %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat= ~lat, weight=1,
             radius = ~sqrt(count) *1500, popup= ~retailer_location)

#Towns with 20 or more winners:
Mapwinners_top20 = towns %>% 
  filter(count >= 20)
#Visualized:
leaflet(Mapwinners_top20) %>% 
  addTiles() %>% 
  addCircles(lng = ~lon, lat= ~lat, weight=1,
             radius = ~sqrt(count) *950, popup= ~retailer_location)




#--GAMES---------------------
game_freq = lotto %>% 
  group_by(game) %>% 
  arrange(desc(game)) %>% 
  summarise('freq_count'=n()) %>% 
  arrange(desc(freq_count)) %>% 
  top_n(freq_count, n=8)


game_amount = lotto.df %>% 
  group_by(game) %>% 
  summarise(Total_winnings= sum(prize)) %>% 
  arrange(desc(Total_winnings)) %>% 
  top_n(Total_winnings, n=4)

game_freq2 = lotto.df %>%
  select(game, prize) %>% 
  filter(game == 'Powerball' |
           game == '10X Cash 11Th Ed.' |
           game == 'Play4 Night' |
           game == 'Cash5' |
           game == "30X Cash 3Rd Ed.") %>% 
  group_by(game) %>% 
  summarise('Avg_per_game'= mean(prize),
            'game_count'= n()) %>% 
  arrange(desc(game_count))



#Which game produces the most amount of 10k + winners? 
r = ggplot(game_freq, aes(x =game, y=freq_count, fill=game)) + geom_col() +
  ggtitle("Games that produce the most frequent Winners (Above 10k)")
r + coord_flip() + scale_fill_brewer() 
r + theme_economist() + scale_fill_economist()

#Which game has the biggest payouts?
rt = ggplot(data =game_amount, aes(x =game, y=Total_winnings)) + geom_col(aes(fill=game)) +
  ggtitle('Total Payouts') + scale_fill_brewer()
rt + theme_wsj() + scale_colour_wsj(palette = "black_green")

rt2 = ggplot(data = game_freq2, aes(x =game, y=Avg_per_game, fill=game)) + geom_col() +
  ggtitle('Average Payouts') + scale_fill_brewer()
zoom <- coord_cartesian(ylim = c(10000, 200000))
rt2 + theme_wsj() + scale_colour_wsj(palette = "black_green") + zoom



#----------------------------
winby_yr = sep_dates %>% 
  filter(year>2007) %>% 
  group_by(year) %>% 
  summarise('number'= n()) %>% 
  arrange(desc(number))

ggplot(data=winby_yr, aes(x=year, y= number, fill=year)) + geom_col() +
  scale_fill_brewer() + theme_fivethirtyeight()

#------Trend/Dates----------------------
#separate the month/day/year into three new columns
sep_dates= separate(lotto.df, date, into=c("month", "day", "year"))

dtemp = sep_dates %>% 
  filter(game =='Powerball' |
           game=='10X Cash 11Th Ed.' |
           game=='Play4 Night') %>% 
  group_by(month,game,prize) %>%
  arrange(desc(month, game)) %>% 
  summarise('num' = n())

top_gamewins= ggplot(dtemp, aes(x = month, y = num, group = game, color = game)) +
  geom_line() + 
  geom_point(size = 1.1) + 
  ggtitle("Monthly Number of Wins for Top Games") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica")

top_gameavg = sep_dates %>%
  select(game, prize) %>%
  filter(game =='Powerball' |
           game=='10X Cash 11Th Ed.' |
           game=='Play4 Night') %>% 
  group_by(game) %>% 
  summarise(Avg_win = mean(prize))
  
#Average win by top games  
tt= ggplot(top_gameavg, aes(x=game, y=Avg_win)) + geom_col()
tt+theme_gdocs() + scale_color_gdocs()
#--------------------------------------  

win_peryr = sep_dates %>% 
  group_by(year) %>% 
  summarise('yearly_payout'= sum(prize))

ggplot(win_peryr, aes(x = year, y=yearly_payout)) + geom_bar(stat='identity') 






#What was the sum of winnings for each year?
sum_year = sep_dates %>%
  filter(year>2007) %>% 
  group_by(year) %>% 
  summarise('total'= sum(prize)) %>% 
  arrange(desc(total))

j = ggplot(sum_year, aes(x=year, y=total, fill=year))
j + geom_col() + geom_line() + theme_stata() + scale_colour_stata()



avg_yrwin = sep_dates %>%
  filter(year>2007) %>% 
  group_by(year) %>% 
  summarise(Avg_win = mean(prize))

####################
scat= sep_dates %>% 
  filter(prize<5000000)
ggplot(scat, aes(x = year, y = prize)) + geom_point(position='jitter', aes(color=year))+
  scale_color_gdocs() + theme_economist_white()
#------------------
scat_zoom = coord_cartesian(ylim = c(10000, 5000000))

scat2 = sep_dates %>% 
  filter(year>2007)
scat_all = ggplot(scat2, aes(x=year, y=prize, color=year)) + geom_point(position='jitter') +
  scale_color_gdocs() + theme_economist_white()

scat_lim = ggplot(scat2, aes(x=year, y=prize, color=year)) + geom_point(position='jitter') + scat_zoom +
  scale_color_gdocs() + theme_economist_white()





#-------------------------

ggplot(sep_dates, aes(x = year, y= prize)) + geom_point(position= 'jitter') + scale_size_area()

monthly_avg = sep_dates %>%
  group_by(month) %>% 
  summarise(monthly_winnings = mean(prize))

ggplot(monthly_avg, aes(x=month, y=monthly_winnings)) + geom_line()


biggest_gamewin = lotto.df %>%
  group_by(game, prize) %>% 
  summarise() %>% 
  top_n(n=10)


top_locations = lotto.df %>% 
  select(retailer_location, prize) %>% 
  top_n(prize, n=20)

#------------------------------------------





