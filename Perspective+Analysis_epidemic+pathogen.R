setwd('C:\\Users\\Aaron Cho\\Desktop\\ongoing\\언어학강독I')


library(ngramr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)

rm(list = ls())
disease <- c('AIDS', 'syphilis', 'hepatitis',  
	'Spanish flu', 'Hongkong flu', 'Asian flu', 'tuberculosis', 'small pox',
	'typhoid', 'cholera', 'dengue')
virus <- c('HIV', 'treponema pallidum', 'hepatitis virus', 
	'H1N1 virus', 'H3N2 virus', 'H2N2 virus', 'mycobacterium tuberculosis', 'variola virus', 
	'salmonella', 'vibrio cholerae', 'dengue virus')
ratio.d <- paste(disease, virus, sep = '/')
ratio.v <- paste(virus, disease, sep = '/')
reported <- c(1981, 1800, 1965, 1989, 1918, 1960, 1800, 1800, 1880, 1817, 1800)

chain <- rep(c('sexual', 'blood', 'airborne', 'waterborne', 'mosquito'), times = c(2, 1, 5, 2, 1))

dt <- data.frame(Disease = disease, 
			Reported = reported, 
			Ratio.d = ratio.d,
			Ratio.v = ratio.v, 
			Virus = virus,
			Chain = chain)

disease.Hangul <- c('에이즈', '매독', '간염', '스페인 독감', '홍콩 독감', '아시안 독감', '결핵', '두창', 
	'장티푸스', '콜레라', '뎅기')


write.table(dt, 'Disease+18_20c.txt', 
		col.names = TRUE, row.names = FALSE, 
		quote = FALSE, sep = '\t', 
		fileEncoding = 'utf8') 
write.table(disease.Hangul, 'disease+Hangul.txt', 
		col.names = FALSE, row.names = FALSE, 
		quote = FALSE, sep = '\n', 
		fileEncoding = 'utf8')

df <- read.delim('Disease+18_20c.txt', 
		header = TRUE, 
		sep = '\t', quote = NULL, 
		fileEncoding = 'utf8')
disease.Hangul <- scan('disease+Hangul.txt', what = '', sep = '\n', 
			quote = NULL, fileEncoding = 'utf8')
summary(dt)


disease <- ngram(df$Disease)
disease.wide <- spread(disease[-4], Phrase, Frequency)

virus <- ngram(df$Virus)
virus.wide <- spread(virus[-4], Phrase, Frequency)
	
ratio.d <- ngram(df$Ratio.d)
ratio.d.wide <- spread(ratio.d[-4], Phrase, Frequency)

ratio.v <- ngram(df$Ratio.v)
ratio.v.wide <- spread(ratio.v[-4], Phrase, Frequency)


		#names
names <- c()

for (i in 1:length(df$Disease)){
	name <- paste(df$Disease[i], '/', disease.Hangul[i])
	names <- c(names, name)
	} 

virus_names <- c()

for (i in 1:length(df$Virus)){
	name <- paste(df$Virus[i], '/', disease.Hangul[i])
	virus_names <- c(virus_names, name)
	} 



	# disease별 그래프 조망
par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.wide$Year, disease.wide[-1], 
	type = 'l', lty = 1:(length(disease.wide)-1),, lwd = 3, 
	col = 1:(length(disease.wide)-1), 
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도')
title('감염병명 상대빈도')
rect(xleft = 1890, xright = 1945, ybottom = 0, ytop = 4e-05,
	col = 'grey', density = 30)
legend('topleft', legend = sort(names), inset = 0.03, cex = 0.8,
	lty = 1:(length(disease.wide)-1),, lwd = 2, col = 1:(length(disease.wide)-1))
	
	# 보고연도 
former <- df %>% 
	filter(Reported < 1900) 
latter <- df %>%
	filter(Reported >= 1900)

disease.former <- ngram(former$Disease)
disease.former.wide <- spread(disease.former[-4], Phrase, Frequency)
disease.latter <- ngram(latter$Disease)
disease.latter.wide <- spread(disease.latter[-4], Phrase, Frequency)

virus.former <- ngram(former$Virus)
virus.former.wide <- spread(virus.former[-4], Phrase, Frequency)
virus.latter <- ngram(latter$Virus)
virus.latter.wide <- spread(virus.latter[-4], Phrase, Frequency)

	
ratio.d.former <- ngram(former$Ratio.d)
ratio.d.former.wide <- spread(ratio.d.former[-4], Phrase, Frequency)
ratio.d.latter <- ngram(latter$Ratio.d)
ratio.d.latter.wide <- spread(ratio.d.former[-4], Phrase, Frequency)

ratio.v.former <- ngram(former$Ratio.v)
ratio.v.former.wide <- spread(ratio.v.former[-4], Phrase, Frequency)
ratio.v.latter <- ngram(latter$Ratio.v)
ratio.v.latter.wide <- spread(ratio.v.latter[-4], Phrase, Frequency)


	# 20세기 이전
par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.former.wide$Year, disease.former.wide[-1], 
	type = 'l', lty = 'solid', lwd = 3, col = 1:length(disease.former$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(20세기 이전)')
title('감염병명 상대빈도(20세기 이전)')
legend('topleft', legend = sort(names[-c(1, 3:6)]), inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:length(disease.former$Phrase))
		# 20세기 이후
par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.latter.wide$Year, disease.latter.wide[-1], 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(disease.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(20세기 이후)')
title('감염병명 상대빈도(20세기 이후)')
legend('topleft', legend = sort(names[c(1, 3:6)]), inset = 0.03, cex = 1.2,
	lty = 'solid', lwd = 2, col = 1:length(disease.latter$Phrase))

		# 20세기 이후 / 20세기 이후 측정치부터
disease.latter.trunc <- ngram(latter$Disease, year_start = 1950)
disease.latter.trunc.wide <- spread(disease.latter.trunc[-4], Phrase, Frequency)
par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.latter.trunc.wide$Year, disease.latter.trunc.wide[-1], 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(disease.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(20세기 이후, 1950~)')
title('감염병명 상대빈도(20세기 이후, 1950~)')
legend('topleft', legend = sort(names[c(1, 3:6)]), inset = 0.03, cex = 1.1,
	lty = 'solid', lwd = 2, col = 1:length(disease.latter$Phrase))


	# 표준화	

par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.wide$Year, scale(disease.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 2, 
	col = 1:(length(disease.wide)-1), 
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(표준화)')
title('감염병명 상대빈도(표준화)')
	# first phase
rect(xleft = 1800, xright = 1900, ybottom = -0.8, ytop = 3,
	col = 'grey', density = 0, lwd = 4, )
rect(xleft = 1880, xright = 1945, ybottom = -0.8, ytop = 4,
	col = 'red', density = 0, lwd = 4, )
	# second phase
	# 삼각형
triangle_x <- c(1800, 2019, 2019)
triangle_y <- c(-1, -1, 3)
polygon(triangle_x, triangle_y, col = 'blue', density = 0, lwd = 6)
	# third phase
rect(xleft = 1950, xright = 2019, ybottom = - 0.2, ytop = 6,
	col = 'yellow', density = 0, lwd = 6)
legend('topleft', legend = sort(names), inset = 0.03, cex = 0.8,
	lty = 'solid', lwd = 2, col = col = 1:(length(disease.wide)-1))

	# take a closer look


		# 20세기 이전
par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.former.wide$Year, scale(disease.former.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 3, col = 1:length(disease.former$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(표준화, 20세기 이전)')
title('감염병명 상대빈도(표준화, 20세기 이전)')
legend('topright', legend = sort(names[-c(1, 3:6)]), inset = 0.03, cex = 1.2,
	lty = 'solid', lwd = 2, col = 1:length(names))
		# 20세기 이후
par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(disease.latter.wide$Year, scale(disease.latter.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(disease.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(표준화, 20세기 이후)')
title('감염병명 상대빈도(표준화, 20세기 이후)')
legend('topleft', legend = sort(names[c(1, 3:6)]), inset = 0.03, cex = 1.2,
	lty = 'solid', lwd = 2, col = 1:length(disease.former$Phrase))

		# 20세기 이후 / 20.5세기 이후 측정치부터
disease.latter.trunc <- ngram(latter$Disease, year_start = 1950)
disease.latter.trunc.wide <- spread(disease.latter.trunc[-4], Phrase, Frequency)
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(disease.latter.trunc.wide$Year, scale(disease.latter.trunc.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(disease.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(표준화, 1950~)')
title('감염병명 상대빈도(표준화, 20세기 이후, 1950~)')
legend('topright', legend = sort(names[c(1, 3:6)]), inset = 0.03, cex = 0.9,
	lty = 'solid', lwd = 2, col = 1:length(disease.latter$Phrase))



	# virus별 그래프 조망
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(virus.wide$Year, virus.wide[-1], 
	type = 'l', lty = 1:(length(virus.wide)-1), lwd = 3, 
	col = 1:(length(virus.wide)-1), 
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명 상대빈도')
title('바이러스명 상대빈도')
rect(xleft = 1980, xright = 2019, ybottom = 0, ytop = 3.1e-05,
	col = 'grey', density = 30)
legend('topleft', legend = sort(virus_names), inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:(length(virus.wide)-1))
	


	# 20세기 이전
par(mar = c(5, 5, 4, 2), xpd = TRUE)
matplot(virus.former.wide$Year, virus.former.wide[-1], 
	type = 'l', lty = 'solid', lwd = 3, col = 1:length(virus.former$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명 상대빈도(20세기 이전)')
title('바이러스명 상대빈도(20세기 이전)')
legend('topleft', legend = sort(virus_names[-c(1, 3:6)]), inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:length(virus.former$Phrase))

		# 20세기 이후
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(virus.latter.wide$Year, virus.latter.wide[-1], 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(virus.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명 상대빈도(20세기 이후)')
title('바이러스명 상대빈도(20세기 이후)')
legend('topleft', legend = sort(virus_names)[2:6], inset = 0.03, cex = 1.2,
	lty = 'solid', lwd = 2, col = 1:length(virus.latter$Phrase))

		# 20세기 이후 / 20세기 이후 연도 조정
virus.latter.trunc <- ngram(latter$Virus, year_start = 1950)
virus.latter.trunc.wide <- spread(virus.latter.trunc[-4], Phrase, Frequency)
par(mar = c(5, 5, 4, 2), xpd = TRUE)
matplot(virus.latter.trunc.wide$Year, virus.latter.trunc.wide[-1], 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(virus.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명 상대빈도(20세기 이후, 1950~)')
title('바이러스명 상대빈도(20세기 이후, 1950~)')
legend('topleft', legend = sort(virus_names[2:6]), inset = 0.03, cex = 1.1,
	lty = 'solid', lwd = 2, col = 1:length(virus.latter$Phrase))
	
	# 표준화	

par(mar = c(5, 5, 2, 2), xpd = TRUE)
matplot(virus.wide$Year, scale(virus.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 2, 
	col = 1:(length(virus.wide)-1), 
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명 상대빈도(표준화)')
title('바이러스명 상대빈도(표준화)')
	# first phase
rect(xleft = 1890, xright = 1940, ybottom = -0.8, ytop = 4.2,
	col = 'grey', density = 0, lwd = 4)
	# second phase
rect(xleft = 1970, xright = 1995, ybottom = -0.8, ytop = 4,
	col = 'red', density = 0, lwd = 4)
	# third phase
rect(xleft = 2000, xright = 2025, ybottom = 0, ytop = 5.8,
	col = 'yellow', density = 0, lwd = 4)
legend('topleft', legend = sort(virus_names), inset = 0.03, cex = 0.8,
	lty = 'solid', lwd = 2, col = 1:(length(virus.wide)-1))



		# 20세기 이전
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(virus.former.wide$Year, scale(virus.former.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 3, col = 1:length(virus.former$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명 상대빈도(표준화, 20세기 이전)')
title('바이러스명 상대빈도(표준화, 20세기 이전)')
legend('topleft', legend = sort(virus_names)[-c(2:6)], inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:length(virus.former$Phrase))
		# 20세기 이후
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(virus.latter.wide$Year, scale(virus.latter.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(virus.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스 상대빈도(표준화, 20세기 이후)')
title('바이러스명 상대빈도(표준화, 20세기 이후)')
legend('topleft', legend = sort(virus_names)[2:6], inset = 0.03, cex = 1.2,
	lty = 'solid', lwd = 2, col = 1:length(virus.latter$Phrase))

	# 1950~
virus.latter.trunc <- ngram(latter$Virus, year_start = 1950)
virus.latter.trunc.wide <- spread(virus.latter.trunc[-4], Phrase, Frequency)
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(virus.latter.trunc.wide$Year, scale(virus.latter.trunc.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(virus.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '감염병명 상대빈도(표준화, 20세기 이후, 1950~)')
title('감염병명 상대빈도(표준화, 20세기 이후, 1950~)')
legend('topleft', legend = sort(virus_names[c(1, 3:6)]), inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:length(virus.latter$Phrase))


	# HIV/AIDS를 필두로 살펴보자
aids <- ngram('AIDS', year_start = 1980)
hiv <- ngram('HIV', year_start = 1980)

p.aids <- ggplot(aids) +
	geom_line(aes(x = Year, y = Frequency), col = 'skyblue', lwd = 1.5) +
	ggtitle('AIDS') +
	geom_segment(aes(x = 1990, xend = 1990, y = 4e-05, yend = 3e-05),
			col = 'deeppink4',  
			arrow = arrow(ends = 'first', length = unit(0.3, 'cm'), 
			angle = 30, type = 'closed')) +
	theme_bw() +
	theme(plot.title = element_text(hjust = 0.5, vjust = 0.2, face = 'bold'), 
		axis.text.y = element_blank()) 
p.hiv <- ggplot(hiv) +
	geom_line(aes(x = Year, y = Frequency), col = 'gold', lwd = 1.5) +
	ggtitle('HIV') +
	geom_segment(aes(x = 1995, xend = 1995, y = 3e-05, yend = 2e-05), 
			col = 'deeppink4', 
			arrow = arrow(ends = 'first', length = unit(0.3, 'cm'), 
					angle = 30,  type = 'closed')) +
	geom_segment(aes(x = 2005, xend = 2005, y = 3e-05, yend = 2e-05), 
			col = 'deeppink4', 
			arrow = arrow(ends = 'first', length = unit(0.3, 'cm'), 
					angle = 30,  type = 'closed')) +
	theme_bw() +
	theme(plot.title = element_text(hjust = 0.5, vjust = 0.2, face = 'bold'), 
		axis.text.y = element_blank()) 

gridExtra::grid.arrange(p.aids, p.hiv)

	# 매독
syphilis <- ngram('syphilis', year_start = 1800)
treponema.pallidum <- ngram('treponema pallidum', year_start = 1800) 

p.syphilis <- ggplot(syphilis) +
	geom_line(aes(x = Year, y = Frequency), col = 'skyblue', lwd = 1.5) +
	ggtitle('Syphilis(매독)') +
	geom_segment(aes(x = 1883, xend = 1883, y = 5.0e-06, yend = 7.51e-06), 
			col = 'deeppink4', 
			arrow = arrow(ends = 'last', length = unit(0.3, 'cm'), 
					angle = 30, type = 'closed')) +
	geom_segment(aes(x = 1918, xend = 1918, y = 1.0e-05, yend = 1.24e-05), 
			col = 'deeppink4', 
			arrow = arrow(ends = 'last', length = unit(0.3, 'cm'), 
					angle = 30, type = 'closed')) +
	geom_segment(aes(x = 1938, xend = 1938, y = 7.49e-06, yend = 1.12e-05), 
			col = 'deeppink4', 
			arrow = arrow(ends = 'last', length = unit(0.3, 'cm'), 
					angle = 30, type = 'closed')) +
	theme_bw() +
	theme(plot.title = element_text(hjust = 0.5, vjust = 0.2, face = 'bold') 
	)

p.treponema.pallidum <- ggplot(treponema.pallidum) +
	geom_line(aes(x = Year, y = Frequency), col = 'gold', lwd = 1.5) +
	ggtitle('Treponema Pallidum(매독균)') +
	geom_segment(aes(x = 1915, xend = 1915, y = 4e-08, yend = 4.88e-08), 
				col = 'deeppink4', 
			arrow = arrow(ends = 'last', length = unit(0.3, 'cm'), 
					angle = 30, type = 'closed')) +
	geom_segment(aes(x = 1978, xend = 1978, y = 1e-08, yend = 1.8e-08), 
				col = 'deeppink4', 
			arrow = arrow(ends = 'last', length = unit(0.3, 'cm'), 
					angle = 30, type = 'closed')) +
	theme_bw() +
	theme(plot.title = element_text(hjust = 0.5, vjust = 0.2, face = 'bold'),
		axis.text.y = element_blank())

gridExtra::grid.arrange(p.syphilis, p.treponema.pallidum)




	# 바이러스명 / 감염병명 (ratio.v)

par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(ratio.v.wide$Year, ratio.v.wide[-1], 
	type = 'l', lty = 'solid', lwd = 3, 
	col = 1:(length(ratio.v.wide)-1), 
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병명 상대빈도')
title('바이러스명/감염병명 상대빈도')
arrows(1807, 2.5, 1810, 8, length = 0.05, angle = 30, code = 1, lty = 'solid', lwd = 3)
axis(side = 2, at = 1, lwd = 3, col.ticks = 'red')
axis(side = 4, at = 1, lwd = 3, col.ticks = 'red')
rect(xleft = 1980, xright = 2025, ybottom = 0, ytop = 2,
	col = 'red', density = 0, lwd = 3)
rect(xleft = 1960, xright = 2026, ybottom = 3, ytop = 29,
	col = 'blue', density = 0, lwd = 3)
legend('topleft', legend = sort(df$Ratio.v), inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:(length(ratio.v.wide)-1))
	


	# 20세기 이전
par(mar = c(5, 5, 4, 2), xpd = TRUE)
matplot(ratio.v.former.wide$Year, ratio.v.former.wide[-1], 
	type = 'l', lty = 'solid', lwd = 3, col = 1:length(ratio.v.former$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병명 상대빈도(20세기 이전)')
title('바이러스명/감염병명 상대빈도(20세기 이전)')
	# first phase
rect(xleft = 1795, xright = 1840, ybottom = -0.01, ytop = 1,
	col = 'red', density = 0, lwd = 4)
	# second phase
rect(xleft = 1925, xright = 2020, ybottom = 0.05, ytop = 0.8,
	col = 'blue', density = 0, lwd = 4)
legend('top', legend = sort(df$Ratio.v)[-c(2:6)] , inset = 0.03, cex = 0.9,
	lty = 'solid', lwd = 2, col = 1:length(ratio.v.former$Phrase))

		# 20세기 이후
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(ratio.v.latter.wide$Year, ratio.v.latter.wide[-1], 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(ratio.v.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병명 상대빈도(20세기 이후)')
title('바이러스명/감염병명 상대빈도(20세기 이후)')
axis(side = 2, at = 1, lwd = 3, col.ticks = 'red')
axis(side = 4, at = 1, lwd = 3, col.ticks = 'red')
	# first phase
rect(xleft = 1960, xright = 2025, ybottom = 0, ytop = 29,
	col = 'blue', density = 0, lwd = 4)
# second phase
rect(xleft = 1970, xright = 2025, ybottom = -0.1, ytop = 2,
	col = 'red', density = 0, lwd = 4)
legend('topleft', legend = sort(df$Ratio.v)[2:6], inset = 0.05, cex = 1.1,
	lty = 'solid', lwd = 2, col = 1:length(ratio.v.latter$Phrase))

		# 20세기 이후 / 20세기 이후 연도 조정
ratio.v.latter.trunc <- ngram(latter$Ratio.v, year_start = 1950)
ratio.v.latter.trunc.wide <- spread(ratio.v.latter.trunc[-4], Phrase, Frequency)
par(mar = c(5, 5, 4, 2), xpd = TRUE)
matplot(ratio.v.latter.trunc.wide$Year, ratio.v.latter.trunc.wide[-1], 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(df$Ratio.v[2:6]),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병 상대빈도(20세기 이후, 1950~)')
title('바이러스명/감염병 상대빈도(20세기 이후, 1950~)')
legend('topleft', legend = sort(df$Ratio.v)[2:6], inset = 0.05, cex = 1.1,
	lty = 'solid', lwd = 2, col = 1:length(df$Ratio.v[2:6]))

		# 20세기 이후 / 연도 조정 / Hongkong flu 제외 
matplot(ratio.v.latter.trunc.wide$Year, ratio.v.latter.trunc.wide[-c(1, 4)], 
	type = 'l', lty = 'solid', lwd = 4, col = col = 1:length(df$Ratio.v[2:6]),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병 상대빈도(20세기 이후, 1950~)')
title('바이러스명/감염병 상대빈도(20세기 이후, 1950~ / Hongkong flu 제외)')
legend('topleft', legend = sort(df$Ratio.v)[c(2:3, 5:6)], inset = 0.05, cex = 1.1,
	lty = 'solid', lwd = 2, col = 1:length(df$Ratio.v[2:6]))



	# 표준화	
## 전체 조망하니까 너무 개판임
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(ratio.v.wide$Year, scale(ratio.v.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 3, 
	col = 1:(length(ratio.v.wide)-1), 
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병명 상대빈도')
title('바이러스명/감염병명 상대빈도')
arrows(1807, 2.5, 1810, 8, length = 0.05, angle = 30, code = 1, lty = 'solid', lwd = 3)
axis(side = 2, at = 1, lwd = 3, col.ticks = 'red')
axis(side = 4, at = 1, lwd = 3, col.ticks = 'red')
rect(xleft = 1980, xright = 2025, ybottom = 0, ytop = 2,
	col = 'red', density = 0, lwd = 3)
rect(xleft = 1960, xright = 2026, ybottom = 3, ytop = 29,
	col = 'blue', density = 0, lwd = 3)
legend('topleft', legend = sort(df$Ratio.v), inset = 0.03, cex = 1,
	lty = 'solid', lwd = 2, col = 1:(length(ratio.v.wide)-1))



		# 20세기 이전
par(mar = c(5, 5, 4, 2), xpd = TRUE)
matplot(ratio.v.former.wide$Year,scale(ratio.v.former.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 3, col = 1:length(ratio.v.former$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병명 상대빈도(표준화, 20세기 이전)')
title('바이러스명/감염병명 상대빈도(표준화, 20세기 이전)')
	# first phase
rect(xleft = 1795, xright = 1840, ybottom = -0.01, ytop = 1,
	col = 'red', density = 0, lwd = 4)
	# second phase
rect(xleft = 1925, xright = 2020, ybottom = 0.05, ytop = 0.8,
	col = 'blue', density = 0, lwd = 4)
legend('top', legend = sort(df$Ratio.v)[-c(2:6)] , inset = 0.03, cex = 0.9,
	lty = 'solid', lwd = 2, col = 1:length(ratio.v.former$Phrase))


		# 20세기 이후
	## 생긴게 너무 개판
par(mar = c(5, 5, 5, 2), xpd = TRUE)
matplot(ratio.v.latter.wide$Year, scale(ratio.v.latter.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 4, col = 1:length(ratio.v.latter$Phrase),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병명 상대빈도(표준화, 20세기 이후)')
title('바이러스명/감염병명 상대빈도(표준화, 20세기 이후)')


		# 20세기 이후 / 연도 조정
ratio.v.latter.trunc <- ngram(latter$Ratio.v, year_start = 1950)
ratio.v.latter.trunc.wide <- spread(ratio.v.latter.trunc[-4], Phrase, Frequency)
par(mar = c(5, 5, 4, 2), xpd = TRUE)
matplot(ratio.v.latter.trunc.wide$Year, scale(ratio.v.latter.trunc.wide[-1]), 
	type = 'l', lty = 'solid', lwd = 4, col = col = 1:length(df$Ratio.v[2:6]),  
	cex.axis = 0.6, xlab = '연도', ylab = '바이러스명/감염병 상대빈도(표준화, 20세기 이후, 1950~)')
title('바이러스명/감염병 상대빈도(표준화, 20세기 이후, 1950~)')
legend('topleft', legend = sort(df$Ratio.v)[2:6], inset = 0.02, cex = 0.8,
	lty = 'solid', lwd = 2, col = 1:length(df$Ratio.v[2:6]))

# 바이러스명에 virus 
ratio.v['Virus'] <- ifelse(grepl('virus', ratio.v$Phrase), 'virus', 'X virus')
ratio.v$Virus <- as.character(ratio.v$Virus)
VirusON <- ratio.v %>% 
	group_by(Year, Virus) %>%
	summarize(Median = median(Frequency), 
			Mean = mean(Frequency))

Med <- ggplot(VirusON, aes(x = Year)) +
geom_line(aes(y = Median, color = Virus), lwd = 1.8)+
ggtitle('바이러스명/감염병명 Median') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
Mean <- ggplot(VirusON, aes(x = Year)) +
geom_line(aes(y = Mean, color = Virus), lwd = 1.8)+
ggtitle('바이러스명/감염병명 Mean') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

gridExtra::grid.arrange(Med, Mean, ncol = 2)

## 더 해볼 것 > 감염경로별, 발원지별 분류 

# 보고연도 코호트 연구
		# 상대빈도 최고치 찍은 연도 찾기 
ratio.v.t <- t(ratio.v.wide)
max.id <- c()

for (i in 1:nrow(ratio.v.t)){
	id <- which.max(ratio.v.t[i, ])
	max.id <- c(max.id, id)	} 
max.year <- ratio.v.wide$Year[max.id]

Max <- data.frame(Max.year = max.year)
ggplot(Max, aes(x = Max.year)) +
geom_histogram(bins = 10, color = 'black', fill = 'grey') +
ggtitle('상대빈도 최고치 연도 히스토그램') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

max.df <- data.frame()
Ngram <- data.frame()
for (i in 1:length(df$Ratio.v)){
	Ngram <- ngram(df$Ratio.v[i], year_start = ifelse(max.year[i] <= 1819, 1800, (max.year[i] - 20)), year_end = ifelse(max.year[i] >= 2000, 2019, (max.year[i] + 20)))
	max.df <- rbind(as.data.frame(Ngram), max.df)

