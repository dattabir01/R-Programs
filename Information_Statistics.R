# Code for calculating the Information statistics
# Defining a function
Inf_Stats <- function(x, y){
# Taking only the Relevant columns

                goodbad <- x
                score <- y

                dataN <- cbind(goodbad,score)
                dataN <- as.data.frame(dataN)
                goodbad <- as.numeric(dataN$goodbad)
                score <- dataN[,-1]
                dataNew <- cbind(goodbad, score)
                dataNew <- as.data.frame(dataNew)
                dataNew$goodbad <- dataNew$goodbad - 1
                
                # Sorting the data in descending order to create deciles
                dataNew_sort <- dataNew[order(-dataNew$score),]
                
                brks <- with(dataNew_sort, 
                             quantile(score,
                                      probs = seq(0,1, by=0.2)))
                
                dataNew_sort <- within(dataNew_sort, 
                                       decile <- cut(score, 
                                                     breaks = brks, 
                                                     labels = 1:5, 
                                                     include.lowest = TRUE))
                
                
                # Rolling up the data at the decile level
                library(sqldf)
                
                df = sqldf( "select decile, 
                                sum(goodbad) as Gi,
                                sum(score)/count(decile) as Scr,
                                count(case when goodbad = 0 then decile end) as Bi
                                from dataNew_sort
                                group by decile")
                
                df$decile = as.numeric(df$decile)
                df <- df[order(-df$decile),]
                
                totGood = sqldf("select sum(Gi) as totgood
                                    from df")
                totBad = sqldf("select sum(Bi) as totbad
                                   from df")
                totGood <- as.numeric(totGood$totgood)
                totBad <- as.numeric(totBad$totbad)
                
                df$Pb <- df$Bi / totBad
                df$Pg <- df$Gi / totGood
                
                df$Ik = (df$Pb - df$Pg) * (log(df$Pb/df$Pg))
                I = sum(df$Ik)
                
                print("Information Statistics:");I
                return(I)
}

# x <- data$goodbad
# y <- data$score
# Inf_Stats(x, y)
