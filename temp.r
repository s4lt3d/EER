# A forecaster

filter.pop <- tail(filter(advisor.history, cnum==50)$money, n=40)
ts.pop <- ts(filter.pop, frequency = 4)
hw <- HoltWinters(ts.pop)
forecast <- predict(hw, n.ahead = 20, prediction.interval = T, level = 0.8)
plot(hw, forecast)


sql.file <- paste(this.dir, "/eertest.sqlite", sep="")
sql.db <- src_sqlite(sql.file, create = F)
advisor.sqlite <- copy_to(sql.db, advisor.current, name="advisor_history", indexes = list("cnum", "round_num", "turns_played"), temporary = F)

collect(advisor.sqlite)


