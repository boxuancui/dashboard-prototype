library(data.table)

bridge <- fread("data/reference_bridge.csv", sep=",")
setkeyv(bridge, c("DateKey", "Placement"))
