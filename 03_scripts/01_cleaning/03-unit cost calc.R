##############   Much of the unit cost stuff is done in Excel. See spread sheet #####################
unitcost <- UNIT_COST
unitcost <- within(unitcost, indicator <- paste(Indicator, type,sep='.'))
unitcost <- unitcost[,c("indicator", "unitcost")]

unitcost <- spread(unitcost, indicator, unitcost)

unitcost2 <- cbind(ppp, unitcost)

unitcost.scaled <- mutate_at(unitcost2,vars(`fear.indirect`:`violentassault.indirect`), funs(.*scale) )

unitcost.scaled <- unitcost.scaled[,c(2,1,4:15)]

