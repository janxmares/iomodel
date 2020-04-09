# I-O model sketch
# JM 4/2020

#
#

library(data.table)
library(here) # set the WD to git folder
library(XLConnect) # read xls files

# load data
io.wb <- loadWorkbook(here("data/SIOToxo2015s.xls"))

# read data into data table
io.cz <- data.table(readWorksheet(io.wb, sheet="SIOT_oxo", startRow=9, startCol=3,
                                                           endRow = 90, endCol = 94, header = F))
# set column names
cnames <- paste("nace", io.cz$Col92, sep = ".")
setnames(io.cz, c(cnames, "icons.total","hh.fcons","g.fcons","nonp.fcons","gfcf",
                  "inventories","export","fcons.total","total","nace.code")) 

# tech. matrix
tech.m <- io.cz[, cnames, with = F]

# normalize the values by overall production
tech.m.norm <- sweep(tech.m, MARGIN = 2, io.cz$total, '/')

# Leontief matrix
IminA <- diag(ncol(tech.m.norm)) - tech.m.norm
Leontief <- solve(IminA)

#
#

# adjust the values by the value added (avoid double-counting)
# read the gross value added and total production
va.cz <- data.table(readWorksheet(io.wb, sheet="SIOT_oxo", startRow = 101, startCol = 3,
                                                           endRow = 101, endCol = 84, header = F))

va.dt <- data.table(output = io.cz$total, gva = c(t(va.cz)))
va.dt[, gvashare := gva/output]

# gross value added multipliers
Leontief.VA <- va.dt$gvashare %*% Leontief

#
#

# Income multipliers based on gross wages to HHs
# read the wages, salaries and social insurance payments within sectors
w.cz <- data.table(readWorksheet(io.wb, sheet="SIOT_oxo", 
                                 startRow = 94, startCol = 3,
                                 endRow = 95, endCol = 84, header = F))

w.dt <- as.data.table(t(w.cz))
setnames(w.dt, c("wages.salaries","social.insurance"))

# compute overall compentasion
w.dt[, compensation := wages.salaries + social.insurance]
w.dt[, output := io.cz$total]

# normalizing HH compensation by overall production within sectors
w.dt[, compshare := compensation/output]

# HH income multipliers
Leontief.comp <- w.dt$compshare %*% Leontief

#
#


# sector output multipliers
multipliers <- colSums(Leontief)

# data table output
multipliers.f <- data.table(sector = row.names(Leontief), output.multiplier = multipliers, gva.multiplier = c(Leontief.VA), comp.multiplier = c(Leontief.comp))

# read sector full names
conc <- data.table(read.csv(here("data/nace.csv"), header = T, encoding="UTF-8", colClasses = "character"))
setnames(conc, c("sektor","nace.kod"))

conc[, sector := paste("nace", nace.kod, sep = ".")]
conc[, nace.kod := NULL]

# merge full names with multipliers
multipliers.f <- merge(multipliers.f, conc, by = c("sector"))
setnames(multipliers.f, c("sector"), c("nace.kod"))

# order the sector by multipliers
m.ordered <- multipliers.f[order(-output.multiplier),]

# Check
# head(m.ordered)
# View(m.ordered)

# write into file
write.csv(m.ordered, file="io_multipliers.csv", row.names = F)