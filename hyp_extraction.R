## DESCRIPTION ----
# simulation of COVID downturn
# 
# data: https://apl.czso.cz/nufile/sut/SIOToxo2015s.xls 
#
# contributors: Petr Baca, Jan Mares, Vilem Semerak
#


## INTRO ----
# rm(list = ls())

## LIBRARIES ----

# load packages
libs <- c("readxl","here","data.table","RColorBrewer","ggplot2") # here intelligently sets the WD
invisible(lapply(libs, function(lib){library(lib, character.only = T)}))

#

## FUNCTIONS ----

getCZIO <- function(filepath = NULL) {
  
  # extract IO matrices from SIOT (CZSO), indu x indu
  # PARAMETERS:
  #   filepath ... path to xls file
  # VALUE: a list of basic IO matrices
  #   vectors are transformed to column vectors 
  #
  #   (notation follows OECD ICIO notation)
  #
  #    Z   | FD  | X  
  #    TLS | TLS |
  #    IM  |
  #    VA  |
  #    X   |
  #    
  
  # read data
  dat_raw <- as.data.frame(read_excel(path = filepath))
  
  # rows & cols to omit
  i_rm <- 1:7        # rows to omit
  j_rm <- c(1:2, 94) # cols to omit
  
  use <- unlist(dat_raw[7,][-j_rm]) # uses (rows) 
  src <- unlist(dat_raw[,1][-i_rm]) # sources (cols)
  # length(use); length(src) # check
  
  mat <- dat_raw[-i_rm, -j_rm] # data matrix
  mat <- matrix(as.numeric(unlist(mat)), nr = nrow(mat), nc = ncol(mat))
  # mat[1:5,1:5]
  # dim(mat) # check
  
  colnames(mat) <- use
  rownames(mat) <- src
  
  # remove subtotals
  i_st <- c(83, 86, 94, 95) # subtotals in rows
  j_st <- c(83, 90) # subtotals in columns
  
  # remove
  mat <- mat[-i_st, -j_st]
  # basic data cleaning done :-)
  
  # matrices
  Z   <- mat[ 1:82,  1:82]
  FD  <- mat[ 1:82, 83:88]
  X   <- matrix(mat[ 1:82, 89], nc = 1)

  TLS <- matrix(mat[83   ,  1:82], nc = 1)
  IM  <- matrix(mat[84   ,  1:82], nc = 1)
  VA  <- t(mat[85:91,  1:82]) # transpose
  
  # add names
  dimnames(X) <- list(dimnames(Z)[[1]], "Output")
  dimnames(FD)[[2]] <- c("C_Household", "C_Govt", "C_NPISH", "GFCF", "I", "Export")
  
  
  dimnames(TLS) <- list(dimnames(Z)[[1]], "TaxesLessSubsidies")
  dimnames(IM) <- list(dimnames(Z)[[1]], "Import")
  dimnames(VA) <- list(dimnames(Z)[[1]], c("WagesSalaries", "Insurance", "OtherTaxes", "OtherSubsidies", "Depreciation", "NetOperatingSurplus", "NetMixedIncome"))

  # return
  res <- list(
    Z   = Z,    # intermediates use
    FD  = FD,   # final use
    X   = X,    # output
    TLS = TLS, # taxes less subsidies
    IM  = IM,
    VA  = VA)
  return(res)
  
}

## GET_DATA ----

IO <- getCZIO(filepath = "data/SIOToxo_dom2015s.xls")

# vector of NACE codes
NACE <- c("A", "B+D+E", "C", "F", "G", "H", "I-55", "I-56", "J", "K", "L", "M+N", "O+P+Q", "R+S+T+U")

# aggregation matrix
# read from file
# agg <- as.matrix(read.csv("cz/aggreg_matrix_csv.csv", header = F, sep = ","))

# build aggregation matrix directly in R
agg <- matrix(0, 14, 82)
agg[1 , 1:3] <- 1           # A
agg[2 , c(4:8, 33:35)] <- 1 # B+D+E
agg[3 , 9:32] <- 1          # C
agg[4 , 36:38] <- 1         # F
agg[5 , 39:40] <- 1         # G
agg[6 , 41:45] <- 1         # H
agg[7 , 46] <- 1            # I-55
agg[8 , 47] <- 1            # I-56
agg[9 , 48:53] <- 1            # J
agg[10, 54:56] <- 1           # K
agg[11, 57] <- 1           # L
agg[12, 58:70] <- 1           # G
agg[13, 71:75] <- 1           # G
agg[14, 76:82] <- 1           # G

# add names
dimnames(agg) <- list(NACE, dimnames(IO$Z)[[1]])

# check
# colSums(agg)

## AGGREGATION ----
Z  <- agg %*% IO$Z %*% t(agg) # transaction matrix
X  <- agg %*% IO$X # output vector (column)
FD <- agg %*% IO$FD # final demand matrix
VA <- agg %*% IO$VA # value added matrix

# TLS <- agg %*% IO$TLS # taxes less subsidies vector

# shares
VAshare <- solve(diag(as.numeric(X))) %*% VA
dimnames(VAshare)[[1]] <- NACE

# add NACE names
dimnames(Z) <- list(NACE, NACE)
dimnames(X)[[1]] <- dimnames(FD)[[1]] <- dimnames(VA)[[1]] <- NACE

# calculate technical coef matrix & Leontief inverse
A <- Z %*% solve(diag(as.numeric(X))) # tech coef mat
L <- solve(diag(nrow(A)) - A)
dimnames(A) <- dimnames(L) <- list(NACE, NACE)

## BUILD_HYPOTHETICAL_EXTRACTION_MATRIX ----

wks <- 6 # duration of outages in weeks

# extraction information
# ProductionDecline - decline of sector production due to coronavirus in %
# Duration - duration of decline (days), 366 days year is assumed 
# ExtractionCoefficient - level of production after decline in %

NACEnames <- c("Zemedelstvi, lesnictvi a rybarstvi",
               "Prumysl, tezba a dobyvani (bez zpracovatelskeho prumyslu)",
               "Zpracovatelsky prumysl",
               "Stavebnictvi",
               "Obchod",
               "Doprava",
               "Ubytovani",
               "Pohostinstvi",
               "Informacni a komunikacni cinnosti",
               "Peneznictvi a pojistovnictvi",
               "Cinnosti v oblasti nemovitosti",
               "Profesni, vedecke, technicke a administrativni cinnosti",
               "Verejna sprava a obrana, vzdelavani, zdravotni a socialni pece",
               "Ostatni cinnosti")
               
extraction_data <- data.frame(
  # ProductionDecline = c(50, 48, 55, 50, 59, 59, 95, 90, 29, 52, 44, 45, 61, 81), # in Percent, scenario 1
  # ProductionDecline = c(20,20,20,20,20,20,20,20,20,20,20,20,20,20), # in Percent, homogenous across sectors, scenario 2
  ProductionDecline = c(25, 24, 22.5, 25, 28.5, 28.5, 80, 80, 14.5, 26, 22, 22.5, 30.5, 40.5), # in Percent, homogenous across sectors, scenario 3
  Duration = wks * 7, # calendar, ie not working days
  row.names = NACE
)

extraction_data$ExtractionCoefficient <- ((366 - extraction_data$Duration) / 366) + ((100 - extraction_data$ProductionDecline) / 100) * (extraction_data$Duration / 366)

#
#
# extraction matrix

EM <- crossprod(t(extraction_data$ExtractionCoefficient))
diag(EM) <- extraction_data$ExtractionCoefficient
dimnames(EM) <- list(NACE, NACE)

## HYPOTHETICAL_EXTRACTION ----
# '_bar' suffix

# apply extraction and calc Leontief mat
A_bar <- A * EM
L_bar <- solve(diag(nrow(A_bar)) - A_bar)

# Adjusting the final consumption vector (elastic response - consumption will be sourced from abroad)
FD_bar <- sweep(FD, 1, diag(EM), "*")

# Calculate the effect on output & value added
X_bar  <- apply(L_bar %*% FD_bar, 1, sum) # output
VA_bar <- X_bar * VAshare # value added

# change in VA overall
# sum(VA_bar)/sum(VA) - 1)*100


#
#

VA_totals_bar <- apply(VA_bar, 2, sum)
VA_totals <- apply(VA, 2, sum)

#
#

# Change in HDP (output)
gdp_change <- sum(X_bar) / sum(X) * 100 - 100 
# gdp_change
                                                                                                            
#
#

VA_totals_bar <- apply(VA_bar, 2, sum)
VA_totals <- apply(VA, 2, sum)

# Change in wages
(VA_totals_bar/VA_totals-1)*100

#
#
#
#
#

# Separate consumption
Cons <- rowSums(FD[,1:3])
Cons_bar <- rowSums(FD_bar[,1:3])

Cons_total <- sum(Cons) 
Cons_total_bar <- sum(Cons_bar)

# Change in consumption
(Cons_total_bar/Cons_total-1)*100

#
#
#
#
#

# read output by NACE in current prices to comput the changes over time and especially in 2009
prod_nace <- data.table(read.csv(here("data/produkce_odvetvi.csv"), header = F))
prod_nace[, V1 := NULL] # drop the NACE names

# aggregation matrix for production categories
agg <- matrix(0, 14, 86)
agg[1, 1:3] <- 1 # A 
agg[2 , c(4:8, 33:37)] <- 1 # B+D+E
agg[3 , 9:32] <- 1          # C
agg[4 , 38:40] <- 1         # F
agg[5 , 41:43] <- 1         # G
agg[6 , 44:38] <- 1         # H
agg[7 , 49] <- 1            # I-55
agg[8 , 50] <- 1            # I-56
agg[9 , 51:56] <- 1            # J
agg[10, 57:59] <- 1           # K
agg[11, 60] <- 1           # L
agg[12, 61:73] <- 1           # G
agg[13, 74:78] <- 1           # G
agg[14, 79:86] <- 1           # G

# sum up to NACE 1-level
prod_agg <- agg %*% as.matrix(prod_nace) 

prod_agg <- as.data.table(prod_agg)
setnames(prod_agg, paste("y", c(2000:2018), sep="_"))

# add the nace names
prod_agg[, odvetvi := NACEnames]

# convert to long format
prod_agg_l <- melt(prod_agg, id.vars = c("odvetvi"), variable.name = "year", value.name = "value")
prod_agg_l[, year := gsub("y_","", year)] # drop the "y_" in year column

# order by odvetvi
prod_agg_l <- prod_agg_l[order(odvetvi, year),]

# compute growth rates
prod_agg_l[, pct_ch := grate(value), by = c("odvetvi")]

# filter only year 2009
prod_agg_l_2009 <- prod_agg_l[year == 2009,]
prod_agg_l_2009[, c("year", "value") := NULL]

#
#

# comparison with 2009
comp <- data.table(covid_lockdown = (as.vector(X_bar / X * 100 - 100)))
comp[, odvetvi := NACEnames]

# merge with 2009
comp <- merge(comp, prod_agg_l_2009, by = c("odvetvi"))
comp[, slowdown2009 := pct_ch*100]
comp[, pct_ch := NULL]

#
#

# Add half slowdown, need to go back up for now :/
comp[, covid_lockdown_half := (as.vector(X_bar / X * 100 - 100))]
comp_long <- melt(comp, id = c("odvetvi"), variable.name = "crisis", value.name = "pct_change")
comp <- comp[order(-covid_lockdown), ]

# Figure formatting
comp_long[, tryf := factor(odvetvi, levels = comp$odvetvi)] # reorder columns
comp_long[crisis == "covid_lockdown", crisis := "2020 lockdown"]
comp_long[crisis == "covid_lockdown_half", crisis := "2020 lockdown / 2"]
comp_long[crisis == "slowdown2009", crisis := "2009 slowdown"]

# plot comparison
fig1 <- ggplot(comp_long, aes(x = pct_change, y=tryf, fill=crisis))

fig1 + geom_col(position = position_dodge(), orientation = "y") + xlab("% change of output") + ylab("") + scale_fill_brewer(name = "Crisis", palette = "Set2") +
       theme(legend.title=element_text(face = "bold"))

#
#

## EXPORT ----
NACEnames <- c("Zemedelstvi, lesnictvi a rybarstvi",
               "Prumysl, tezba a dobyvani (bez zpracovatelskeho prumyslu)",
               "Zpracovatelsky prumysl",
               "Stavebnictvi",
               "Obchod",
               "Doprava",
               "Ubytovani",
               "Pohostinstvi",
               "Informacni a komunikacni cinnosti",
               "Peneznictvi a pojistovnictvi",
               "Cinnosti v oblasti nemovitosti",
               "Profesni, vedecke, technicke a administrativni cinnosti",
               "Verejna sprava a obrana, vzdelavani, zdravotni a socialni pece",
               "Ostatni cinnosti")

export <- data.frame(
  NACE_kod           = NACE,
  NACE_nazev         = NACEnames,
  redukcni_parametry = diag(EM),
  output_po_soku     = X_bar,
  output_zmena_pct   = (as.vector(X_bar / X * 100 - 100)),
  VA_absolutni_zmena = apply(VA_bar - VA, 1, sum),
  VA_zmena_pct       = apply(VA_bar, 1, sum) / apply(VA, 1, sum) * 100 - 100,
  stringsAsFactors = F, row.names = NULL)

write.table(export, "DOM_AGG_hypex_2015.csv", sep = ",", row.names = F)

## GRAPHICS ----

d_plot <- export[,c("NACE_nazev", "output_zmena_pct")]
d_plot <- d_plot[order(d_plot$output_zmena_pct),]

png("output_impact.png", width = 800, height = 600)

par(op)
par(mar = c(2, 0.5, 2, 2), oma = c(2, 15, 4, 3))

# output
yc <- barplot(d_plot$output_zmena_pct, col = "#001278", border = NA, horiz = T, xaxt = "n")
axis(1, -100:100 * 2); axis(3, -100:100 * 2)
abline(v = -100:100 * 2, col = "#FFFFFF", lwd = 2)
mtext(1, line = 2, text = "%", adj = 1); mtext(3, line = 2, text = "%", adj = 1)

# numbers
mtext(side = 4, line = 0.5, at = yc, text = sprintf("%6.2f", d_plot$output_zmena_pct), las = 2, col = "#001278", adj = 0)

# labels
mtext(side = 2, at = yc, line = 2, text = wrap_strings(d_plot$NACE_nazev, 30), las = 2)

# add titles and source
par(mfrow = c(1, 1), oma = rep(0.1, 4), mar = c(3, 0.5, 2, 0.5), new = TRUE)
plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
mtext(3, cex = 1.6, adj = 0, font = 2, line = 0,
      text = "Dopad na vystup odvetvi")
mtext(3, cex = 1.1, adj = 0, font = 3, line = -1.2,
      text = paste0("celkovy odhadovany dopad na HDP ", sprintf("%0.1f", sum(X_bar) / sum(X) * 100 - 100), "%"))

# source
mtext(1, text = "Zdroj: EPT", adj = 1, line = 2, font = 2, cex = 0.8)

dev.off()