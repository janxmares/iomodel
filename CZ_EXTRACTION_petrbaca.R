## DESCRIPTION ----
# https://apl.czso.cz/nufile/sut/SIOTpxp2015s.xls 
#

## INTRO ----
rm(list = ls())
graphics.off(); op <- par(no.readonly = T)

## LIBRARIES ----

# required packages
libs <- c("readxl")
      
# load and install (if not installed)
invisible(lapply(libs, FUN = function(lib) if(!require(lib, character.only = T))
  { install.packages(lib); library(lib, character.only = T) }) )

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
  
  if(!require("readxl")) {install.packages("readxl"); library("readxl")}
  
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

wrap_strings <- function(vector_of_strings, width = 40){sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")})}

## GET_DATA ----

IO <- getCZIO(filepath = "cz/SIOToxo_dom2015s.xls")

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
agg[9 , 48:53] <- 1            # G
agg[10, 54:56] <- 1           # G
agg[11, 57] <- 1           # G
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

extraction_data <- data.frame(
  ProductionDecline = c(58, 53, 66, 57, 62, 68, 96, 92, 17, 60, 45, 48, 68, 84), # in Percent
  Duration = wks * 7, # calendar, ie not working days
  row.names = NACE
)
extraction_data$ExtractionCoefficient <- ((366 - extraction_data$Duration) / 366) + (extraction_data$ProductionDecline / 100) * (extraction_data$Duration / 366)

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
               "Verrejna sprava a obrana, vzdelavani, zdravotni a socialni pece",
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





