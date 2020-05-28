rm(list = ls())

library(data.table)
library(readxl)
library(dplyr)
library(here)

file_name_total <- "fstimulus/data/SIOT_2015s_CZ_CPA184_pxp.xls"
file_name_dom <- "fstimulus/data/SIOT_DOM_2015s_CZ_CPA184_pxp.xls"
multiplier_output_file <- "fstimulus/results/multipliers_table.csv"
fiscal_stimulus_file <- "fstimulus/data/fiscal_stimulus.xls"
fiscal_stimulus_effects_file <- "fstimulus/results/stimulus_effects_table.csv"

try <- readxl
# Reading data, total table and preparing the matrices
year <- read_excel(path = here(file_name_total), range = "A3:A3",col_names = FALSE)
sector_codes <- read_excel(here(file_name_total), range = "A9:A192",col_names = FALSE)
sector_names <- read_excel(file_name_total, range = "B9:B192",col_names = FALSE)
flow_matrix_TOT.MATRIX <- as.matrix(read_excel(file_name_total, range = "C9:GD192",col_names = FALSE))
final_use_TOT.CVECTOR <- as.matrix(read_excel(file_name_total, range = "GL9:GL192",col_names = FALSE))
total_use_TOT.CVECTOR <- as.matrix(read_excel(file_name_total, range = "GM9:GM192",col_names = FALSE))
value_added_TOT.RVECTOR <- read_excel(file_name_total, range = "C203:GD203",col_names = FALSE)
output_TOT.RVECTOR <- read_excel(file_name_total, range = "C204:GD204",col_names = FALSE)
resources_TOT.RVECTOR <- read_excel(file_name_total, range = "C206:GD206",col_names = FALSE)
labour_TOT.RVECTOR <- read_excel(file_name_total, range = "C208:GD208",col_names = FALSE)
exports_TOT.CVECTOR <- read_excel(file_name_total, range = "GK9:GK192",col_names = FALSE)

# Reading data, table with domestic flows and preparing the matrices
flow_matrix_DOM.MATRIX <- as.matrix(read_excel(file_name_dom, range = "C9:GD192",col_names = FALSE))
final_use_DOM.CVECTOR <- read_excel(file_name_dom, range = "GL9:GL192",col_names = FALSE)
total_use_DOM.CVECTOR <- read_excel(file_name_dom, range = "GM9:GM192",col_names = FALSE)
final_use_DOM.CVECTOR <- as.matrix(read_excel(file_name_dom, range = "GL9:GL192",col_names = FALSE))
total_use_DOM.CVECTOR <- read_excel(file_name_dom, range = "GM9:GM192",col_names = FALSE)
final_use_IMP_TOTAL <- read_excel(file_name_dom, range = "GL195:GL195",col_names = FALSE)
change_inventories_TOTAL <- read_excel(file_name_dom, range = "GJ196:GJ196",col_names = FALSE)
exports_DOM.CVECTOR <- read_excel(file_name_dom, range = "GK9:GK192",col_names = FALSE)
I_DOM.MATRIX <- as.matrix(read_excel(file_name_dom, range = "GI9:GJ192",col_names = FALSE))
G_DOM.MATRIX <- as.matrix(read_excel(file_name_dom, range = "GG9:GH192",col_names = FALSE))
C_DOM.MATRIX <- as.matrix(read_excel(file_name_dom, range = "GF9:GF192",col_names = FALSE))
labour_compensation.RVECTOR <- read_excel(file_name_total, range = "C197:GD197",col_names = FALSE)

# Read fiscal stimulus file
stimulus.CVECTOR <- as.matrix(read_excel(fiscal_stimulus_file, range = "C9:C192",col_names = FALSE))

# Preparing vectors with relative shares and auxiliary data
EMP_output_ratio.CVECTOR <-t(labour_TOT.RVECTOR/output_TOT.RVECTOR)
VA_output_ratio.CVECTOR <- t(value_added_TOT.RVECTOR/output_TOT.RVECTOR)
LCOMP_output_ratio.CVECTOR <- t(labour_compensation.RVECTOR/output_TOT.RVECTOR)

# Values of parameters, initial values
mat_dim <- ncol(flow_matrix_TOT.MATRIX)
previous_output <- as.matrix(rep(0, 184))
limit <- 0.1
test_var <- as.matrix(rep(0, 10000))
increment <- 10000

# Calculating A matrices
A_TOT.MATRIX <- sweep(flow_matrix_TOT.MATRIX, MARGIN = 2, t(resources_TOT.RVECTOR),"/")
A_DOM.MATRIX <- sweep(flow_matrix_DOM.MATRIX, MARGIN = 2, t(output_TOT.RVECTOR),"/")

# Leontief inversion
IminA_TOT.MATRIX <- diag(mat_dim) - (A_TOT.MATRIX)
IminA_DOM.MATRIX <- diag(mat_dim) - (A_DOM.MATRIX)
Leontief_TOT.MATRIX <- solve(IminA_TOT.MATRIX)
Leontief_DOM.MATRIX <- solve(IminA_DOM.MATRIX)

# Checking the results
x_tot <- Leontief_TOT.MATRIX %*% final_use_TOT.CVECTOR
x_dom <- Leontief_DOM.MATRIX %*% final_use_DOM.CVECTOR
  
# Getting basic output multipliers
Output_mutiplier_TOT <- as.matrix(colSums(Leontief_TOT.MATRIX))
Output_mutiplier_DOM <- as.matrix(colSums(Leontief_DOM.MATRIX))

# Getting basic value added multipliers
VA_matrix_TOT <- sweep(Leontief_TOT.MATRIX, MARGIN = 1, VA_output_ratio.CVECTOR , '*') 
VA_matrix_DOM <- sweep(Leontief_DOM.MATRIX, MARGIN = 1, VA_output_ratio.CVECTOR , '*') 
VA_multiplier_TOT <- as.matrix(colSums(VA_matrix_TOT))
VA_multiplier_DOM <- as.matrix(colSums(VA_matrix_DOM))

# Gettin basic employment multipliers
EMP_matrix_TOT <- sweep(Leontief_TOT.MATRIX, MARGIN = 1, EMP_output_ratio.CVECTOR, '*') 
EMP_matrix_DOM <- sweep(Leontief_DOM.MATRIX, MARGIN = 1, EMP_output_ratio.CVECTOR, '*') 
EMP_multiplier_TOT <- as.matrix(colSums(EMP_matrix_TOT))
EMP_multiplier_DOM <- as.matrix(colSums(EMP_matrix_DOM))

# Save basic multipliers tables
Multipliers_table <- data.frame(cbind(sector_codes, sector_names, Output_mutiplier_DOM, Output_mutiplier_TOT, VA_multiplier_DOM, VA_multiplier_TOT, EMP_multiplier_DOM, EMP_multiplier_TOT ))
setnames(Multipliers_table, c("K�d kategorie v�robku","N�zev kategorie","Output multipliers - DOM","Output multipliers - TOT","VA multipliers - DOM","VA multipliers - TOT","Employment multipliers - DOM","Employment multipliers - TOT"))

write.csv(Multipliers_table, file=multiplier_output_file, row.names = F) 


##########################################################################################
# Calculating effects of fiscal interventions

# Initial increase of demand due to fiscal stimuls + type I multiplier effects - on domestic data

Output_change_DOM_stimulus <- Leontief_DOM.MATRIX %*% stimulus.CVECTOR
VA_change_DOM_stimulus <- Output_change_DOM_stimulus * VA_output_ratio.CVECTOR
Employment_change_DOM_stimulus <- Output_change_DOM_stimulus * EMP_output_ratio.CVECTOR



# Adding induced effects - attempt to get a simple version of Type II multipliers
# Assumption: stable structure of demand (based on C+I+G)
# Exports assumed to remain constant
# Deriving pattern of final use
# We take total final domestic expenditures without exports, i.e. (C+I+G) 
# Then we calculate the shares in demand, where we assume constant proportion still spent on all imported products

C_I_G_DOM <- final_use_DOM.CVECTOR - exports_DOM.CVECTOR
I_DOM <- rowSums(I_DOM.MATRIX)
G_DOM <- rowSums(G_DOM.MATRIX)

C_I_G_TOT <- final_use_TOT.CVECTOR - exports_TOT.CVECTOR
total_expenditures_TOT <- colSums(final_use_TOT.CVECTOR)
total_expenditures_DOM <- colSums(final_use_DOM.CVECTOR)
total_exports_TOT <- colSums(exports_TOT.CVECTOR)

total_expenditures_wEXP_TOT <- total_expenditures_TOT - total_exports_TOT
demand_shares.CVECTOR <- C_I_G_DOM/total_expenditures_wEXP_TOT

total_CONS <- colSums(C_DOM.MATRIX)
total_W <- rowSums(labour_compensation.RVECTOR)

demand_shares_Cbased.CVECTOR <- C_DOM.MATRIX/total_CONS

# Cycle used for iterative solution

demand_change <- stimulus.CVECTOR
LCOMP_change <- 0
VA_change <- 0
i <-0

while (increment>limit)
{
# Take total change in value added and calculate implications for final demand

demand_change <- as.matrix(stimulus.CVECTOR + LCOMP_change*demand_shares_Cbased.CVECTOR)
#demand_change <- as.matrix(stimulus.CVECTOR + .9*VA_change*demand_shares_Cbased.CVECTOR)
updated_output <- Leontief_DOM.MATRIX %*% demand_change
updated_LCOMP <- updated_output * LCOMP_output_ratio.CVECTOR
#updated_VA <- updated_output * VA_output_ratio.CVECTOR
LCOMP_change <-colSums(updated_LCOMP)
#LCOMP_change <-colSums(updated_VA)


increment <- as.numeric(colSums(updated_output) - colSums(previous_output))

test_var[i]<-increment
i <- i+1
previous_output <- updated_output
}


output_T2_effect <- updated_output
VA_T2_effect <- updated_output * VA_output_ratio.CVECTOR
Employment_T2_effect <- updated_output * EMP_output_ratio.CVECTOR

Output_T2_tot <- colSums(updated_output)/rowSums(output_TOT.RVECTOR)*100
Output_T1_tot <- colSums(Output_change_DOM_stimulus)/rowSums(output_TOT.RVECTOR)*100


Stimulus_effects_table <- data.frame(cbind(sector_codes, sector_names, stimulus.CVECTOR, Output_change_DOM_stimulus, output_T2_effect, VA_change_DOM_stimulus, VA_T2_effect, Employment_change_DOM_stimulus))
setnames(Stimulus_effects_table, c("K�d kategorie v�robku","N�zev kategorie","Stimulus", "Dopad na produkci TI - (mil. Kc), DOM","Dopad na produkci TII - (mil. Kc), DOM","Dopad na pridanou hodnotu TI - - (mil. Kc), DOM","Dopad na pridanou hodnotu TII - - (mil. Kc), DOM","Dopad na pocet pracovn�ch m�st TI"))
write.csv(Stimulus_effects_table, file=fiscal_stimulus_effects_file, row.names = F) 

