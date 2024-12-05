# Load necessary libraries
library(lavaan)
library(dplyr)
library(psych)
library(readxl)

# Load the dataset
data <- read_excel("data.xlsx")

# ----------------------------------------------------
# CFA Measurment Model
# ----------------------------------------------------
cfa_model <- '
  # First-order LMX factors
  ProfessionalRespect =~ LMX_1 + LMX_8 + LMX_12
  Affect =~ LMX_3 + LMX_6 + LMX_10
  Contribution =~ LMX_4 + LMX_7 + LMX_11
  Loyalty =~ LMX_2 + LMX_5 + LMX_9
  
  # Liking factor
  Liking =~ Liking_1 + Liking_2 + Liking_3 + Liking_4
  
  # Allow all factors to correlate with each other
  ProfessionalRespect ~~ Affect + Contribution + Loyalty + Liking
  Affect ~~ Contribution + Loyalty + Liking
  Contribution ~~ Loyalty + Liking
  Loyalty ~~ Liking
  
  # Standardize all latent variables
  ProfessionalRespect ~~ 1*ProfessionalRespect
  Affect ~~ 1*Affect
  Contribution ~~ 1*Contribution
  Loyalty ~~ 1*Loyalty
  Liking ~~ 1*Liking
'
# Fit CFA model
fit_cfa <- cfa(cfa_model, data = data, estimator = "MLR")
# Summarize results
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# ----------------------------------------------------
# Baseline Model
# ----------------------------------------------------

# Get unstandardized factor loadings and error variances for Liking from Phase 1a
#fit baseline model
baseline_model <- '
  # First-order LMX factors
  ProfessionalRespect =~ LMX_1 + LMX_8 + LMX_12
  Affect =~ LMX_3 + LMX_6 + LMX_10
  Contribution =~ LMX_4 + LMX_7 + LMX_11
  Loyalty =~ LMX_2 + LMX_5 + LMX_9
  
  # Liking factor with fixed parameters from Phase 1a
  Liking =~ 1.000*Liking_1 + 1.381*Liking_2 + 1.223*Liking_3 + 1.518*Liking_4
  
  # Fix error variances for Liking indicators
  Liking_1 ~~ 0.848*Liking_1
  Liking_2 ~~ 0.248*Liking_2
  Liking_3 ~~ 0.428*Liking_3
  Liking_4 ~~ 0.220*Liking_4
  
  # Allow substantive factors to correlate with each other
  ProfessionalRespect ~~ Affect + Contribution + Loyalty
  Affect ~~ Contribution + Loyalty
  Contribution ~~ Loyalty
  
  # Fix correlations between Liking and substantive variables to zero
  ProfessionalRespect ~~ 0*Liking
  Affect ~~ 0*Liking
  Contribution ~~ 0*Liking
  Loyalty ~~ 0*Liking
  
  # Standardize all latent variables
  ProfessionalRespect ~~ 1*ProfessionalRespect
  Affect ~~ 1*Affect
  Contribution ~~ 1*Contribution
  Loyalty ~~ 1*Loyalty
  Liking ~~ 1*Liking
'

# Fit the baseline model
fit_baseline <- cfa(baseline_model, data = data, estimator = "MLR")
# Examine results
summary(fit_baseline, fit.measures = TRUE, standardized = TRUE)

# ----------------------------------------------------
# Method-U Model
# ----------------------------------------------------

methodU_model <- '
  # First-order LMX factors
  ProfessionalRespect =~ LMX_1 + LMX_8 + LMX_12
  Affect =~ LMX_3 + LMX_6 + LMX_10
  Contribution =~ LMX_4 + LMX_7 + LMX_11
  Loyalty =~ LMX_2 + LMX_5 + LMX_9
  
  # Liking factor with fixed parameters from Phase 1a
  Liking =~ 1.000*Liking_1 + 1.381*Liking_2 + 1.223*Liking_3 + 1.518*Liking_4
  
  # Fix error variances for Liking indicators
  Liking_1 ~~ 0.848*Liking_1
  Liking_2 ~~ 0.248*Liking_2
  Liking_3 ~~ 0.428*Liking_3
  Liking_4 ~~ 0.220*Liking_4
  
  # Add method factor loadings from Liking to substantive indicators (freely estimated)
  Liking =~ LMX_1 + LMX_8 + LMX_12 + 
            LMX_3 + LMX_6 + LMX_10 + 
            LMX_4 + LMX_7 + LMX_11 +
            LMX_2 + LMX_5 + LMX_9
            
  # Allow substantive factors to correlate with each other
  ProfessionalRespect ~~ Affect + Contribution + Loyalty
  Affect ~~ Contribution + Loyalty
  Contribution ~~ Loyalty
  
  # Fix correlations between Liking and substantive variables to zero
  ProfessionalRespect ~~ 0*Liking
  Affect ~~ 0*Liking
  Contribution ~~ 0*Liking
  Loyalty ~~ 0*Liking
  
  # Standardize all latent variables
  ProfessionalRespect ~~ 1*ProfessionalRespect
  Affect ~~ 1*Affect
  Contribution ~~ 1*Contribution
  Loyalty ~~ 1*Loyalty
  Liking ~~ 1*Liking
'

# Fit the Method U model
fit_methodU <- cfa(methodU_model, data = data, estimator = "MLR")
# Examine results
summary(fit_methodU, fit.measures = TRUE, standardized = TRUE)

# Compare with baseline model
anova(fit_baseline, fit_methodU)

# Check method factor loadings
standardizedSolution(fit_methodU)

##Compare substantive factor loadings to baseline factor loadings:
standardizedSolution(fit_baseline)

# ----------------------------------------------------
# Method-I Model
# ----------------------------------------------------

# Method I Model with within-construct equality constraints
methodI_model <- '
   # First-order LMX factors
   ProfessionalRespect =~ LMX_1 + LMX_8 + LMX_12
   Affect =~ LMX_3 + LMX_6 + LMX_10
   Contribution =~ LMX_4 + LMX_7 + LMX_11
   Loyalty =~ LMX_2 + LMX_5 + LMX_9
   
   # Liking factor with fixed parameters from Phase 1a
   Liking =~ 1.000*Liking_1 + 1.381*Liking_2 + 1.223*Liking_3 + 1.518*Liking_4
   
   # Fix error variances for Liking indicators
   Liking_1 ~~ 0.848*Liking_1
   Liking_2 ~~ 0.248*Liking_2
   Liking_3 ~~ 0.428*Liking_3
   Liking_4 ~~ 0.220*Liking_4
   
   # Add method factor loadings from Liking to substantive indicators with within-construct equality constraints
   Liking =~ a*LMX_1 + a*LMX_8 + a*LMX_12 +         # Professional Respect
             b*LMX_3 + b*LMX_6 + b*LMX_10 +         # Affect
             c*LMX_4 + c*LMX_7 + c*LMX_11 +         # Contribution
             d*LMX_2 + d*LMX_5 + d*LMX_9            # Loyalty
   
   # Allow substantive factors to correlate with each other
   ProfessionalRespect ~~ Affect + Contribution + Loyalty
   Affect ~~ Contribution + Loyalty
   Contribution ~~ Loyalty
   
   # Fix correlations between Liking and substantive variables to zero
   ProfessionalRespect ~~ 0*Liking
   Affect ~~ 0*Liking
   Contribution ~~ 0*Liking
   Loyalty ~~ 0*Liking
   
   # Standardize all latent variables
   ProfessionalRespect ~~ 1*ProfessionalRespect
   Affect ~~ 1*Affect
   Contribution ~~ 1*Contribution
   Loyalty ~~ 1*Loyalty
   Liking ~~ 1*Liking
'

# Fit the Method I model
fit_methodI <- cfa(methodI_model,data = data,estimator = "MLR")
# Examine results
summary(fit_methodI, fit.measures = TRUE, standardized = TRUE)

# Compare with Method U model using chi-square difference test
anova(fit_methodU, fit_methodI)

# ----------------------------------------------------
# Method-R Model
# ----------------------------------------------------

# Method-R Model (based on retained Method U model, with baseline correlations fixed)
methodR_model <- '
   # First-order LMX factors
   ProfessionalRespect =~ LMX_1 + LMX_8 + LMX_12
   Affect =~ LMX_3 + LMX_6 + LMX_10
   Contribution =~ LMX_4 + LMX_7 + LMX_11
   Loyalty =~ LMX_2 + LMX_5 + LMX_9
   
   # Liking factor with fixed parameters from Phase 1a
   Liking =~ 1.000*Liking_1 + 1.381*Liking_2 + 1.223*Liking_3 + 1.518*Liking_4
   
   # Fix error variances for Liking indicators
   Liking_1 ~~ 0.848*Liking_1
   Liking_2 ~~ 0.248*Liking_2
   Liking_3 ~~ 0.428*Liking_3
   Liking_4 ~~ 0.220*Liking_4
   
   # Add method factor loadings from Method U model (freely estimated)
   Liking =~ LMX_1 + LMX_8 + LMX_12 + 
             LMX_3 + LMX_6 + LMX_10 + 
             LMX_4 + LMX_7 + LMX_11 +
             LMX_2 + LMX_5 + LMX_9
   
   # Fix substantive factor correlations to baseline values
   ProfessionalRespect ~~ 0.843*Affect + 
                         0.813*Contribution + 
                         0.856*Loyalty
   
   Affect ~~ 0.809*Contribution + 
            0.879*Loyalty
   
   Contribution ~~ 0.827*Loyalty
   
   # Fix correlations between Liking and substantive variables to zero
   ProfessionalRespect ~~ 0*Liking
   Affect ~~ 0*Liking
   Contribution ~~ 0*Liking
   Loyalty ~~ 0*Liking
   
   # Standardize all latent variables
   ProfessionalRespect ~~ 1*ProfessionalRespect
   Affect ~~ 1*Affect
   Contribution ~~ 1*Contribution
   Loyalty ~~ 1*Loyalty
   Liking ~~ 1*Liking
'

# Fit Method-R model
fit_methodR <- cfa(methodR_model,data = data,estimator = "MLR")
summary(fit_methodR)
# Compare Method-R with Method U model
anova(fit_methodU, fit_methodR)

# compare correlations from baseline model and retained model (methodU)
baseline_correlations <- lavInspect(fit_baseline, "cor.lv") 
MethodU_correlations <- lavInspect(fit_methodU, "cor.lv") 

# ----------------------------------------------------
# Phase 2
# ----------------------------------------------------

# Phase 2a: Get standardized method factor loadings from Method U model
std_solution <- standardizedSolution(fit_methodU)

# Filter to get just the method factor loadings (Liking -> LMX items)
method_loadings <- std_solution %>%
  filter(op == "=~" & 
           lhs == "Liking" &
           !rhs %in% c("Liking_1", "Liking_2", "Liking_3", "Liking_4"))

# Square the standardized estimates to get variance explained
method_loadings$variance_explained <- method_loadings$est.std^2

# ----------------------------------------------------
# Structural Models
# ----------------------------------------------------
# Create parcels for LMX dimensions
data <- data %>%
  mutate(
    LMX_parcel1 = rowMeans(select(., LMX_1, LMX_8, LMX_12)), # Professional Respect
    LMX_parcel2 = rowMeans(select(., LMX_3, LMX_6, LMX_10)), # Affect
    LMX_parcel3 = rowMeans(select(., LMX_4, LMX_7, LMX_11)), # Contribution
    LMX_parcel4 = rowMeans(select(., LMX_2, LMX_5, LMX_9))   # Loyalty
  )
# ----------------------------------------------------
# Structural Baseline Model
# ----------------------------------------------------

# Define the structural baseline model
baseline_model <- '
  # Measurement model
  LMX =~ 1*LMX_parcel1 + LMX_parcel2 + LMX_parcel3 + LMX_parcel4
  Trust =~ 1*Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10 + Trust_11
  OCBI =~ 1*OCBI_1 + OCBI_2 + OCBI_3 + OCBI_4 + OCBI_5 + OCBI_6 + OCBI_7 + OCBI_8
  OCBO =~ 1*OCBO_1 + OCBO_2 + OCBO_3 + OCBO_4 + OCBO_5 + OCBO_6 + OCBO_7 + OCBO_8
  OCA =~ 1*OCA_1 + OCA_2 + OCA_3 + OCA_4 + OCA_5 + OCA_6 + OCA_7
  Liking =~ 1*Liking_1 + Liking_2 + Liking_3 + Liking_4
  
  # Structural model
  Trust ~ b1*LMX
  OCBI ~ b2*LMX
  OCBO ~ b3*LMX
  OCA ~ b4*LMX
  
  # Method factor (uncorrelated)
  Liking ~~ 0*LMX
  Liking ~~ 0*Trust
  Liking ~~ 0*OCBI
  Liking ~~ 0*OCBO
  Liking ~~ 0*OCA
'

# Fit the baseline model
baseline_fit <- sem(baseline_model, data = data_parcels, estimator = "MLR")
# Summary of the baseline model
summary(baseline_fit, standardized = TRUE, fit.measures = TRUE)

# ----------------------------------------------------
# Structural Method Model
# ----------------------------------------------------

# Define the method model
method_model <- '
  # Measurement model
  LMX =~ LMX_parcel1 + LMX_parcel2 + LMX_parcel3 + LMX_parcel4
  Trust =~ Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10 + Trust_11
  OCBI =~ OCBI_1 + OCBI_2 + OCBI_3 + OCBI_4 + OCBI_5 + OCBI_6 + OCBI_7 + OCBI_8
  OCBO =~ OCBO_1 + OCBO_2 + OCBO_3 + OCBO_4 + OCBO_5 + OCBO_6 + OCBO_7 + OCBO_8
  OCA =~ OCA_1 + OCA_2 + OCA_3 + OCA_4 + OCA_5 + OCA_6 + OCA_7
  Liking =~ Liking_1 + Liking_2 + Liking_3 + Liking_4

  # Structural paths (freely estimated)
  Trust ~ LMX
  OCBI ~ LMX
  OCBO ~ LMX
  OCA ~ LMX
  
  # Method effects
  LMX_parcel1 + LMX_parcel2 + LMX_parcel3 + LMX_parcel4 ~ m1*Liking
  Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10 + Trust_11 ~ m2*Liking
  OCBI_1 + OCBI_2 + OCBI_3 + OCBI_4 + OCBI_5 + OCBI_6 + OCBI_7 + OCBI_8 ~ m3*Liking
  OCBO_1 + OCBO_2 + OCBO_3 + OCBO_4 + OCBO_5 + OCBO_6 + OCBO_7 + OCBO_8 ~ m4*Liking
  OCA_1 + OCA_2 + OCA_3 + OCA_4 + OCA_5 + OCA_6 + OCA_7 ~ m5*Liking
'

# Fit the method model
method_fit <- sem(method_model, data = data_parcels, estimator = "MLR")
# Summary of the method model
summary(method_fit, standardized = TRUE, fit.measures = TRUE)

# ----------------------------------------------------
# Constrained Model
# ----------------------------------------------------

# Define the constrained model
constrained_model <- '
  # Measurement model
  LMX =~ 1*LMX_parcel1 + LMX_parcel2 + LMX_parcel3 + LMX_parcel4
  Trust =~ 1*Trust_1 + Trust_2 + Trust_3 + Trust_4 + Trust_5 + Trust_6 + Trust_7 + Trust_8 + Trust_9 + Trust_10 + Trust_11
  OCBI =~ 1*OCBI_1 + OCBI_2 + OCBI_3 + OCBI_4 + OCBI_5 + OCBI_6 + OCBI_7 + OCBI_8
  OCBO =~ 1*OCBO_1 + OCBO_2 + OCBO_3 + OCBO_4 + OCBO_5 + OCBO_6 + OCBO_7 + OCBO_8
  OCA =~ 1*OCA_1 + OCA_2 + OCA_3 + OCA_4 + OCA_5 + OCA_6 + OCA_7
  Liking =~ 1*Liking_1 + Liking_2 + Liking_3 + Liking_4

  # Constrain structural paths to match baseline estimates
  Trust ~ c(b1)*LMX
  OCBI ~ c(b2)*LMX
  OCBO ~ c(b3)*LMX
  OCA ~ c(b4)*LMX
'

# Fit the constrained model
constrained_fit <- sem(constrained_model, data = data_parcels, estimator = "MLR")
# Summary of the constrained model
summary(constrained_fit, standardized = TRUE, fit.measures = TRUE)

# Chi-square difference test: Baseline vs. Method
chi_sq_diff <- lavTestLRT(baseline_fit, method_fit)
print(chi_sq_diff)

# Chi-square difference test: Method vs. Constrained
chi_sq_diff_method_constrained <- lavTestLRT(method_fit, constrained_fit)
print(chi_sq_diff_method_constrained)

###Bootstrapping
options(max.print = 10000)
baseline_fit_boot <- cfa(baseline_model, data = data_parcels, estimator = "ML", se = "bootstrap", bootstrap = 1000)
param_estimates <- parameterEstimates(baseline_fit_boot, boot.ci.type = "perc")
print(param_estimates)

options(max.print = 10000)
method_fit_boot <- cfa(method_model, data = data_parcels, estimator = "ML", se = "bootstrap", bootstrap = 1000)
param_estimates <- parameterEstimates(method_fit_boot, boot.ci.type = "perc")
print(param_estimates)

