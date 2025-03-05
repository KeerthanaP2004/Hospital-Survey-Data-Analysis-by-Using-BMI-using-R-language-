# Hospital-Survey-Data-Analysis-by-Using-BMI-using-R-language-
# Load the NHANES and dplyr packages
library(NHANES)
library(dplyr)
install.packages("broom") 


# Load the NHANESraw data
data("NHANESraw")

# Take a glimpse at the contents
glimpse(NHANESraw)

# Load the ggplot2 package
library(ggplot2)

# Use mutate to create a 4-year weight variable and call it WTMEC4YR
NHANESraw <- NHANESraw %>%
  mutate(WTMEC4YR = WTMEC2YR / 2) 


# Calculate the sum of this weight variable
NHANESraw %>% summarize(total_WTMEC4YR = sum(WTMEC4YR))

# Plot the sample weights using boxplots, with Race1 on the x-axis
ggplot(NHANESraw, aes(x=Race1, y=WTMEC4YR))+
  geom_boxplot()


# Load the survey package
install.packages("survey")
library(survey)

# Now you can proceed with your analysis using the survey design object

# Assuming NHANESraw is your dataset
nhanes_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,)

require(survey)

install.packages("survey", lib="path/to/library")


# Specify the survey design
'survey::svydesign()'
install.packages("svydesign")


nhanes_design <- svydesign(
  data = NHANESraw,
  strata = ~SDMVSTRA,
  id = ~SDMVPSU,
  nest = TRUE,
  weights = ~WTMEC4YR)

# Print a summary of this design 
install.packages("nhanesdesign")
summary(nhanes_design)

# Select adults of Age >= 20 with subset
nhanes_adult <- nhanes_design%>%
  subset(Age >=20)

# Print a summary of this subset
summary(nhanes_adult)

# Compare the number of observations in the full data to the adult data
nrow(nhanes_adult)
nrow(nhanes_design)

# Calculate the mean BMI in NHANESraw
bmi_mean_raw <- NHANESraw %>% 
  filter(Age >= 20) %>%
  summarize(avg.BMI = mean(BMI, na.rm=TRUE))
bmi_mean_raw

# Calculate the survey-weighted mean BMI of US adults
bmi_mean <- svymean(~BMI, design = nhanes_adult, na.rm = TRUE)
bmi_mean

# Draw a weighted histogram of BMI in the US population
NHANESraw %>% 
  filter(Age >= 20) %>%
  ggplot(mapping=aes(x=BMI, weight=WTMEC4YR)) + 
  geom_histogram()+
  geom_vline(xintercept = coef(bmi_mean), color="red")

# Load the broom library
library(broom)

# Make a boxplot of BMI stratified by physically active status
NHANESraw %>% 
  filter(Age>=20) %>%
  ggplot(mapping=aes(x=PhysActive, y= BMI, weight=WTMEC4YR))+
  geom_boxplot()
install.packages("quantreg")
# Conduct a t-test comparing mean BMI between physically active status
survey_ttest <- svyttest(BMI~PhysActive, design = nhanes_adult)

# Use broom to show the tidy results
tidy(survey_ttest)

# Estimate the proportion who are physically active by current smoking status
phys_by_smoke <- svyby(~PhysActive, by = ~SmokeNow, 
                       FUN = svymean, 
                       design = nhanes_adult, 
                       keep.names = FALSE)

# Print the table
phys_by_smoke

# Plot the proportions
ggplot(data = phys_by_smoke, aes(SmokeNow, PhysActiveYes, fill = SmokeNow)) +
  geom_col()+
  ylab("Proportion Physically Active")

# Estimate mean BMI by current smoking status
BMI_by_smoke <- svyby(~BMI, by = ~SmokeNow, 
                      FUN = svymean, 
                      design = nhanes_adult, 
                      na.rm = TRUE)
BMI_by_smoke

# Plot the distribution of BMI by current smoking status
NHANESraw %>% 
  filter(Age>=20, !is.na(SmokeNow)) %>% 
  ggplot(mapping=aes(x=SmokeNow, y= BMI, weight=WTMEC4YR))+
  geom_boxplot()

# Plot the distribution of BMI by smoking and physical activity status
NHANESraw %>% 
  filter(Age>=20) %>%
  ggplot(mapping=aes(x=SmokeNow, y= BMI, weight=WTMEC4YR, color=PhysActive))+
  geom_boxplot()


# Fit a multiple regression model
mod1 <- svyglm(BMI ~ SmokeNow * PhysActive, design = nhanes_adult)

# Tidy the model results
tidy_mod1 <- tidy(mod1)
tidy_mod1

# Calculate expected mean difference in BMI for activity within non-smokers
diff_non_smoke <- tidy_mod1 %>% 
  filter(term == "PhysActiveYes") %>% 
  select(estimate)
diff_non_smoke

# Calculate expected mean difference in BMI for activity within smokers
diff_smoke <- tidy_mod1 %>% 
  filter(term %in% c('PhysActiveYes','SmokeNowYes:PhysActiveYes')) %>% 
  summarize(estimate = sum(estimate))
diff_smoke

# Adjust mod1 for other possible confounders
mod2 <- svyglm(BMI ~ PhysActive*SmokeNow + Race1 + Alcohol12PlusYr + Gender, 
               design = nhanes_adult)

# Tidy the output
tidy(mod2)

Output:
install.packages("broom")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
also installing the dependencies ‘stringi’, ‘cpp11’, ‘backports’, ‘ellipsis’, ‘purrr’, ‘stringr’, ‘tidyr’

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/stringi_1.8.3.zip'
Content type 'application/zip' length 14998651 bytes (14.3 MB)
downloaded 14.3 MB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/cpp11_0.4.7.zip'
Content type 'application/zip' length 304002 bytes (296 KB)
downloaded 296 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/backports_1.4.1.zip'
Content type 'application/zip' length 101330 bytes (98 KB)
downloaded 98 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/ellipsis_0.3.2.zip'
Content type 'application/zip' length 40537 bytes (39 KB)
downloaded 39 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/purrr_1.0.2.zip'
Content type 'application/zip' length 499048 bytes (487 KB)
downloaded 487 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/stringr_1.5.1.zip'
Content type 'application/zip' length 318869 bytes (311 KB)
downloaded 311 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/tidyr_1.3.1.zip'
Content type 'application/zip' length 1267415 bytes (1.2 MB)
downloaded 1.2 MB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/broom_1.0.5.zip'
Content type 'application/zip' length 1863516 bytes (1.8 MB)
downloaded 1.8 MB

package ‘stringi’ successfully unpacked and MD5 sums checked
package ‘cpp11’ successfully unpacked and MD5 sums checked
package ‘backports’ successfully unpacked and MD5 sums checked
package ‘ellipsis’ successfully unpacked and MD5 sums checked
package ‘purrr’ successfully unpacked and MD5 sums checked
package ‘stringr’ successfully unpacked and MD5 sums checked
package ‘tidyr’ successfully unpacked and MD5 sums checked
package ‘broom’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> # Load the NHANES and dplyr packages
> library(NHANES)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> install.packages("broom")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/broom_1.0.5.zip'
Content type 'application/zip' length 1863516 bytes (1.8 MB)
downloaded 1.8 MB

package ‘broom’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> # Load the NHANESraw data
> data("NHANESraw")
> # Take a glimpse at the contents
> glimpse(NHANESraw)
Rows: 20,293
Columns: 78
$ ID               <int> 51624, 51625, 51626, 51627, 51628, 51629, 51630, 51631, 51632, 51633…
$ SurveyYr         <fct> 2009_10, 2009_10, 2009_10, 2009_10, 2009_10, 2009_10, 2009_10, 2009_…
$ Gender           <fct> male, male, male, male, female, male, female, female, male, male, ma…
$ Age              <int> 34, 4, 16, 10, 60, 26, 49, 1, 10, 80, 10, 80, 4, 35, 9, 4, 17, 13, 7…
$ AgeMonths        <int> 409, 49, 202, 131, 722, 313, 596, 12, 124, NA, 121, NA, 48, 431, 115…
$ Race1            <fct> White, Other, Black, Black, Black, Mexican, White, White, Hispanic, …
$ Race3            <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ Education        <fct> High School, NA, NA, NA, High School, 9 - 11th Grade, Some College, …
$ MaritalStatus    <fct> Married, NA, NA, NA, Widowed, Married, LivePartner, NA, NA, Married,…
$ HHIncome         <fct> 25000-34999, 20000-24999, 45000-54999, 20000-24999, 10000-14999, 250…
$ HHIncomeMid      <int> 30000, 22500, 50000, 22500, 12500, 30000, 40000, 40000, 70000, 17500…
$ Poverty          <dbl> 1.36, 1.07, 2.27, 0.81, 0.69, 1.01, 1.91, 1.36, 2.68, 1.27, 0.93, 1.…
$ HomeRooms        <int> 6, 9, 5, 6, 6, 4, 5, 5, 7, 4, 5, 5, 7, NA, 6, 6, 5, 6, 4, 6, 9, 5, 7…
$ HomeOwn          <fct> Own, Own, Own, Rent, Rent, Rent, Rent, Rent, Own, Own, Own, Own, Own…
$ Work             <fct> NotWorking, NA, NotWorking, NA, NotWorking, Working, NotWorking, NA,…
$ Weight           <dbl> 87.4, 17.0, 72.3, 39.8, 116.8, 97.6, 86.7, 9.4, 26.0, 79.1, 44.7, 89…
$ Length           <dbl> NA, NA, NA, NA, NA, NA, NA, 75.7, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ HeadCirc         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ Height           <dbl> 164.7, 105.4, 181.3, 147.8, 166.0, 173.0, 168.4, NA, 140.3, 174.3, 1…
$ BMI              <dbl> 32.22, 15.30, 22.00, 18.22, 42.39, 32.61, 30.57, NA, 13.21, 26.04, 2…
$ BMICatUnder20yrs <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ BMI_WHO          <fct> 30.0_plus, 12.0_18.5, 18.5_to_24.9, 12.0_18.5, 30.0_plus, 30.0_plus,…
$ Pulse            <int> 70, NA, 68, 68, 72, 72, 86, NA, 70, 88, 84, 54, NA, NA, 82, NA, 68, …
$ BPSysAve         <int> 113, NA, 109, 93, 150, 104, 112, NA, 108, 139, 94, 121, NA, NA, 86, …
$ BPDiaAve         <int> 85, NA, 59, 41, 68, 49, 75, NA, 53, 43, 45, 60, NA, NA, 47, NA, 78, …
$ BPSys1           <int> 114, NA, 112, 92, 154, 102, 118, NA, 106, 142, 94, 126, NA, NA, 84, …
$ BPDia1           <int> 88, NA, 62, 36, 70, 50, 82, NA, 60, 62, 38, 62, NA, NA, 50, NA, 76, …
$ BPSys2           <int> 114, NA, 114, 94, 150, 104, 108, NA, 106, 140, 92, 124, NA, NA, 84, …
$ BPDia2           <int> 88, NA, 60, 44, 68, 48, 74, NA, 50, 46, 40, 62, NA, NA, 50, NA, 82, …
$ BPSys3           <int> 112, NA, 104, 92, 150, 104, 116, NA, 110, 138, 96, 118, NA, NA, 88, …
$ BPDia3           <int> 82, NA, 58, 38, 68, 50, 76, NA, 56, 40, 50, 58, NA, NA, 44, NA, 74, …
$ Testosterone     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ DirectChol       <dbl> 1.29, NA, 1.55, 1.89, 1.16, 1.16, 1.16, NA, 1.58, 1.94, 1.60, 1.27, …
$ TotChol          <dbl> 3.49, NA, 4.97, 4.16, 5.22, 4.14, 6.70, NA, 4.14, 4.71, 2.87, 3.83, …
$ UrineVol1        <int> 352, NA, 281, 139, 30, 202, 77, NA, 39, 128, 109, 38, NA, NA, 123, N…
$ UrineFlow1       <dbl> NA, NA, 0.415, 1.078, 0.476, 0.563, 0.094, NA, 0.300, 1.208, 0.956, …
$ UrineVol2        <int> NA, NA, NA, NA, 246, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ UrineFlow2       <dbl> NA, NA, NA, NA, 2.51, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ Diabetes         <fct> No, No, No, No, Yes, No, No, No, No, No, No, Yes, No, No, No, No, No…
$ DiabetesAge      <int> NA, NA, NA, NA, 56, NA, NA, NA, NA, NA, NA, 70, NA, NA, NA, NA, NA, …
$ HealthGen        <fct> Good, NA, Vgood, NA, Fair, Good, Good, NA, NA, Excellent, NA, Good, …
$ DaysPhysHlthBad  <int> 0, NA, 2, NA, 20, 2, 0, NA, NA, 0, NA, 0, NA, NA, NA, NA, 0, 0, NA, …
$ DaysMentHlthBad  <int> 15, NA, 0, NA, 25, 14, 10, NA, NA, 0, NA, 0, NA, NA, NA, NA, 18, 2, …
$ LittleInterest   <fct> Most, NA, NA, NA, Most, None, Several, NA, NA, None, NA, None, NA, N…
$ Depressed        <fct> Several, NA, NA, NA, Most, Most, Several, NA, NA, None, NA, None, NA…
$ nPregnancies     <int> NA, NA, NA, NA, 1, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ nBabies          <int> NA, NA, NA, NA, 1, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ Age1stBaby       <int> NA, NA, NA, NA, NA, NA, 27, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ SleepHrsNight    <int> 4, NA, 8, NA, 4, 4, 8, NA, NA, 6, NA, 9, NA, 7, NA, NA, 7, NA, NA, 1…
$ SleepTrouble     <fct> Yes, NA, No, NA, No, No, Yes, NA, NA, No, NA, No, NA, No, NA, NA, No…
$ PhysActive       <fct> No, NA, Yes, NA, No, Yes, No, NA, NA, Yes, NA, No, NA, No, NA, NA, Y…
$ PhysActiveDays   <int> NA, NA, 5, NA, NA, 2, NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, 6, 2, N…
$ TVHrsDay         <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ CompHrsDay       <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ TVHrsDayChild    <int> NA, 4, NA, 1, NA, NA, NA, NA, 1, NA, 3, NA, 2, NA, 5, 2, NA, NA, 2, …
$ CompHrsDayChild  <int> NA, 1, NA, 1, NA, NA, NA, NA, 0, NA, 0, NA, 1, NA, 0, 0, NA, NA, 6, …
$ Alcohol12PlusYr  <fct> Yes, NA, NA, NA, No, Yes, Yes, NA, NA, Yes, NA, No, NA, NA, NA, NA, …
$ AlcoholDay       <int> NA, NA, NA, NA, NA, 19, 2, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA…
$ AlcoholYear      <int> 0, NA, NA, NA, 0, 48, 20, NA, NA, 52, NA, 0, NA, NA, NA, NA, NA, NA,…
$ SmokeNow         <fct> No, NA, NA, NA, Yes, No, Yes, NA, NA, No, NA, No, NA, NA, NA, NA, NA…
$ Smoke100         <fct> Yes, NA, NA, NA, Yes, Yes, Yes, NA, NA, Yes, NA, Yes, NA, No, NA, NA…
$ SmokeAge         <int> 18, NA, NA, NA, 16, 15, 38, NA, NA, 16, NA, 21, NA, NA, NA, NA, NA, …
$ Marijuana        <fct> Yes, NA, NA, NA, NA, Yes, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ AgeFirstMarij    <int> 17, NA, NA, NA, NA, 10, 18, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ RegularMarij     <fct> No, NA, NA, NA, NA, Yes, No, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ AgeRegMarij      <int> NA, NA, NA, NA, NA, 12, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ HardDrugs        <fct> Yes, NA, NA, NA, No, Yes, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ SexEver          <fct> Yes, NA, NA, NA, Yes, Yes, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ SexAge           <int> 16, NA, NA, NA, 15, 9, 12, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ SexNumPartnLife  <int> 8, NA, NA, NA, 4, 10, 10, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ SexNumPartYear   <int> 1, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ SameSex          <fct> No, NA, NA, NA, No, No, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ SexOrientation   <fct> Heterosexual, NA, NA, NA, NA, Heterosexual, Heterosexual, NA, NA, NA…
$ WTINT2YR         <dbl> 80100.544, 53901.104, 13953.078, 11664.899, 20090.339, 22537.827, 74…
$ WTMEC2YR         <dbl> 81528.772, 56995.035, 14509.279, 12041.635, 21000.339, 22633.582, 74…
$ SDMVPSU          <int> 1, 2, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1,…
$ SDMVSTRA         <int> 83, 79, 84, 86, 75, 88, 85, 86, 88, 77, 86, 79, 84, 77, 88, 89, 81, …
$ PregnantNow      <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, Unknown, NA, NA,…
> # Load the ggplot2 package
> library(ggplot2)
> # Use mutate to create a 4-year weight variable and call it WTMEC4YR
> NHANESraw <- NHANESraw %>%
+   mutate(WTMEC4YR = WTMEC2YR / 2) 
> # Calculate the sum of this weight variable
> NHANESraw %>% summarize(total_WTMEC4YR = sum(WTMEC4YR))
# A tibble: 1 × 1
  total_WTMEC4YR
           <dbl>
1     304267200.
> # Plot the sample weights using boxplots, with Race1 on the x-axis
> ggplot(NHANESraw, aes(x=Race1, y=WTMEC4YR))+
+   geom_boxplot()
> # Load the survey package
> install.packages("survey")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/survey_4.2-1.zip'
Content type 'application/zip' length 3197772 bytes (3.0 MB)
downloaded 3.0 MB

package ‘survey’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> library(survey)
Loading required package: grid
Loading required package: Matrix
Loading required package: survival

Attaching package: ‘survey’

The following object is masked from ‘package:graphics’:

    dotchart

> # Assuming NHANESraw is your dataset
> nhanes_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,)
Error in svydesign.default(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,  : 
  Clusters not nested in strata at top level; you may want nest=TRUE.
> detach("package:graphics", unload = TRUE)
Warning message:
‘graphics’ namespace cannot be unloaded:
  namespace ‘graphics’ is imported by ‘survival’, ‘Matrix’, ‘splines’, ‘lattice’, ‘stats’, ‘scales’, ‘colorspace’, ‘labeling’, ‘survey’ so cannot be unloaded 
> library(graphics, lib.loc = "C:/Program Files/R/R-4.3.3/library")

Attaching package: ‘graphics’

The following object is masked from ‘package:survey’:

    dotchart

The following object is masked from ‘package:Matrix’:

    image

> # Create the survey design object
> survey_design <- svydesign.default(
+   data = NHANESraw,
+   strata = ~SDMVSTRA,
+   id = ~SDMVPSU,
+   nest = TRUE
+ )
Error in svydesign.default(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,  : 
  could not find function "svydesign.default"
> # Assuming NHANESraw is your dataset
> nhanes_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,)
Error in svydesign.default(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,  : 
  Clusters not nested in strata at top level; you may want nest=TRUE.
> install.packages("svydesign")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
Warning in install.packages :
  package ‘svydesign’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
> # Specify the survey design
> 'survey::svydesign()'
[1] "survey::svydesign()"
> install.packages("svydesign")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
Warning in install.packages :
  package ‘svydesign’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
> nhanes_design <- svydesign(
+   data = NHANESraw,
+   strata = ~SDMVSTRA,
+   id = ~SDMVPSU,
+   nest = TRUE,
+   weights = ~WTMEC4YR)
> # Print a summary of this design
> install.packages("nhanesdesign")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
Warning in install.packages :
  package ‘nhanesdesign’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
> summary(nhanes_design)
Stratified 1 - level Cluster Sampling design (with replacement)
With (62) clusters.
svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, 
    nest = TRUE, weights = ~WTMEC4YR)
Probabilities:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
8.986e-06 5.664e-05 1.054e-04       Inf 1.721e-04       Inf 
Stratum Sizes: 
            75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
obs        803 785 823 829 696 751 696 724 713 683 592 946 598 647 251 862 998 875 602 688 722
design.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
actual.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
            96  97  98  99 100 101 102 103
obs        676 608 708 682 700 715 624 296
design.PSU   2   2   2   2   2   2   2   2
actual.PSU   2   2   2   2   2   2   2   2
Data variables:
 [1] "ID"               "SurveyYr"         "Gender"           "Age"             
 [5] "AgeMonths"        "Race1"            "Race3"            "Education"       
 [9] "MaritalStatus"    "HHIncome"         "HHIncomeMid"      "Poverty"         
[13] "HomeRooms"        "HomeOwn"          "Work"             "Weight"          
[17] "Length"           "HeadCirc"         "Height"           "BMI"             
[21] "BMICatUnder20yrs" "BMI_WHO"          "Pulse"            "BPSysAve"        
[25] "BPDiaAve"         "BPSys1"           "BPDia1"           "BPSys2"          
[29] "BPDia2"           "BPSys3"           "BPDia3"           "Testosterone"    
[33] "DirectChol"       "TotChol"          "UrineVol1"        "UrineFlow1"      
[37] "UrineVol2"        "UrineFlow2"       "Diabetes"         "DiabetesAge"     
[41] "HealthGen"        "DaysPhysHlthBad"  "DaysMentHlthBad"  "LittleInterest"  
[45] "Depressed"        "nPregnancies"     "nBabies"          "Age1stBaby"      
[49] "SleepHrsNight"    "SleepTrouble"     "PhysActive"       "PhysActiveDays"  
[53] "TVHrsDay"         "CompHrsDay"       "TVHrsDayChild"    "CompHrsDayChild" 
[57] "Alcohol12PlusYr"  "AlcoholDay"       "AlcoholYear"      "SmokeNow"        
[61] "Smoke100"         "SmokeAge"         "Marijuana"        "AgeFirstMarij"   
[65] "RegularMarij"     "AgeRegMarij"      "HardDrugs"        "SexEver"         
[69] "SexAge"           "SexNumPartnLife"  "SexNumPartYear"   "SameSex"         
[73] "SexOrientation"   "WTINT2YR"         "WTMEC2YR"         "SDMVPSU"         
[77] "SDMVSTRA"         "PregnantNow"      "WTMEC4YR"        
> # Select adults of Age >= 20 with subset
> nhanes_adult <- nhanes_design%>%
+   subset(Age >=20)
> # Print a summary of this subset
> summary(nhanes_adult)
Stratified 1 - level Cluster Sampling design (with replacement)
With (62) clusters.
subset(., Age >= 20)
Probabilities:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
8.986e-06 4.303e-05 8.107e-05       Inf 1.240e-04       Inf 
Stratum Sizes: 
            75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
obs        471 490 526 500 410 464 447 400 411 395 357 512 327 355 153 509 560 483 376 368 454
design.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
actual.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
            96  97  98  99 100 101 102 103
obs        362 315 414 409 377 460 308 165
design.PSU   2   2   2   2   2   2   2   2
actual.PSU   2   2   2   2   2   2   2   2
Data variables:
 [1] "ID"               "SurveyYr"         "Gender"           "Age"             
 [5] "AgeMonths"        "Race1"            "Race3"            "Education"       
 [9] "MaritalStatus"    "HHIncome"         "HHIncomeMid"      "Poverty"         
[13] "HomeRooms"        "HomeOwn"          "Work"             "Weight"          
[17] "Length"           "HeadCirc"         "Height"           "BMI"             
[21] "BMICatUnder20yrs" "BMI_WHO"          "Pulse"            "BPSysAve"        
[25] "BPDiaAve"         "BPSys1"           "BPDia1"           "BPSys2"          
[29] "BPDia2"           "BPSys3"           "BPDia3"           "Testosterone"    
[33] "DirectChol"       "TotChol"          "UrineVol1"        "UrineFlow1"      
[37] "UrineVol2"        "UrineFlow2"       "Diabetes"         "DiabetesAge"     
[41] "HealthGen"        "DaysPhysHlthBad"  "DaysMentHlthBad"  "LittleInterest"  
[45] "Depressed"        "nPregnancies"     "nBabies"          "Age1stBaby"      
[49] "SleepHrsNight"    "SleepTrouble"     "PhysActive"       "PhysActiveDays"  
[53] "TVHrsDay"         "CompHrsDay"       "TVHrsDayChild"    "CompHrsDayChild" 
[57] "Alcohol12PlusYr"  "AlcoholDay"       "AlcoholYear"      "SmokeNow"        
[61] "Smoke100"         "SmokeAge"         "Marijuana"        "AgeFirstMarij"   
[65] "RegularMarij"     "AgeRegMarij"      "HardDrugs"        "SexEver"         
[69] "SexAge"           "SexNumPartnLife"  "SexNumPartYear"   "SameSex"         
[73] "SexOrientation"   "WTINT2YR"         "WTMEC2YR"         "SDMVPSU"         
[77] "SDMVSTRA"         "PregnantNow"      "WTMEC4YR"        
> # Compare the number of observations in the full data to the adult data
> nrow(nhanes_adult)
[1] 11778
> nrow(nhanes_design)
[1] 20293
> # Calculate the mean BMI in NHANESraw
> bmi_mean_raw <- NHANESraw %>% 
+   filter(Age >= 20) %>%
+   summarize(avg.BMI = mean(BMI, na.rm=TRUE))
> bmi_mean_raw
# A tibble: 1 × 1
  avg.BMI
    <dbl>
1    29.0
> # Calculate the survey-weighted mean BMI of US adults
> bmi_mean <- svymean(~BMI, design = nhanes_adult, na.rm = TRUE)
> bmi_mean
      mean     SE
BMI 28.734 0.1235
> # Draw a weighted histogram of BMI in the US population
> NHANESraw %>% 
+   filter(Age >= 20) %>%
+   ggplot(mapping=aes(x=BMI, weight=WTMEC4YR)) + 
+   geom_histogram()+
+   geom_vline(xintercept = coef(bmi_mean), color="red")
stat_bin() using bins = 30. Pick better value with binwidth.
Warning message:
Removed 547 rows containing non-finite outside the scale range (stat_bin()). 
> # Load the broom library
> library(broom)
> # Make a boxplot of BMI stratified by physically active status
> NHANESraw %>% 
+   filter(Age>=20) %>%
+   ggplot(mapping=aes(x=PhysActive, y= BMI, weight=WTMEC4YR))+
+   geom_boxplot()
Warning messages:
1: Removed 547 rows containing non-finite outside the scale range (stat_boxplot()). 
2: Computation failed in stat_boxplot().
Caused by error in loadNamespace():
! there is no package called ‘quantreg’ 
> install.packages("quantreg")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
also installing the dependencies ‘SparseM’, ‘MatrixModels’

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/SparseM_1.81.zip'
Content type 'application/zip' length 1042203 bytes (1017 KB)
downloaded 1017 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/MatrixModels_0.5-3.zip'
Content type 'application/zip' length 414526 bytes (404 KB)
downloaded 404 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/quantreg_5.97.zip'
Content type 'application/zip' length 1562180 bytes (1.5 MB)
downloaded 1.5 MB

package ‘SparseM’ successfully unpacked and MD5 sums checked
package ‘MatrixModels’ successfully unpacked and MD5 sums checked
package ‘quantreg’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> install.packages("quantreg")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/quantreg_5.97.zip'
Content type 'application/zip' length 1562180 bytes (1.5 MB)
downloaded 1.5 MB

package ‘quantreg’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> # Conduct a t-test comparing mean BMI between physically active status
> survey_ttest <- svyttest(BMI~PhysActive, design = nhanes_adult)
> # Use broom to show the tidy results
> tidy(survey_ttest)
# A tibble: 1 × 8
  estimate statistic  p.value parameter conf.low conf.high method              alternative
     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl> <chr>               <chr>      
1    -1.85     -9.72 4.56e-11        32    -2.23     -1.46 Design-based t-test two.sided  
> # Estimate the proportion who are physically active by current smoking status
> phys_by_smoke <- svyby(~PhysActive, by = ~SmokeNow, 
+                        FUN = svymean, 
+                        design = nhanes_adult, 
+                        keep.names = FALSE)
> # Print the table
> phys_by_smoke
  SmokeNow PhysActiveNo PhysActiveYes se.PhysActiveNo se.PhysActiveYes
1       No    0.4566990     0.5433010      0.01738054       0.01738054
2      Yes    0.5885421     0.4114579      0.01163246       0.01163246
> # Plot the proportions
> ggplot(data = phys_by_smoke, aes(SmokeNow, PhysActiveYes, fill = SmokeNow)) +
+   geom_col()+
+   ylab("Proportion Physically Active")
> # Estimate mean BMI by current smoking status
> BMI_by_smoke <- svyby(~BMI, by = ~SmokeNow, 
+                       FUN = svymean, 
+                       design = nhanes_adult, 
+                       na.rm = TRUE)
> BMI_by_smoke
    SmokeNow      BMI        se
No        No 29.25734 0.1915138
Yes      Yes 27.74873 0.1652377
> # Plot the distribution of BMI by current smoking status
> NHANESraw %>% 
+   filter(Age>=20, !is.na(SmokeNow)) %>% 
+   ggplot(mapping=aes(x=SmokeNow, y= BMI, weight=WTMEC4YR))+
+   geom_boxplot()
Warning message:
Removed 244 rows containing non-finite outside the scale range (stat_boxplot()). 
> # Plot the distribution of BMI by smoking and physical activity status
> NHANESraw %>% 
+   filter(Age>=20) %>%
+   ggplot(mapping=aes(x=SmokeNow, y= BMI, weight=WTMEC4YR, color=PhysActive))+
+   geom_boxplot()
Warning message:
Removed 547 rows containing non-finite outside the scale range (stat_boxplot()). 
> # Fit a multiple regression model
> mod1 <- svyglm(BMI ~ SmokeNow * PhysActive, design = nhanes_adult)
> # Tidy the model results
> tidy_mod1 <- tidy(mod1)
> tidy_mod1
# A tibble: 4 × 5
  term                      estimate std.error statistic  p.value
  <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)                  30.5      0.210    146.   2.62e-44
2 SmokeNowYes                  -2.24     0.267     -8.40 2.26e- 9
3 PhysActiveYes                -2.35     0.236     -9.97 4.96e-11
4 SmokeNowYes:PhysActiveYes     1.00     0.344      2.92 6.52e- 3
> # Calculate expected mean difference in BMI for activity within non-smokers
> diff_non_smoke <- tidy_mod1 %>% 
+   filter(term == "PhysActiveYes") %>% 
+   select(estimate)
> diff_non_smoke
# A tibble: 1 × 1
  estimate
     <dbl>
1    -2.35
> # Calculate expected mean difference in BMI for activity within smokers
> diff_smoke <- tidy_mod1 %>% 
+   filter(term %in% c('PhysActiveYes','SmokeNowYes:PhysActiveYes')) %>% 
+   summarize(estimate = sum(estimate))
> diff_smoke
# A tibble: 1 × 1
  estimate
     <dbl>
1    -1.35
> # Adjust mod1 for other possible confounders
> mod2 <- svyglm(BMI ~ PhysActive*SmokeNow + Race1 + Alcohol12PlusYr + Gender, 
+                design = nhanes_adult)
> # Tidy the output
> tidy(mod2)
# A tibble: 10 × 5
   term                      estimate std.error statistic  p.value
   <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
 1 (Intercept)                 33.2       0.316   105.    1.75e-33
 2 PhysActiveYes               -2.11      0.273    -7.75  5.56e- 8
 3 SmokeNowYes                 -2.23      0.303    -7.34  1.40e- 7
 4 Race1Hispanic               -1.47      0.420    -3.49  1.88e- 3
 5 Race1Mexican                -0.191     0.464    -0.412 6.84e- 1
 6 Race1White                  -2.08      0.320    -6.49  1.04e- 6
 7 Race1Other                  -3.11      0.620    -5.01  4.09e- 5
 8 Alcohol12PlusYrYes          -0.855     0.358    -2.39  2.50e- 2
 9 Gendermale                  -0.256     0.230    -1.11  2.78e- 1
10 PhysActiveYes:SmokeNowYes    0.737     0.387     1.90  6.92e- 2
> 
> install.packages("broom")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
also installing the dependencies ‘stringi’, ‘cpp11’, ‘backports’, ‘ellipsis’, ‘purrr’, ‘stringr’, ‘tidyr’

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/stringi_1.8.3.zip'
Content type 'application/zip' length 14998651 bytes (14.3 MB)
downloaded 14.3 MB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/cpp11_0.4.7.zip'
Content type 'application/zip' length 304002 bytes (296 KB)
downloaded 296 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/backports_1.4.1.zip'
Content type 'application/zip' length 101330 bytes (98 KB)
downloaded 98 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/ellipsis_0.3.2.zip'
Content type 'application/zip' length 40537 bytes (39 KB)
downloaded 39 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/purrr_1.0.2.zip'
Content type 'application/zip' length 499048 bytes (487 KB)
downloaded 487 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/stringr_1.5.1.zip'
Content type 'application/zip' length 318869 bytes (311 KB)
downloaded 311 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/tidyr_1.3.1.zip'
Content type 'application/zip' length 1267415 bytes (1.2 MB)
downloaded 1.2 MB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/broom_1.0.5.zip'
Content type 'application/zip' length 1863516 bytes (1.8 MB)
downloaded 1.8 MB

package ‘stringi’ successfully unpacked and MD5 sums checked
package ‘cpp11’ successfully unpacked and MD5 sums checked
package ‘backports’ successfully unpacked and MD5 sums checked
package ‘ellipsis’ successfully unpacked and MD5 sums checked
package ‘purrr’ successfully unpacked and MD5 sums checked
package ‘stringr’ successfully unpacked and MD5 sums checked
package ‘tidyr’ successfully unpacked and MD5 sums checked
package ‘broom’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> # Load the NHANES and dplyr packages
> library(NHANES)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> install.packages("broom")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/broom_1.0.5.zip'
Content type 'application/zip' length 1863516 bytes (1.8 MB)
downloaded 1.8 MB

package ‘broom’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> # Load the NHANESraw data
> data("NHANESraw")
> # Take a glimpse at the contents
> glimpse(NHANESraw)
Rows: 20,293
Columns: 78
$ ID               <int> 51624, 51625, 51626, 51627, 51628, 51629, 51630, 51631, 51632, 51633…
$ SurveyYr         <fct> 2009_10, 2009_10, 2009_10, 2009_10, 2009_10, 2009_10, 2009_10, 2009_…
$ Gender           <fct> male, male, male, male, female, male, female, female, male, male, ma…
$ Age              <int> 34, 4, 16, 10, 60, 26, 49, 1, 10, 80, 10, 80, 4, 35, 9, 4, 17, 13, 7…
$ AgeMonths        <int> 409, 49, 202, 131, 722, 313, 596, 12, 124, NA, 121, NA, 48, 431, 115…
$ Race1            <fct> White, Other, Black, Black, Black, Mexican, White, White, Hispanic, …
$ Race3            <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ Education        <fct> High School, NA, NA, NA, High School, 9 - 11th Grade, Some College, …
$ MaritalStatus    <fct> Married, NA, NA, NA, Widowed, Married, LivePartner, NA, NA, Married,…
$ HHIncome         <fct> 25000-34999, 20000-24999, 45000-54999, 20000-24999, 10000-14999, 250…
$ HHIncomeMid      <int> 30000, 22500, 50000, 22500, 12500, 30000, 40000, 40000, 70000, 17500…
$ Poverty          <dbl> 1.36, 1.07, 2.27, 0.81, 0.69, 1.01, 1.91, 1.36, 2.68, 1.27, 0.93, 1.…
$ HomeRooms        <int> 6, 9, 5, 6, 6, 4, 5, 5, 7, 4, 5, 5, 7, NA, 6, 6, 5, 6, 4, 6, 9, 5, 7…
$ HomeOwn          <fct> Own, Own, Own, Rent, Rent, Rent, Rent, Rent, Own, Own, Own, Own, Own…
$ Work             <fct> NotWorking, NA, NotWorking, NA, NotWorking, Working, NotWorking, NA,…
$ Weight           <dbl> 87.4, 17.0, 72.3, 39.8, 116.8, 97.6, 86.7, 9.4, 26.0, 79.1, 44.7, 89…
$ Length           <dbl> NA, NA, NA, NA, NA, NA, NA, 75.7, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ HeadCirc         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ Height           <dbl> 164.7, 105.4, 181.3, 147.8, 166.0, 173.0, 168.4, NA, 140.3, 174.3, 1…
$ BMI              <dbl> 32.22, 15.30, 22.00, 18.22, 42.39, 32.61, 30.57, NA, 13.21, 26.04, 2…
$ BMICatUnder20yrs <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ BMI_WHO          <fct> 30.0_plus, 12.0_18.5, 18.5_to_24.9, 12.0_18.5, 30.0_plus, 30.0_plus,…
$ Pulse            <int> 70, NA, 68, 68, 72, 72, 86, NA, 70, 88, 84, 54, NA, NA, 82, NA, 68, …
$ BPSysAve         <int> 113, NA, 109, 93, 150, 104, 112, NA, 108, 139, 94, 121, NA, NA, 86, …
$ BPDiaAve         <int> 85, NA, 59, 41, 68, 49, 75, NA, 53, 43, 45, 60, NA, NA, 47, NA, 78, …
$ BPSys1           <int> 114, NA, 112, 92, 154, 102, 118, NA, 106, 142, 94, 126, NA, NA, 84, …
$ BPDia1           <int> 88, NA, 62, 36, 70, 50, 82, NA, 60, 62, 38, 62, NA, NA, 50, NA, 76, …
$ BPSys2           <int> 114, NA, 114, 94, 150, 104, 108, NA, 106, 140, 92, 124, NA, NA, 84, …
$ BPDia2           <int> 88, NA, 60, 44, 68, 48, 74, NA, 50, 46, 40, 62, NA, NA, 50, NA, 82, …
$ BPSys3           <int> 112, NA, 104, 92, 150, 104, 116, NA, 110, 138, 96, 118, NA, NA, 88, …
$ BPDia3           <int> 82, NA, 58, 38, 68, 50, 76, NA, 56, 40, 50, 58, NA, NA, 44, NA, 74, …
$ Testosterone     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ DirectChol       <dbl> 1.29, NA, 1.55, 1.89, 1.16, 1.16, 1.16, NA, 1.58, 1.94, 1.60, 1.27, …
$ TotChol          <dbl> 3.49, NA, 4.97, 4.16, 5.22, 4.14, 6.70, NA, 4.14, 4.71, 2.87, 3.83, …
$ UrineVol1        <int> 352, NA, 281, 139, 30, 202, 77, NA, 39, 128, 109, 38, NA, NA, 123, N…
$ UrineFlow1       <dbl> NA, NA, 0.415, 1.078, 0.476, 0.563, 0.094, NA, 0.300, 1.208, 0.956, …
$ UrineVol2        <int> NA, NA, NA, NA, 246, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ UrineFlow2       <dbl> NA, NA, NA, NA, 2.51, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ Diabetes         <fct> No, No, No, No, Yes, No, No, No, No, No, No, Yes, No, No, No, No, No…
$ DiabetesAge      <int> NA, NA, NA, NA, 56, NA, NA, NA, NA, NA, NA, 70, NA, NA, NA, NA, NA, …
$ HealthGen        <fct> Good, NA, Vgood, NA, Fair, Good, Good, NA, NA, Excellent, NA, Good, …
$ DaysPhysHlthBad  <int> 0, NA, 2, NA, 20, 2, 0, NA, NA, 0, NA, 0, NA, NA, NA, NA, 0, 0, NA, …
$ DaysMentHlthBad  <int> 15, NA, 0, NA, 25, 14, 10, NA, NA, 0, NA, 0, NA, NA, NA, NA, 18, 2, …
$ LittleInterest   <fct> Most, NA, NA, NA, Most, None, Several, NA, NA, None, NA, None, NA, N…
$ Depressed        <fct> Several, NA, NA, NA, Most, Most, Several, NA, NA, None, NA, None, NA…
$ nPregnancies     <int> NA, NA, NA, NA, 1, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ nBabies          <int> NA, NA, NA, NA, 1, NA, 2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ Age1stBaby       <int> NA, NA, NA, NA, NA, NA, 27, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ SleepHrsNight    <int> 4, NA, 8, NA, 4, 4, 8, NA, NA, 6, NA, 9, NA, 7, NA, NA, 7, NA, NA, 1…
$ SleepTrouble     <fct> Yes, NA, No, NA, No, No, Yes, NA, NA, No, NA, No, NA, No, NA, NA, No…
$ PhysActive       <fct> No, NA, Yes, NA, No, Yes, No, NA, NA, Yes, NA, No, NA, No, NA, NA, Y…
$ PhysActiveDays   <int> NA, NA, 5, NA, NA, 2, NA, NA, NA, 4, NA, NA, NA, NA, NA, NA, 6, 2, N…
$ TVHrsDay         <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ CompHrsDay       <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ TVHrsDayChild    <int> NA, 4, NA, 1, NA, NA, NA, NA, 1, NA, 3, NA, 2, NA, 5, 2, NA, NA, 2, …
$ CompHrsDayChild  <int> NA, 1, NA, 1, NA, NA, NA, NA, 0, NA, 0, NA, 1, NA, 0, 0, NA, NA, 6, …
$ Alcohol12PlusYr  <fct> Yes, NA, NA, NA, No, Yes, Yes, NA, NA, Yes, NA, No, NA, NA, NA, NA, …
$ AlcoholDay       <int> NA, NA, NA, NA, NA, 19, 2, NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA…
$ AlcoholYear      <int> 0, NA, NA, NA, 0, 48, 20, NA, NA, 52, NA, 0, NA, NA, NA, NA, NA, NA,…
$ SmokeNow         <fct> No, NA, NA, NA, Yes, No, Yes, NA, NA, No, NA, No, NA, NA, NA, NA, NA…
$ Smoke100         <fct> Yes, NA, NA, NA, Yes, Yes, Yes, NA, NA, Yes, NA, Yes, NA, No, NA, NA…
$ SmokeAge         <int> 18, NA, NA, NA, 16, 15, 38, NA, NA, 16, NA, 21, NA, NA, NA, NA, NA, …
$ Marijuana        <fct> Yes, NA, NA, NA, NA, Yes, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ AgeFirstMarij    <int> 17, NA, NA, NA, NA, 10, 18, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ RegularMarij     <fct> No, NA, NA, NA, NA, Yes, No, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ AgeRegMarij      <int> NA, NA, NA, NA, NA, 12, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ HardDrugs        <fct> Yes, NA, NA, NA, No, Yes, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ SexEver          <fct> Yes, NA, NA, NA, Yes, Yes, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ SexAge           <int> 16, NA, NA, NA, 15, 9, 12, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
$ SexNumPartnLife  <int> 8, NA, NA, NA, 4, 10, 10, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
$ SexNumPartYear   <int> 1, NA, NA, NA, NA, 1, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ SameSex          <fct> No, NA, NA, NA, No, No, Yes, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
$ SexOrientation   <fct> Heterosexual, NA, NA, NA, NA, Heterosexual, Heterosexual, NA, NA, NA…
$ WTINT2YR         <dbl> 80100.544, 53901.104, 13953.078, 11664.899, 20090.339, 22537.827, 74…
$ WTMEC2YR         <dbl> 81528.772, 56995.035, 14509.279, 12041.635, 21000.339, 22633.582, 74…
$ SDMVPSU          <int> 1, 2, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1,…
$ SDMVSTRA         <int> 83, 79, 84, 86, 75, 88, 85, 86, 88, 77, 86, 79, 84, 77, 88, 89, 81, …
$ PregnantNow      <fct> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, Unknown, NA, NA,…
> # Load the ggplot2 package
> library(ggplot2)
> # Use mutate to create a 4-year weight variable and call it WTMEC4YR
> NHANESraw <- NHANESraw %>%
+   mutate(WTMEC4YR = WTMEC2YR / 2) 
> # Calculate the sum of this weight variable
> NHANESraw %>% summarize(total_WTMEC4YR = sum(WTMEC4YR))
# A tibble: 1 × 1
  total_WTMEC4YR
           <dbl>
1     304267200.
> # Plot the sample weights using boxplots, with Race1 on the x-axis
> ggplot(NHANESraw, aes(x=Race1, y=WTMEC4YR))+
+   geom_boxplot()
> # Load the survey package
> install.packages("survey")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/survey_4.2-1.zip'
Content type 'application/zip' length 3197772 bytes (3.0 MB)
downloaded 3.0 MB

package ‘survey’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> library(survey)
Loading required package: grid
Loading required package: Matrix
Loading required package: survival

Attaching package: ‘survey’

The following object is masked from ‘package:graphics’:

    dotchart

> # Assuming NHANESraw is your dataset
> nhanes_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,)
Error in svydesign.default(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,  : 
  Clusters not nested in strata at top level; you may want nest=TRUE.
> detach("package:graphics", unload = TRUE)
Warning message:
‘graphics’ namespace cannot be unloaded:
  namespace ‘graphics’ is imported by ‘survival’, ‘Matrix’, ‘splines’, ‘lattice’, ‘stats’, ‘scales’, ‘colorspace’, ‘labeling’, ‘survey’ so cannot be unloaded 
> library(graphics, lib.loc = "C:/Program Files/R/R-4.3.3/library")

Attaching package: ‘graphics’

The following object is masked from ‘package:survey’:

    dotchart

The following object is masked from ‘package:Matrix’:

    image

> # Create the survey design object
> survey_design <- svydesign.default(
+   data = NHANESraw,
+   strata = ~SDMVSTRA,
+   id = ~SDMVPSU,
+   nest = TRUE
+ )
Error in svydesign.default(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,  : 
  could not find function "svydesign.default"
> # Assuming NHANESraw is your dataset
> nhanes_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,)
Error in svydesign.default(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU,  : 
  Clusters not nested in strata at top level; you may want nest=TRUE.
> install.packages("svydesign")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
Warning in install.packages :
  package ‘svydesign’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
> # Specify the survey design
> 'survey::svydesign()'
[1] "survey::svydesign()"
> install.packages("svydesign")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
Warning in install.packages :
  package ‘svydesign’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
> nhanes_design <- svydesign(
+   data = NHANESraw,
+   strata = ~SDMVSTRA,
+   id = ~SDMVPSU,
+   nest = TRUE,
+   weights = ~WTMEC4YR)
> # Print a summary of this design
> install.packages("nhanesdesign")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
Warning in install.packages :
  package ‘nhanesdesign’ is not available for this version of R

A version of this package for your version of R might be available elsewhere,
see the ideas at
https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
> summary(nhanes_design)
Stratified 1 - level Cluster Sampling design (with replacement)
With (62) clusters.
svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, 
    nest = TRUE, weights = ~WTMEC4YR)
Probabilities:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
8.986e-06 5.664e-05 1.054e-04       Inf 1.721e-04       Inf 
Stratum Sizes: 
            75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
obs        803 785 823 829 696 751 696 724 713 683 592 946 598 647 251 862 998 875 602 688 722
design.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
actual.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
            96  97  98  99 100 101 102 103
obs        676 608 708 682 700 715 624 296
design.PSU   2   2   2   2   2   2   2   2
actual.PSU   2   2   2   2   2   2   2   2
Data variables:
 [1] "ID"               "SurveyYr"         "Gender"           "Age"             
 [5] "AgeMonths"        "Race1"            "Race3"            "Education"       
 [9] "MaritalStatus"    "HHIncome"         "HHIncomeMid"      "Poverty"         
[13] "HomeRooms"        "HomeOwn"          "Work"             "Weight"          
[17] "Length"           "HeadCirc"         "Height"           "BMI"             
[21] "BMICatUnder20yrs" "BMI_WHO"          "Pulse"            "BPSysAve"        
[25] "BPDiaAve"         "BPSys1"           "BPDia1"           "BPSys2"          
[29] "BPDia2"           "BPSys3"           "BPDia3"           "Testosterone"    
[33] "DirectChol"       "TotChol"          "UrineVol1"        "UrineFlow1"      
[37] "UrineVol2"        "UrineFlow2"       "Diabetes"         "DiabetesAge"     
[41] "HealthGen"        "DaysPhysHlthBad"  "DaysMentHlthBad"  "LittleInterest"  
[45] "Depressed"        "nPregnancies"     "nBabies"          "Age1stBaby"      
[49] "SleepHrsNight"    "SleepTrouble"     "PhysActive"       "PhysActiveDays"  
[53] "TVHrsDay"         "CompHrsDay"       "TVHrsDayChild"    "CompHrsDayChild" 
[57] "Alcohol12PlusYr"  "AlcoholDay"       "AlcoholYear"      "SmokeNow"        
[61] "Smoke100"         "SmokeAge"         "Marijuana"        "AgeFirstMarij"   
[65] "RegularMarij"     "AgeRegMarij"      "HardDrugs"        "SexEver"         
[69] "SexAge"           "SexNumPartnLife"  "SexNumPartYear"   "SameSex"         
[73] "SexOrientation"   "WTINT2YR"         "WTMEC2YR"         "SDMVPSU"         
[77] "SDMVSTRA"         "PregnantNow"      "WTMEC4YR"        
> # Select adults of Age >= 20 with subset
> nhanes_adult <- nhanes_design%>%
+   subset(Age >=20)
> # Print a summary of this subset
> summary(nhanes_adult)
Stratified 1 - level Cluster Sampling design (with replacement)
With (62) clusters.
subset(., Age >= 20)
Probabilities:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
8.986e-06 4.303e-05 8.107e-05       Inf 1.240e-04       Inf 
Stratum Sizes: 
            75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
obs        471 490 526 500 410 464 447 400 411 395 357 512 327 355 153 509 560 483 376 368 454
design.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
actual.PSU   2   2   2   2   2   2   2   2   2   2   2   3   2   2   2   3   3   3   2   2   2
            96  97  98  99 100 101 102 103
obs        362 315 414 409 377 460 308 165
design.PSU   2   2   2   2   2   2   2   2
actual.PSU   2   2   2   2   2   2   2   2
Data variables:
 [1] "ID"               "SurveyYr"         "Gender"           "Age"             
 [5] "AgeMonths"        "Race1"            "Race3"            "Education"       
 [9] "MaritalStatus"    "HHIncome"         "HHIncomeMid"      "Poverty"         
[13] "HomeRooms"        "HomeOwn"          "Work"             "Weight"          
[17] "Length"           "HeadCirc"         "Height"           "BMI"             
[21] "BMICatUnder20yrs" "BMI_WHO"          "Pulse"            "BPSysAve"        
[25] "BPDiaAve"         "BPSys1"           "BPDia1"           "BPSys2"          
[29] "BPDia2"           "BPSys3"           "BPDia3"           "Testosterone"    
[33] "DirectChol"       "TotChol"          "UrineVol1"        "UrineFlow1"      
[37] "UrineVol2"        "UrineFlow2"       "Diabetes"         "DiabetesAge"     
[41] "HealthGen"        "DaysPhysHlthBad"  "DaysMentHlthBad"  "LittleInterest"  
[45] "Depressed"        "nPregnancies"     "nBabies"          "Age1stBaby"      
[49] "SleepHrsNight"    "SleepTrouble"     "PhysActive"       "PhysActiveDays"  
[53] "TVHrsDay"         "CompHrsDay"       "TVHrsDayChild"    "CompHrsDayChild" 
[57] "Alcohol12PlusYr"  "AlcoholDay"       "AlcoholYear"      "SmokeNow"        
[61] "Smoke100"         "SmokeAge"         "Marijuana"        "AgeFirstMarij"   
[65] "RegularMarij"     "AgeRegMarij"      "HardDrugs"        "SexEver"         
[69] "SexAge"           "SexNumPartnLife"  "SexNumPartYear"   "SameSex"         
[73] "SexOrientation"   "WTINT2YR"         "WTMEC2YR"         "SDMVPSU"         
[77] "SDMVSTRA"         "PregnantNow"      "WTMEC4YR"        
> # Compare the number of observations in the full data to the adult data
> nrow(nhanes_adult)
[1] 11778
> nrow(nhanes_design)
[1] 20293
> # Calculate the mean BMI in NHANESraw
> bmi_mean_raw <- NHANESraw %>% 
+   filter(Age >= 20) %>%
+   summarize(avg.BMI = mean(BMI, na.rm=TRUE))
> bmi_mean_raw
# A tibble: 1 × 1
  avg.BMI
    <dbl>
1    29.0
> # Calculate the survey-weighted mean BMI of US adults
> bmi_mean <- svymean(~BMI, design = nhanes_adult, na.rm = TRUE)
> bmi_mean
      mean     SE
BMI 28.734 0.1235
> # Draw a weighted histogram of BMI in the US population
> NHANESraw %>% 
+   filter(Age >= 20) %>%
+   ggplot(mapping=aes(x=BMI, weight=WTMEC4YR)) + 
+   geom_histogram()+
+   geom_vline(xintercept = coef(bmi_mean), color="red")
stat_bin() using bins = 30. Pick better value with binwidth.
Warning message:
Removed 547 rows containing non-finite outside the scale range (stat_bin()). 
> # Load the broom library
> library(broom)
> # Make a boxplot of BMI stratified by physically active status
> NHANESraw %>% 
+   filter(Age>=20) %>%
+   ggplot(mapping=aes(x=PhysActive, y= BMI, weight=WTMEC4YR))+
+   geom_boxplot()
Warning messages:
1: Removed 547 rows containing non-finite outside the scale range (stat_boxplot()). 
2: Computation failed in stat_boxplot().
Caused by error in loadNamespace():
! there is no package called ‘quantreg’ 
> install.packages("quantreg")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
also installing the dependencies ‘SparseM’, ‘MatrixModels’

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/SparseM_1.81.zip'
Content type 'application/zip' length 1042203 bytes (1017 KB)
downloaded 1017 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/MatrixModels_0.5-3.zip'
Content type 'application/zip' length 414526 bytes (404 KB)
downloaded 404 KB

trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/quantreg_5.97.zip'
Content type 'application/zip' length 1562180 bytes (1.5 MB)
downloaded 1.5 MB

package ‘SparseM’ successfully unpacked and MD5 sums checked
package ‘MatrixModels’ successfully unpacked and MD5 sums checked
package ‘quantreg’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> install.packages("quantreg")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/Keerthana P/AppData/Local/R/win-library/4.3’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.3/quantreg_5.97.zip'
Content type 'application/zip' length 1562180 bytes (1.5 MB)
downloaded 1.5 MB

package ‘quantreg’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\Keerthana P\AppData\Local\Temp\RtmpqkJ1tx\downloaded_packages
> # Conduct a t-test comparing mean BMI between physically active status
> survey_ttest <- svyttest(BMI~PhysActive, design = nhanes_adult)
> # Use broom to show the tidy results
> tidy(survey_ttest)
# A tibble: 1 × 8
  estimate statistic  p.value parameter conf.low conf.high method              alternative
     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl> <chr>               <chr>      
1    -1.85     -9.72 4.56e-11        32    -2.23     -1.46 Design-based t-test two.sided  
> # Estimate the proportion who are physically active by current smoking status
> phys_by_smoke <- svyby(~PhysActive, by = ~SmokeNow, 
+                        FUN = svymean, 
+                        design = nhanes_adult, 
+                        keep.names = FALSE)
> # Print the table
> phys_by_smoke
  SmokeNow PhysActiveNo PhysActiveYes se.PhysActiveNo se.PhysActiveYes
1       No    0.4566990     0.5433010      0.01738054       0.01738054
2      Yes    0.5885421     0.4114579      0.01163246       0.01163246
> # Plot the proportions
> ggplot(data = phys_by_smoke, aes(SmokeNow, PhysActiveYes, fill = SmokeNow)) +
+   geom_col()+
+   ylab("Proportion Physically Active")
> # Estimate mean BMI by current smoking status
> BMI_by_smoke <- svyby(~BMI, by = ~SmokeNow, 
+                       FUN = svymean, 
+                       design = nhanes_adult, 
+                       na.rm = TRUE)
> BMI_by_smoke
    SmokeNow      BMI        se
No        No 29.25734 0.1915138
Yes      Yes 27.74873 0.1652377
> # Plot the distribution of BMI by current smoking status
> NHANESraw %>% 
+   filter(Age>=20, !is.na(SmokeNow)) %>% 
+   ggplot(mapping=aes(x=SmokeNow, y= BMI, weight=WTMEC4YR))+
+   geom_boxplot()
Warning message:
Removed 244 rows containing non-finite outside the scale range (stat_boxplot()). 
> # Plot the distribution of BMI by smoking and physical activity status
> NHANESraw %>% 
+   filter(Age>=20) %>%
+   ggplot(mapping=aes(x=SmokeNow, y= BMI, weight=WTMEC4YR, color=PhysActive))+
+   geom_boxplot()
Warning message:
Removed 547 rows containing non-finite outside the scale range (stat_boxplot()). 
> # Fit a multiple regression model
> mod1 <- svyglm(BMI ~ SmokeNow * PhysActive, design = nhanes_adult)
> # Tidy the model results
> tidy_mod1 <- tidy(mod1)
> tidy_mod1
# A tibble: 4 × 5
  term                      estimate std.error statistic  p.value
  <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
1 (Intercept)                  30.5      0.210    146.   2.62e-44
2 SmokeNowYes                  -2.24     0.267     -8.40 2.26e- 9
3 PhysActiveYes                -2.35     0.236     -9.97 4.96e-11
4 SmokeNowYes:PhysActiveYes     1.00     0.344      2.92 6.52e- 3
> # Calculate expected mean difference in BMI for activity within non-smokers
> diff_non_smoke <- tidy_mod1 %>% 
+   filter(term == "PhysActiveYes") %>% 
+   select(estimate)
> diff_non_smoke
# A tibble: 1 × 1
  estimate
     <dbl>
1    -2.35
> # Calculate expected mean difference in BMI for activity within smokers
> diff_smoke <- tidy_mod1 %>% 
+   filter(term %in% c('PhysActiveYes','SmokeNowYes:PhysActiveYes')) %>% 
+   summarize(estimate = sum(estimate))
> diff_smoke
# A tibble: 1 × 1
  estimate
     <dbl>
1    -1.35
> # Adjust mod1 for other possible confounders
> mod2 <- svyglm(BMI ~ PhysActive*SmokeNow + Race1 + Alcohol12PlusYr + Gender, 
+                design = nhanes_adult)
> # Tidy the output
> tidy(mod2)
# A tibble: 10 × 5
   term                      estimate std.error statistic  p.value
   <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
 1 (Intercept)                 33.2       0.316   105.    1.75e-33
 2 PhysActiveYes               -2.11      0.273    -7.75  5.56e- 8
 3 SmokeNowYes                 -2.23      0.303    -7.34  1.40e- 7
 4 Race1Hispanic               -1.47      0.420    -3.49  1.88e- 3
 5 Race1Mexican                -0.191     0.464    -0.412 6.84e- 1
 6 Race1White                  -2.08      0.320    -6.49  1.04e- 6
 7 Race1Other                  -3.11      0.620    -5.01  4.09e- 5
 8 Alcohol12PlusYrYes          -0.855     0.358    -2.39  2.50e- 2
 9 Gendermale                  -0.256     0.230    -1.11  2.78e- 1
10 PhysActiveYes:SmokeNowYes    0.737     0.387     1.90  6.92e- 2
> 
>
