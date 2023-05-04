Structural Estimation Replication Code Files
============================================


Code to carry out the structural estimation of the consideration set model is written
in Fortran.  Files are organized into the folders described below.  In each case, some 
code has been given annotations to make it easier to follow.  The remaining code is
similar with different modifications, but has not been extensively annotated.  The
folder Input_Files contains data files that can be used to run the estimation.  A list
of the census tract attributes that are contained in the files tracts_XXX.txt is 
contained in the file tract_variable_list.csv.

Code is estimated using Mac OSX Pro Fortran 2022 SUSS License + 32/64-bit IMSL Bundle.



Estimation_First_Stage:  Code to bootstrap estimates of the probability that a 
renter from each race group will receive a positive response to an inquiry to view
an apartment in each census tract in each of our 5 cities.

   probit_bootstrap_annotated.f -- main first-stage code (w/annotations)

   probit_bootstrap_nohouston.f -- first-stage code for specification without Houston
   
   
   
   

Estimation_Second_Stage*:  Code to carry out bootstrap estimation of residential choice
model with and without consideration sets.  Uses bootstrap estimation results from first-
stage probit estimations.

   main_estimation_bootstrap_annotated.f -- main second-stage code (w/annotations)

   main_estimation_bootstrap_nohouston.f -- second-stage code for specification without
                                            Houston

   race_heterogeneity_estimation.f -- second-stage code allowing for heterogeneity
                                      in preferences by race (parameters vary for renters
                                      of color).  Follow instructions in code to turn off
                                      consideration sets and estimate standard model
                                      without choice constraints.
                                      
*Note that these results require the use of InfoUSA data.
The InfoUSA data for this project are subject to a Data Use Agreement with Data Axle (https://www.data-axle.com/). 
Researchers interested in access to the data should contact them for further information about data purchase. 
General information about the data can be found here (https://dupri.duke.edu/infousa-data).                                     
                                      
                                      
Simulated Search Model:  Code in this folder takes estimates from our main model with
consideration sets and uses it to simulate search by white, Black and Hispanic renters.
These results are used to determine the amount of search that a Black or Hispanic
renter would need to undertake in order to achieve the same level of utility as a 
similar white renter.

   searchsim_atl_annotate.f -- simulates search behavior for Atlanta and saves results
                               to be aggregated with other cities

   searchsim_hou.f -- simulates search behavior in Houston

   searchsim_phl.f -- simulates search behavior in Philadelphia

   searchsim_cle.f -- simulates search behavior in Cleveland

   searchsim_sjc.f -- simulates search behavior in San Jose

   search_agg_q_annotated -- combines results from cities into a single set of results 
                             by considering search over the percentage of each market.
                             For example, a Black renter may need to search 10% more of
                             the market to achieve the same utility as a white renter. 
                             In Houston this may be 30 more census tracts whereas it may
                             only be 13 more census tracts in Atlanta.
                             


WelfareSimulations:  Code in this folder uses the results of our main estimation to 
measure the welfare consequences of the choice set constraints resulting from 
discrimination.

   welfare_simulation_black_ATL_annotated.f -- calculates the reduction in income without
                                               choice set constraints that yields the same
                                               utility as having choice set constraints.
                                               This code does this for Black renter 
                                               identities in Atlanta.
                                               
   welfare_simulation_black_HOU.f -- similar code for Black renter identities in Houston

   welfare_simulation_black_PHL.f -- similar code for Black renter identities in 
   									 Philadelphia

   welfare_simulation_black_CLE.f -- similar code for Black renter identities in Cleveland

   welfare_simulation_black_SJC.f -- similar code for Black renter identities in San Jose

   welfare_simulation_black_HOU.f -- similar code for Black renter identities in Houston

   welfare_simulation_hisp_ATL.f -- similar code for Hispanic renter identities in Atlanta

   welfare_simulation_hisp_HOU.f -- similar code for Hispanic renter identities in Houston

   welfare_simulation_hisp_PHL.f -- similar code for Hispanic renter identities in 
   									Philadelphia

   welfare_simulation_hisp_CLE.f -- similar code for Hispanic renter identities in 
   								    Cleveland

   welfare_simulation_hisp_SJC.f -- similar code for Black renter identities in San Jose

   welfare_simulation_hisp_HOU.f -- similar code for Black renter identities in Houston