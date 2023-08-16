'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  Macro-Econometric Forecasting by IMF
'  Final Assignment: Forecasting U.S. Savings Behavior After the Crisis
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  This program assumes that the working directory is the current folder where this program file is saved. 
'  Please ensure that the base workfile "M1_Data.wf1" is open before running this program.
'------------------------------------------
'  Step 1: Set Up
'------------------------------------------
'  Select the pagefile called "USA_CY", which contains all the household saving/consumption data 

'logmode p  'Useful for debugging code.  Provides a line-by-line check, showing the actual line in the program where the error occurred.

'Session 1: Inspect before you regress!

pageselect USA_CY

%start = "1961q1"  	  'First period included in the regression sample
%bf_crisis  = "2007q4" 'Last quarter before the onset of the sharp rise in the saving ratio
%fq_crisis = "2008q1"  'Beginning of the crisis (sudden rise in the saving ratio)
%last = "2014q3"		  'Last period in the regression sample

'----------------------------------------------------------------
'  Step 2: Consider the following long-run model (PIH)
'  ln_rc = c + b_1* ln_rdy + b_2* ln_rnw + epsilon
'----------------------------------------------------------------

'  Calculate the saving rate, and real net worth (deseasonalized)

smpl @all

series saving_rate = (saving/ndy)*100

smpl 1952:1 2014:3

' Deseasonalize net_worth (original data is not seasonally adjusted)

net_worth.x13(mode=m, errlog=net_worth_sa_log) 'sa variable is saved as net_worth_d11  
genr rnw = net_worth_d11/y_deflator  'Calculate real net worth
rnw.label(d) Real net worth

'Time plot of log(rc/rdy) against log(rnw/rdy)

delete(noerr) c_ratio_line*
group c_ratio_line log(rc/rdy) log(rnw/rdy)
freeze(mode=overwrite,c_ratio_lineplot) c_ratio_line.line(d)
c_ratio_lineplot.label(d) Time plot, log(rc/rdy) against log(rnw/rdy)
show c_ratio_lineplot

' Scatter plot of log(rc/rdy) against log(rnw/rdy)

freeze(mode=overwrite,c_ratio_scatterplot) c_ratio_line.scat
c_ratio_scatterplot.label(d) Scatter plot, log(rc/rdy) against log(rnw/rdy)
show c_ratio_scatterplot

'Scatter plot of dlog(rc/rdy) against dlog(rnw/rdy)

delete(noerr) d_c_ratio_line*
group d_c_ratio_line dlog(rc/rdy) dlog(rnw/rdy)
freeze(mode=overwrite,d_c_ratio_scatterplot) d_c_ratio_line.scat
d_c_ratio_scatterplot.label(d) Scatter plot, dlog(rc/rdy) against dlog(rnw/rdy)
show d_c_ratio_scatterplot

smpl {%start} {%last}

equation lr_c_ratio.ls log(rc/rdy) c log(rnw/rdy)
lr_c_ratio.label(d) Long-run regression using consumption-income ratio; real net worth-income ratio.
lr_c_ratio.resids  'Residuals are all negative till 1975:4, suggesting a structural break

smpl @all

genr sb_1975_4 = 0

smpl @first 1975:4 'Allow for a possible structural break around before1975:4 (visible by regressing the consumption-labor income ratio against net-worth/income ratio (see below); residuals are all negative before 1975:4)

genr sb_1975_4 = 1

sb_1975_4.label(d) Dummy variable to allow for a possible structural break before 1975:4

smpl {%start} {%last}

equation lr_c_ratio.ls log(rc/rdy) c log(rnw/rdy)  sb_1975_4
lr_c_ratio.label(d) Long-run regression using consumption-income ratio; real net worth-income ratio
lr_c_ratio.resids  'Residuals are all negative till 1975:4, suggesting a structural break

'  Generate log variables (for unit root testing purposes only; otherwise use auto variables)

genr ln_rc = log(rc)
ln_rc.label(d) Real consumption (log form)
genr ln_rdy = log(rdy) 'Real disposable income
ln_rdy.label(d) Real disposable income (log form)
genr ln_rnw = log(rnw)
ln_rnw.label(d) Real net worth (log form)

'------------------------------------------
'  Step 3: Specify sample period
'------------------------------------------

smpl {%start} {%bf_crisis}

'Session 2: Time series properties of the data.

' Conduct unit root tests

freeze(mode=overwrite,uroot_ln_rc) ln_rc.uroot(adf,const,maxlag=15,info=sic) 'Unit root test for real consumption
uroot_ln_rc.label(d) Unit root test for real consumption
freeze(mode=overwrite,uroot_ln_rdy) ln_rdy.uroot(adf,const,maxlag=15,info=sic) 'Unit root test for disposable real labor income
uroot_ln_rdy.label(d) Unit root test for real disposable income
freeze(mode=overwrite,uroot_ln_rnw) ln_rnw.uroot(adf,const,maxlag=15,info=sic) 'Unit root test for real net wealth
uroot_ln_rnw.label(d) Unit root test for real net worth

freeze(mode=overwrite,uroot_ln_rc_kpss) ln_rc.uroot(kpss,trend,maxlag=15,info=sic) 'KPSS test for real consumption
uroot_ln_rc_kpss.label(d) Stationarity test for real consumption (KPSS)
freeze(mode=overwrite,uroot_ln_rdy_kpss) ln_rdy.uroot(kpss,trend,maxlag=15,info=sic) 'KPSS test for disposable real labor income
uroot_ln_rdy_kpss.label(d) Stationarity test for real disposable income (KPSS)
freeze(mode=overwrite,uroot_ln_rnw_kpss) ln_rnw.uroot(kpss,trend,maxlag=15,info=sic) 'KPSS test for real net wealth
uroot_ln_rnw_kpss.label(d) Stationarity test for real net worth (KPSS)

'Step 1: estimate VAR with ad-hoc (2) number of lags

var rf.ls 1 2 log(rc) log(rdy) log(rnw) @ c sb_1975_4 
rf.label(d) Vector autoregression -- long-run consumption, allowing for pre 1976 structural break

' Step 2: Check lag structure (AIC, SIC, etc) -- in this case, most criteria suggest two lags.

freeze(mode=overwrite,rf_lag_length) rf.laglen(8)
rf_lag_length.label(d) Lag length criteria

show rf_lag_length

' Step 3: Johansen's cointegration test with 1 lag

freeze(mode=overwrite,rf_coint_test) rf.coint(c,1,cvsize=0.10)
rf_coint_test.label(d) Johansen cointegration test (10 % percent level of significance)

show rf_coint_test 'Display the results of the cointegration test (10 % level of significance)

'---------------------------------------------------------------------
'  Step 4: Modeling short-run relationship (VECM)
'---------------------------------------------------------------------
' In addition to the long-run relationship, consider 
' the following exogenous variables in the VECM
' 	change in the unemployment rate (proxy for uncertainty)
' 	log of consumer_sentiment (proxy for uncertainty)

' Before include we them, first conduct unit root tests on these variables.

smpl @all

smpl {%start} {%bf_crisis}

freeze(uroot_unemp,mode=overwrite) unemp.uroot(adf,const,maxlag=15,info=sic)
uroot_unemp.label(d) Unit root test for the unemployment rate
freeze(uroot_sentiment,mode=overwrite) consumer_sentiment.uroot(adf,const,maxlag=15,info=sic)
uroot_sentiment.label(d) Unit root test for consumer sentiment.

' Estimate VEC model, given optimal (in this case 1) lags in the VAR

var rf_vecm.ec 1 1 log(rc) log(rdy) log(rnw) @ sb_1975_4 d(unemp) log(consumer_sentiment)
rf_vecm.label(d) VECM results for base consumption function model.

'.......................................................................................
' Step 5: Test if RDY and RNW are (jointly) weakly exogenous
'.......................................................................................

rf_vecm.cleartext(coint) 'Clear any restrictions on the ECM before doing so
rf_vecm.append(coint) b(1,1)=1, a(3,1) = 0 'Test for weak exogeneity of income/wealth...
rf_vecm.ec(restrict) 1 1 log(rc) log(rdy) log(rnw) @ sb_1975_4 d(unemp)  log(consumer_sentiment)

show rf_vecm

'-----------------------------------------------------------------------------------------
'  Step 6: Forecasting the saving rate using the alternative VECM
'-----------------------------------------------------------------------------------------
 
rf_vecm.makemodel(vecm_baseline) 'Create a model from the ECM just estimated
vecm_baseline.label(d) Baseline model for real consumption

' Add the definition of saving rate
' Important to express the saving ratio in terms of variables predicted by the model (namely, using real quantities)

smpl @all

smpl {%start} {%bf_crisis}

'Redefine the saving_rate variable accounting for the way things were calculated.

genr saving_rate = 100*(rdy - (rc + ((gov_transfers + interest_payments)/y_deflator)))/rdy

vecm_baseline.append saving_rate = 100*(rdy - (rc + ((gov_transfers + interest_payments)/y_deflator)))/rdy

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Baseline scenario (output variables are xx_0; alternative are xx_1)
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
delete(noerr) *_0 *_1
'............................................
' Prepare dynamic solution 
'............................................
smpl {%fq_crisis} {%last}
vecm_baseline.solve(s=d,d=d) 'Solve the model using dynamic simulation (errors accumulate)
smpl {%fq_crisis}-8 {%last}
delete(noerr) g_dynamic*
vecm_baseline.makegraph(a) g_dynamic @endog
show g_dynamic
g_dynamic.label(d) Simulation results, dynamic, from baseline model.

' Solution type (s) is deterministic (d), i.e. s=d
' Model solution dynamics (d) is dynamic (d), i.e., d=d
' Copy the solution to saving_rate_dynamic

copy saving_rate_0 saving_rate_dynamic
delete(noerr) saving_rate_0

'Build a graph for the result

smpl 2004:1 {%last}
delete(noerr) dynamic_forecast
graph dynamic_forecast saving_rate saving_rate_dynamic
dynamic_forecast.addtext(t) Figure 1: Household Saving Rate Predicted by VECM model (Dynamic Forecast)
dynamic_forecast.name(1) Household Saving Rate (Actual)
dynamic_forecast.name(2) Household Saving Rate (Dynamic Forecast)
show dynamic_forecast
dynamic_forecast.label(d) Forecast of Saving Rate using "Dynamic"

'............................................
' Prepare a static solution
'............................................
smpl {%fq_crisis} {%last}
vecm_baseline.solve(s=d,d=s) 'Solution = deterministic (s=d), method = static (d=s)
smpl {%fq_crisis}-8 {%last}
delete(noerr) g_static*
vecm_baseline.makegraph(a) g_static @endog
show g_static
g_static.label(d) Simulation results, static, from baseline model.

copy saving_rate_0 saving_rate_static
delete(noerr) saving_rate_0

smpl 2004:1 {%last}

delete(noerr) static_forecast
graph static_forecast saving_rate saving_rate_static
static_forecast.addtext(t) Figure 2: Household Saving Rate Predicted by VECM model (Static Forecast)
static_forecast.name(1) Household Saving Rate (Actual)
static_forecast.name(2) Household Saving Rate (Static Forecast)
show static_forecast
static_forecast.label(d) Forecast of Saving Rate using "Static"

'............................................
' Prepare a fit solution - shows the predicted behaviour of the saving ratio using the fitted model.
'............................................
smpl {%fq_crisis} {%last}
vecm_baseline.solve(s=d,d=f) 'Solution = deterministic (s=d), method = fit (d=f)
smpl {%fq_crisis}-8 {%last}
delete(noerr) g_fit*
vecm_baseline.makegraph(a) g_fit @endog
show g_fit
g_fit.label(d) Simulation results, fit, from baseline model.

copy saving_rate_0 saving_rate_fit
saving_rate_fit.label(d) "Simulation -- Fit (single equation mode)"
delete(noerr) saving_rate_0
delete(noerr) fit_forecast
smpl 2004:1 {%last}
graph fit_forecast saving_rate saving_rate_fit
show saving_rate saving_rate_fit
fit_forecast.addtext(t) Figure 3: Household Saving Rate Predicted by VECM model (Fit Forecast)
fit_forecast.name(1) Household Saving Rate (Actual)
fit_forecast.name(2) Household Saving Rate (Fit Forecast)
show fit_forecast
fit_forecast.label(d) Forecast of Saving Rate using "Fit"

'Lastly, it looks like there has not been a structural change in the underlying long-run parameters. Let's check using a very simple test -- Chow Forecast Test (even though the variables are I(1) and cointegrated. Treat test as indicative only.)

smpl {%start} @last
equation lr_model.ls log(rc) c log(rdy) log(rnw) sb_1975_4
lr_model.label(d) Single equation long-run model for real consumption
lr_model.chow(f) 2008q1
show lr_model.chow(f) 2008q1 'Result suggests no structural change....

delete(noerr) *_0 *_1 *_a


