* Load the data
use "/Users/bruce/Desktop/GHRC Yan Lab/Hearing impairment & PF 2023/outputs/pooled_11_15.dta", clear

* Encode the categorical variable
encode wave, gen(newWave)

* Generate centered age variable
gen centered_age = age - 60

* Declare the panel data structure
xtset id visit

* Estimate the average difference - model 1
xtgee SPPB i.hr age i.sex i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

* Estimate the average difference - model 2
xtgee SPPB i.hr age i.sex i.residence i.education i.marital i.hhconsumpLevel i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

* Estimate the average difference - model 3
xtgee SPPB i.hr age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

* Estimate the average difference - model 4
xtgee SPPB i.hr age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

// xttobit SPPB i.hr age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, ul(12)


predict fitted, mu
gen res = SPPB - fitted
histogram res, normal
scatter res fitted
qnorm res


* Step 1: Identify participants with at least 2 interviews
bysort id: egen interview_count = count(visit)

* Step 2: Filter the dataset to keep only participants with at least 2 interviews
keep if interview_count >= 2

* Step 3: Fit the model

xtgee SPPB i.hr##c.visit age i.sex i.residence i.education i.marital i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

xtgee SPPB i.hr##c.visit age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

xttobit SPPB i.hr##c.visit age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, ul(12)

// metobit SPPB i.hr##c.visit age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, || id: visit, covariance(independent) ul(12) vce(robust)


lincom visit + 1.hr#c.visit

* Predict the mean values
margins hr, at(visit = (0(1)2))

* Plot the predicted values
marginsplot, xdimension(visit) ytitle("Predicted mean SPPB score") xtitle("Study visits")

xtgee SPPB i.hr##c.centered_age i.sex i.residence i.education i.marital i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.4f) pformat(%5.3f) sformat(%8.0g)

xtgee SPPB i.hr##c.centered_age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, family(gaussian) link(identity) corr(unstructured) vce(robust) cformat(%9.4f) pformat(%5.3f) sformat(%8.0g)

xttobit SPPB i.hr##c.centered_age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, ul(12)

metobit SPPB i.hr##c.centered_age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, || id: centered_age, covariance(independent) ul(12) vce(robust)

lincom c.centered_age + 1.hr#c.centered_age

* Predict the mean values
margins hr, at(centered_age = (0(1)45))

* Plot the predicted values
marginsplot, xdimension(centered_age) ytitle("Predicted mean SPPB score") xtitle("Years after 60") noci

* Fit a linear mixed-effects model
meglm SPPB i.hr##c.visit age i.sex i.residence i.education i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke i.arthre i.dyslipe i.livere i.kidneye i.asthmae i.newWave, || id:c.visit, covariance(unstructured) family(gaussian) link(identity) vce(robust) eform

lincom visit + 1.hr#c.visit
