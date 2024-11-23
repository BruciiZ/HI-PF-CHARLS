* Load the data
use "/Users/bruce/Desktop/GHRC Yan Lab/Hearing impairment & PF 2023/outputs/pooled_11_15.dta", clear

* Encode the categorical variable
encode wave, gen(newWave)

* Generate centered age variable
gen centered_age = age - 60

gen reversed_standScore = 4 - standScore + 1

* Estimate the average difference - model 1 (Mixed-Effects Ordinal Logistic Regression)
meologit reversed_standScore i.hr centered_age i.sex i.newWave || id:, ///
    or cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

* Estimate the average difference - model 2 (Mixed-Effects Ordinal Logistic Regression)
meologit reversed_standScore i.hr centered_age i.sex i.residence i.education ///
    i.marital i.hhconsumpLevel i.newWave || id:, ///
    or cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

* Estimate the average difference - model 3 (Mixed-Effects Ordinal Logistic Regression)
meologit reversed_standScore i.hr centered_age i.sex i.residence i.education ///
    i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink ///
    i.newWave || id:, ///
    or cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)

* Estimate the average difference - model 4 (Mixed-Effects Ordinal Logistic Regression)
meologit reversed_standScore i.hr centered_age i.sex i.residence i.education ///
    i.marital i.hhconsumpLevel i.mbmi i.smoke i.drink ///
    i.hibpe i.diabe i.cancer i.lunge i.hearte i.stroke ///
    i.arthre i.dyslipe i.livere i.kidneye i.asthmae ///
    i.newWave || id:, ///
    or cformat(%9.2f) pformat(%5.3f) sformat(%8.0g)
