* Clear env
clear
clear matrix
clear mata
set maxvar 50000

* Load the data
use "/Users/bruce/Desktop/GHRC Yan Lab/Hearing impairment & PF 2023/outputs/pooled_11_15.dta", clear

* Encode the categorical variable
// encode wave, gen(newWave)

* Drop variable
// drop wave age

* Declare the panel data structure
// xtset id visit

* Multiple imputation with chained equations set-up
mi set mlong

mi reshape wide age wave marital hr hhconsumpLevel mbmi hibpe diabe cancer lunge hearte stroke arthre dyslipe livere kidneye asthmae smoke drink gait cstand balance handgrip, i(id) j(visit)

// mi register imputed education marital hr hhconsumpLevel mbmi hibpe diabe cancer lunge hearte stroke arthre dyslipe livere kidneye asthmae smoke drink gait cstand balance handgrip

mi register imputed education marital0 marital1 marital2 hr0 hr1 hr2 hhconsumpLevel0 hhconsumpLevel1 hhconsumpLevel2 mbmi0 mbmi1 mbmi2 hibpe0 hibpe1 hibpe2 diabe0 diabe1 diabe2 cancer0 cancer1 cancer2 lunge0 lunge1 lunge2 hearte0 hearte1 hearte2 stroke0 stroke1 stroke2 arthre0 arthre1 arthre2 dyslipe0 dyslipe1 dyslipe2 livere0 livere1 livere2 kidneye0 kidneye1 kidneye2 asthmae0 asthmae1 asthmae2 smoke0 smoke1 smoke2 drink0 drink1 drink2 gait0 gait1 gait2 cstand0 cstand1 cstand2 balance0 balance1 balance2 handgrip0 handgrip1 handgrip2

* Conduct multiple imputation
// mi impute chained (regress) gait cstand handgrip (logit) marital hr hibpe diabe cancer lunge hearte stroke arthre dyslipe livere kidneye asthmae smoke drink (ologit) education hhconsumpLevel mbmi balance = i.wave age i.sex i.residence c.visit, add(1) rseed(2024) noisily

mi impute chained (regress) gait0 gait1 gait2 cstand0 cstand1 cstand2 handgrip0 handgrip1 handgrip2 (logit, augment) marital0 marital1 marital2 hr0 hr1 hr2 hibpe0 hibpe1 hibpe2 diabe0 diabe1 diabe2 cancer0 cancer1 cancer2 lunge0 lunge1 lunge2 hearte0 hearte1 hearte2 stroke0 stroke1 stroke2 arthre0 arthre1 arthre2 dyslipe0 dyslipe1 dyslipe2 livere0 livere1 livere2 kidneye0 kidneye1 kidneye2 asthmae0 asthmae1 asthmae2 smoke0 smoke1 smoke2 drink0 drink1 drink2 (ologit, augment) education hhconsumpLevel0 hhconsumpLevel1 hhconsumpLevel2 mbmi0 mbmi1 mbmi2 balance0 balance1 balance2 = age0 age1 age2 i.sex i.residence, add(1) rseed(2024) noisily force
