# Occupancy Modeling in R

### What is Occupancy?

Occupancy is a parameter that describes whether a particular entity *occupies* a particular site (i.e: an animal is present in a specific nature reserve). Originally developed for use in ecological research, occupany modeling is a statistical technique that seeks to estimate the "true" occupancy of a given set of sites based on survey data. In the most common approaches, models take into account that the probability of detection is almost certainly imperfect and unknown (that is, there is some unspecified probability, greater than 0, of surveying a site and *not detecting* a species that is truly present there, a **false negative**). However, note that very few models can cope with **false positives** (surveying a site and "detecting" a species that is truly *absent* there).

Unfortunately, this field is so young that the terminology has not quite solidified yet: different groups may use different names for the same techniques. Whenever introducing a new term, I will also list any synonyms I have seen before. Hopefully that approach will allow the reader to effectively search for more information about a particular term, if it is difficult to find resources using the terminology that I prefer.

With that in mind: there are **four main types of occupancy models.**

1. **Single-species, single-season (a.k.a: "one-species" or even just "occupancy" models)**
This simplest type of model seeks to study whether a given species is present in one or more sites at a particular time. It is based on taking multiple "surveys" (a.k.a: "observations") of each site and looking at the trends in detection over those surveys.
2. **Single-species, multiple-season (a.k.a: "dynamic occupancy" or "colonization-extinction" models)**
This type of model looks at *changes in occupancy over time.* It requires taking samples in multiple "seasons" (a.k.a: "years") and looking at patterns across those seasons. This approach also models the probability that a site that is unoccupied in one season may be "colonized" (so it is occupied in later seasons) and a site that is occupied in one season may become "extinct" (so it is *not* occupied in later seasons).
3. **Multi-species, single-season**
This type of model uses the estimated occupancy of *other species in the community* to model the occupancy of a given species. It is more complex than the two single-species models, and we will not discuss it here.
4. **Multi-species, multi-season**
This type of model is the most complex. It uses the estimated occupancy of *other species in the community* as covariates, just as in the multi-species/single-season case, but also models the changes over time, as in the single-species/multi-season case. For an example, see [this 2015 publication by MW Tobler, et al]. We will not discuss this type of model here.

For a more comprehensive discussion of occupancy modeling techniques, especially for multi-species cases, the 2005 book [Occupancy Estimation and Modeling] is an excellent reference.

[this 2015 publication by MW Tobler, et al]: https://www.rdcu.be/Omqb "Spatiotemporal hierarchical modelling of species richness and occupancy using camera trap data"
[Occupancy Estimation and Modeling]: https://www.elsevier.com/books/occupancy-estimation-and-modeling/mackenzie/978-0-12-088766-8 "ISBN: 9780120887668"

---

### Overview: Dynamic Occupancy Modeling with `unmarked`

There are 3 main methods for modeling occupancy in R, discussed at length in [this 2017 publication by O. Gimenez]. The most important conclusion of that study was that the package `unmarked` (based on C\+\+) is the "fastest" (most computationally efficient) followed by `R2admb`, with `rjags` (based on JAGS) trailing far behind. Based on that information, **`unmarked` is generally the best choice for any occupancy analysis** (so long as it supports all the features you need; if not, you'll need to learn ADMB and `r2admb`).

The basic workflow for doing dynamic occupancy modeling in `unmarked` is as follows:

1. Create an `unmarkedMultFrame` object containing all the detection and covariate data
2. Create several `colext` models based on the `umarkedMultFrame`
3. Use `fitList` to compare all the `colext` model objects
4. Choose the "best" model based on the results from `fitList` and your own requirements

In the next section, we will go over these four steps in more detail.

[this 2017 publication by O. Gimenez]: https://rpubs.com/ogimenez/297167 "Fitting dynamic occupancy models in ADMB"

---

### Using `colext` for single-species, multi-season models

The data structures used in `unmarked` are specialized classes introduced by the package. As a result, they are powerful and flexible, but not quite as as beginner-friendly as the data structures of stock `base` R. This section offers a guide to constructing and then using them with several of the functions offered by the package.

#### Creating an `unmarkedMultFrame`

There are five main pieces to an `unmarkedMultFrame`:

1. The number of seasons `numPrimary`
2. The detection matrix `y`
3. The site-covariates data.frame `siteCovs`
4. The observation-covariates list `obsCovs`
5. The (confusingly titled) season-covariates list `yearlySiteCovs`

The **integer** `numPrimary` is just the number of seasons (also called "primary time periods" or "years", but they need not correspond with 12-month periods). *N.B: For convenience, I will define `surveyTotal` as the product of `numPrimary` and the number of surveys <u>per season</u>, as we will use that quanity frequently.*

The detection **matrix** `y` should have `surveyTotal` columns (wide), with each row corresponding to a different site. In each position, record a 1 if the species of interest was detected at that site during that survey, a 0 if it was not deteced, and an NA if the survey did not take place at all.

The site-covariates **data.frame** `siteCovs` should have one column for each covariate (elevation, forest cover, etc.) with the rows corresponding to the rows (sites) of `y`. This is where information is stored about those covariates that change only with each site. For example, the elevation above sea level.

The observations-covariates list `obsCovs` should be a **list of data.frames**. Each data.frame should have the same dimensions as `y` (that is, `surveyTotal` columns, with the rows corresponding to the rows (sites) of `y`). This is where information is stored about those covariates that change with each survey. For example, the temperature of the environment when each survey took place.

The season-covariates list `yearlySiteCovs` should be a **list of matrices**. Each matrix should have `numPrimary` columns, with the rows corresponding to the rows (sites) of `y`. This is where information is stored about those covariates that change with each season, but *not* each survey. For example, the amount of rainfall in the previous season.

**A note about NA (missing) values in this data:**
> AIC-based model selection requires that all models are fit to the same data. `unmarked` removes missing data in a context specific way.  For missing `siteCovs`, the entire row of data must be removed.  However, for missing `yearlySiteCovs` or `obsCovs`, only the corresponding observation are removed.  Thus, **if `unmarked` removes different observations from different models, the models cannot be compared using AIC**. A way around this is to remove the detection data corresponding to missing covariates before fitting the models.
> 
> --- [Marc Kery and Richard Chandler], emphasis added

Although, if *all* the models being compared have the covariate with the missing data, then it shouldn't be an issue, as `unmarked` will process them all in the same way.

[Marc Kery and Richard Chandler]: https://cran.r-project.org/web/packages/unmarked/vignettes/colext.pdf "Vignette: Dynamic occupancy models in unmarked"

#### Creating `colext` models

To be continued...

---
