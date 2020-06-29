## ----setup, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE-------------
# TODO: fix childrens to have apostrophe
# https://www.jstatsoft.org/pages/view/authors
# https://www.jstatsoft.org/pages/view/style
# graphicx, color, hyperref, ae, fancyverb and natbib are included by jss.cls
# fontenc also appears in latex log.
# amsmath is eventually included, but for boldsymbol to work, I need in preamble here.
# rmarkdown latex style, which jss_article inherits.
# generate replication code (done in Makefile for JSS submission, also manually
# for vignette index)
# knitr::purl("efficiency-prebuilt.Rmd")
# build vignette for package (not done by CRAN, devotols, etc), to avoid a
# pandoc 2.2 dependency which r-hub, CRAN etc did not have April 2019. Also
# avoids need for a ton of dependencies on CI platforms, and potentially out of
# whole package DESCRIPTION.
# rmarkdown::render("vignettes/efficiency-prebuilt.Rmd")
# # or, to juggle pkgbuild::build trashing inst/doc..., using .install_extras
# options(tinytex.verbose = TRUE)
# rmarkdown::render("vignettes/efficiency-prebuilt.Rmd", output_dir = "vignettes")
# use render with clean = FALSE to examine intermediates, but _do_ clean
# when changing the source Rmd, to avoid latex and Rmd cache confusion.
# to manually go from generated tex to pdf, for debugging, try something like:
# tinytex::latexmk("efficiency-prebuilt.tex", "pdflatex", "bibtex", 
#   engine_args=c("-file-line-error", "-halt-on-error",
#   "-interaction", "errorstopmode"))
# Note that rstudio will delete this after kniting into vignettes/
# Consider vim :SyntasticCheck markdown/mdl
# This could be useful to generate html version without confusing errors
# between formats: https://bookdown.org/yihui/rmarkdown-cookbook/latex-html.html
# The resuling PDF is included by vignettes/efficiency.Rnw, which
# encapsulates the output of this document %\ %VignetteIndexEntry{icd:
# Efficient Computation of Comorbidities from ICD Codes Using Sparse Matrix
# Multiplication in R}
library("nhds")
library("icd")
requireNamespace("knitr")
# using this here and not compiling for CRAN etc saves a lot of dependencies
requireNamespace("rticles")

fig.width = 6.5
fig.height = fig.width / ((1 + sqrt(5)) / 2)
bitmap_dpi = 600
knitr::opts_chunk$set(verbose = TRUE)
knitr::opts_chunk$set(fig.width = fig.width)
knitr::opts_chunk$set(fig.height = fig.height)
knitr::opts_chunk$set(cache = FALSE) # slower but okay, and error-prone to cache
#knitr::opts_knit$set(concordance = TRUE) # does not work with Rmd -> tex -> pdf
options(tinytex.verbose = TRUE)

kable_caption_bottom <- function(x) {
  y <- unlist(strsplit(as.character(x), '\\n'))
  cap_line <- grep("caption", y)
  cap <- y[cap_line]
  last <- y[length(y)]
  y <- y[-c(cap_line, length(y))]
  y <- c(y, cap, last)
  y <- paste(y, sep = "\\n")
  class(y) <- "knitr_kable"
  attr(y, "format") <- "latex"
  y
}

clean_cap <- function(x) sub("[[:space:]]+", " ", x)
# see https://bookdown.org/yihui/rmarkdown-cookbook/kable.html
# longtable may not be needed, but booktabs looks important.
my_kable <- function(x, col.names, caption, ...)
  kable_caption_bottom(
    knitr::kable(x,
                 #col.names = sprintf("%s", col.names),
                 col.names = col.names,
                 escape = FALSE, # keep my latex markup
                 format = "latex",
                 #longtable = TRUE,
                 booktabs = TRUE,
                 caption = clean_cap(caption),
                 ...))

longtail_fig_cap <- "Frequency distribution of ICD codes in 5,000 pediatric
hospital inpatients showing nearly 4,000 unique ICD codes; 1,401 of them appear
only once. Low-frequency codes like these are of minimal use when statistically
comparing groups, unless they are categorized into comorbidities."

versus_fig_cap <- "Performance comparison of comorbidity packages up to
10,000,000 rows, with 500,000 patient-visits and 20 comorbidities per visit.
Models are fitted where the log-log relationship becomes linear, where rows >
1,000. Using a four-core eight-thread 3.40GHz CPU, 32GB RAM R 3.5.1 using
Linux, kernel 4.15. \\pkg{comorbidity} was run with and without parallel
option, and the best strategy was chosen for each number of iterations."

speedup_fig_cap <- "Relative speed-up using \\pkg{icd} compared to the
alternatives, using the same numbers of patient-visits and comorbidities as in
Figure \\ref{fig:versus}. The variation in the ratio of improvement \\pkg{icd}
offers is likely due to the varying bottlenecks experienced by each package as
the problem scales."



## ----longtail, eval=TRUE, echo=FALSE, fig.cap=longtail_fig_cap----------------
h <- inverse.rle(structure(list(
  lengths = c(
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1,
    2, 1, 1, 1, 1, 1, 3, 1, 2, 1, 1, 1, 4, 2, 1, 1, 2, 2, 4, 4, 2, 1,
    1, 2, 1, 1, 1, 3, 1, 2, 1, 4, 1, 2, 4, 4, 3, 2, 3, 7, 5, 2, 10, 4,
    4, 4, 5, 6, 6, 1, 8, 13, 9, 11, 10, 13, 13, 14, 10, 16, 23, 18,
    24, 27, 32, 39, 45, 33, 51, 87, 86, 76, 126, 166, 240, 365, 632,
    1401),
  values = c(
    525, 405, 296, 283, 281, 268, 227, 209, 193, 180, 168, 166, 156,
    146, 144, 135, 134, 131, 120, 116, 115, 113, 110, 106, 103, 90, 88,
    86, 85, 82, 81, 80, 78, 77, 76, 75, 73, 72, 71, 70, 69, 68, 67, 64,
    63, 62, 61, 56, 55, 54, 53, 51, 50, 49, 48, 46, 45, 44, 43, 41, 40,
    39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23,
    22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5,
    4, 3, 2, 1)), class = "rle"))
  plot(h, type = "h",
       xlab = "ICD code, by descending frequency",
       ylab = "count",
       xlim = c(0, 4000),
       log = "y")


## ----cmbexplainchf, echo=FALSE------------------------------------------------
{
chftail <- tail(
  explain_table(
    icd10_map_quan_deyo[["CHF"]])[c("code", "short_desc")]
)
rownames(chftail) <- NULL
my_kable(
  chftail,
  row.names = NA,
  col.names = c("ICD-10 code", "Definition"),
  linesep = "", # only a few, so don't space every five lines.
  caption = "Excerpt from an ICD-10 comorbidity map for CHF.")
}


## ----terminology, echo=FALSE, eval=TRUE, results='asis'-----------------------
termdf <- as.data.frame(
  matrix(byrow = TRUE, ncol = 2,
         data = c(
           "comorbidity",
           "broad category of disease, e.g., cancer. A set of diagnostic codes that are defined as belonging to this category of diseases.",
           "comorbidity map",
           "set of comorbidities, each comorbidity defined by set of codes",
           "patient-visit",
           paste("record identifier, unique for each",
                 "encounter with a patient, but could",
                 "represent a patient at one moment, or a",
                 "summation of all conditions a patient has",
                 "ever had"),
           "CHF",
           "congestive heart failure, a comorbidity used in examples"
           )))
my_kable(termdf, col.names = c("Term", "Description"),
         align = "lp{10cm}",
         caption = "Terminology")


## ----suckedintoengine---------------------------------------------------------
library("icd")
explain_code("V97.33XD")


## ----exampleambiguity---------------------------------------------------------
explain_code(as.icd9("V10"))
explain_code(as.icd10("V10"))
explain_code("0100")
explain_code("100")


## ----exampleicdcodewho--------------------------------------------------------
explain_code(c("S62", "S62.6"))


## ----exampleicdcodecm---------------------------------------------------------
explain_code(c("S62.60", "S62.607", "S62.607S"))


## ----charlsonnames------------------------------------------------------------
names(icd10_map_charlson)


## ----firstfewcharlson---------------------------------------------------------
icd10_map_charlson[1:2]


## ----heuristicexample---------------------------------------------------------
cmbs1 <- comorbid_ahrq(uranium_pathology)
cmbs2 <- comorbid_ahrq(uranium_pathology,
                       visit_name = "case",
                       icd_name = "icd10")


## ----icd9chfmap2, eval=TRUE---------------------------------------------------
icd9_map_elix[["CHF"]]


## ----icd10chf-----------------------------------------------------------------
icd10_map_quan_deyo[["CHF"]]


## ----icd10chf2, eval=TRUE, echo=TRUE------------------------------------------
grep(pattern = "I50", icd10cm2019[["code"]], value = TRUE)
# I50 should match exactly:
icd10_comorbid_quan_deyo(data.frame(id = 1, code = "I50"))[1, "CHF"]
# I502 is a defined ICD-10-CM code
explain_code("I502")
icd10_comorbid_quan_deyo(data.frame(id = 1, code = "I502"))[1, "CHF"]
# a fictitious code anywhere beneath I50 should match:
is_defined("I502X9")
icd10_comorbid_quan_deyo(data.frame(id = 1, code = "I502X9"))[1, "CHF"]


## ----icd10chf3----------------------------------------------------------------
is_defined("I09")
explain_code("I09")
icd10_comorbid_quan_deyo(data.frame(id = 1, code = "I09"))[1, "CHF"]


## ----icd9chfmap, eval=FALSE, echo=TRUE----------------------------------------
## CHF <- c(
##   "I099", "I110", "I130", "I132", "I255", "I420", "I425", "I426",
##   "I427", "I428", "I429",
##   "I43", "I50", "P290"
## )


## ----comorbidmatrix, echo=FALSE, fig.cap="These are visualizations of some complete maps, black representing the appearance of a particular ICD code in a comorbidity column", dev="png", dpi=bitmap_dpi----
cmbimage <- function(map, xlab, ...) {
  code <- unique(unlist(map))
  x <- comorbid(map = map, x = data.frame(ptid = seq_along(code), code))
  mode(x) <- "integer"
  image(z = t(x), ylim = c(1, 0),
        col = c("#FFFFFF", "#000000"),
        xaxt = 'n', yaxt = 'n',
        useRaster = TRUE, ...)
  mtext(text = xlab, side = 1, line = 1.5, outer = FALSE)
}
{
  par(mfcol = c(1, 3), cex = 1,
      mar = c(2, 2, 2, 1) + 0.1, oma = rep(0.5, 4))
  cmbimage(icd9_map_ahrq, xlab = "ICD-9 AHRQ\ncomorbidities")
  mtext(text = "ICD codes", side = 2, line = 0.5, outer = FALSE)
  cmbimage(icd10_map_ahrq, xlab = "ICD-10 AHRQ\ncomorbidities")
  cmbimage(icd9_map_multi_ccs[["lvl1"]], xlab = "ICD-9 CCS\ncategories")
}


## ----transformoutput----------------------------------------------------------
library(icd)
# Packaged sample Vermont data is already one row per patient, so order should
# be restored in output
v <- comorbid_charlson(vermont_dx)
identical(as.integer(rownames(v)), vermont_dx[["visit_id"]])

vermont_shuffled <- vermont_dx[sample(seq_len(nrow(vermont_dx))), ]
w <- comorbid_charlson(vermont_shuffled)
identical(as.integer(rownames(w)), vermont_shuffled[["visit_id"]])

# Packaged sample Uranium Pathology data is long-format with one row per diagnostic
# code, multiple rows per patient. The case identifier increases monotonically.
u <- comorbid_charlson(uranium_pathology)
uranium_shuffled <- uranium_pathology[sample(seq_len(nrow(uranium_pathology))), ]
# avoiding restoring the order can be avoided, but the ids then differ in order
s1 <- comorbid_charlson(uranium_shuffled, restore_id_order = FALSE)
identical(as.integer(rownames(s1)), unique(uranium_shuffled[["case"]]))
# the default does restore the order as given:
s2 <- comorbid_charlson(uranium_shuffled)
identical(as.integer(rownames(s2)), unique(uranium_shuffled[["case"]]))
# and we can confirm that the output data is identical when re-ordered
s3 <- s1[order(as.integer(rownames(s1))), ]
s4 <- s2[order(as.integer(rownames(s2))), ]
identical(as.integer(rownames(s3)), unique(uranium_pathology[["case"]]))
identical(as.integer(rownames(s4)), unique(uranium_pathology[["case"]]))
# re-ordered results from the shuffled and unshuffled data are the same:
identical(s3, u)
identical(s4, u)


## ----simplemap, eval=FALSE----------------------------------------------------
## list(
##   "Rheumatic Heart Disease" = "I098",
##   "Hypertension" = c("I10", "I11"),
##   "Heart failure" = c("I50", "I110"))


## ----workedexinput, echo=FALSE, results="asis"--------------------------------
workedexinput <- matrix(
  ncol = 4,
  data = c(
    paste("Encounter", c("one", "two", "three", "four")),
    "K401", "I0981", "M352", "I110",
    "", "C450", "I10", "H40001",
    "", "", "", "I10"))
my_kable(
  workedexinput,
  col.names = c("Patient-Visit", sprintf("Code %i", 1:3)),
  caption = paste("Four patient-visits with some ICD-10 codes in",
                  "wide format"))


## ----cmbresult, echo=FALSE, results="asis"------------------------------------
workedexinput <- matrix(
  ncol = 4,
  data = c(
    paste("Encounter", c("one", "two", "three", "four")),
    "", "yes", "", "",
    "", "", "yes", "yes",
    "", "", "", "yes"
  ))
my_kable(
  workedexinput,
  col.names = c("Patient-Visit", "Rheum", "HTN", "CHF"),
  caption = paste("Output of the example using ICD-10 codes.",
                  "\\textit{Rheum} is rheumatic disease, \\textit{HTN} is",
                  "hypertension, \\textit{CHF} is congestive heart failure."))


## ----vermonthead--------------------------------------------------------------
head(vermont_dx[1:10])


## ----vermontlong--------------------------------------------------------------
v <- vermont_dx[-c(2:5)]
v[1:5, 1:5]


## ----vermontcompute-----------------------------------------------------------
v_cmb <- comorbid_charlson(v, return_df = TRUE)


## ----vermontresult, echo=FALSE------------------------------------------------
print(head(v_cmb), row.names = FALSE)


## ----vermontimage, echo=FALSE, fig.cap="This visualization of the result of the comorbidity calculation shows a black cell for each positive comorbidity in one thousand patients from Vermont, USA.", dev="png", dpi=bitmap_dpi----
comorbid_matrix <- comorbid_ahrq(v)
{
  image(comorbid_matrix, col = c("#FFFFFF", "#000000"),
        xaxt = 'n', yaxt = 'n', useRaster = FALSE)
  mtext(text = "Comorbidity", side = 2, line = 1)
  mtext(text = "Patient-visit", side = 1, line = 1)
}


## ----nhdsexample--------------------------------------------------------------
# start with all the adults
adults2010 <- nhds::nhds_adult()
# count patient-visits:
nrow(adults2010)
# use icd to determine which has congestive heart failure:
adults2010[["CHF"]] <- comorbid_ahrq(adults2010)[, "CHF"]
boxplot(age_years ~ CHF,
        data = adults2010,
        outline = FALSE,
        ylab = "Age")


## ----bench, echo=FALSE--------------------------------------------------------
fac <- 1e-3
res <- structure(list(
  datarows = c(10, 100, 1000, 10000, 1e+05, 1e+06, 1e+07),
  icd = c(
    0.0010278929839842,
    0.00120355357648805,
    0.00180120998993516,
    0.008042294124607,
    0.062866612104699,
    1.573,
    15.609
    ),
  comorbidity = c(
    0.0132215245394036,
    0.0231163185089827,
    0.111484928464051,
    1.00265117513482,
    5.36533053999301,
    87.162,
    10986.539
    ),
  medicalrisk = c(
    0.00351565598975867,
    0.0102955044712871,
    0.0715644441079348,
    0.756538210087456,
    7.0441670359578,
    72.286,
    854.759
    )),
  row.names = c(NA, -7L),
  class = "data.frame")
fit_res <- log10(res[4:7, ])
icd_model <- lm(icd ~ datarows, data = fit_res)
cmb_model <- lm(comorbidity ~ datarows, data = fit_res)
mdr_model <- lm(medicalrisk ~ datarows, data = fit_res)
pred_hours <- function(model)
  10 ^ predict(model, data.frame(datarows = log10(20 * 1e8))) / 3600
preds <- vapply(list(icd_model, cmb_model, mdr_model),
                FUN = pred_hours, FUN.VALUE = numeric(1))
names(preds) <- c("icd", "comorbidity", "medicalrisk")
xseq = seq(0, 7)
yseq = seq(-3, 3, 3)
ysrr = 0:4
logxaxis <- sapply(paste0("expression(10^", xseq, ")"),
                   function(x) eval(parse(text = x)))
logyaxis <- sapply(paste0("expression(10^", yseq, ")"),
                   function(x) eval(parse(text = x)))
logyaxrr <- sapply(paste0("expression(", 10^ysrr, ")"),
                   function(x) eval(parse(text = x)))
colours <- c(comorbidity = 'darkred',
             icd = 'black',
             medicalrisk = 'darkblue')
rr <- data.frame(datarows = res$datarows,
                       icd = 1,
                       comorbidity = res$comorbidity / res$icd,
                       medicalrisk = res$medicalrisk / res$icd)

## ----versus, echo=FALSE, fig.cap=versus_fig_cap-------------------------------
plt_inset <- 0.05
xmin = 10
  {
  plot(NA, NA, log = "xy",
       type = "l", col = 'darkred',
       xlab = "rows of data",
       ylab = "seconds",
       xlim = c(xmin, max(res$datarows)),
       ylim = c(fac, max(c(res$medicalrisk, res$comorbidity))),
       xaxt = "n", yaxt = "n"
  )
  axis(1, 10^xseq, logxaxis)
  axis(2, 10^yseq, logyaxis)
  lines(res$datarows, res$comorbidity, col = colours["comorbidity"])
  lines(res$datarows, res$icd, col = colours["icd"])
  lines(res$datarows, res$medicalrisk, col = colours["medicalrisk"])
  abline(icd_model, col = colours["icd"], lty = 3)
  abline(cmb_model, col = colours["comorbidity"], lty = 3)
  abline(mdr_model, col = colours["medicalrisk"], lty = 3)
  legend("topleft", inset = plt_inset, legend = names(res[-1]),
  fill = colours[names(res[-1])])
}


## ----speedup, echo=FALSE, fig.width=6, fig.height=3.7, fig.cap=speedup_fig_cap----
{
  ymaxrr <- 10^ceiling(
    log10(
      max(c(rr$medicalrisk, rr$comorbidity))
    )
  )
  plot(NA, NA, log = "xy",
       type = "l", col = 'darkred',
       xlab = "rows of data",
       ylab = "time / time using 'icd'",
       xlim = c(xmin, max(rr$datarows)),
       ylim = c(1, ymaxrr),
       xaxt = "n", yaxt = "n"
  )
  axis(1, 10^xseq, logxaxis)
  axis(2, 10^ysrr, logyaxrr)
  lines(rr$datarows, rr$icd, col = colours["icd"])
  lines(rr$datarows, rr$comorbidity, col = colours["comorbidity"])
  lines(rr$datarows, rr$medicalrisk, col = colours["medicalrisk"])
  legend("topleft", inset = plt_inset,
         legend = names(rr[-1]),
         fill = colours[names(rr[-1])])
}


## ----bigdata, echo=FALSE, fig.cap="Predicted duration of computation for one hundred-million patient-visits, with twenty diagnoses per patient"----
{
  barplot(preds,
          col = colours[c("icd", "comorbidity", "medicalrisk")],
          ylab = "hours", log = "y", ylim = c(1, 100000))
  legend("topleft", inset = plt_inset, legend = names(preds), fill = colours[names(preds)])
}



## ----pulmonaryproblem---------------------------------------------------------
sc <- c("Chronic Obstructive Pulmonary Disease And Allied Conditions",
        "Pneumoconioses And Other Lung Diseases Due To External Agents")
icd9_sub_chapters[sc][[1]]
icd9_sub_chapters[sc][[2]]


## ----permute497---------------------------------------------------------------
explain_code(c("497", "479", "947", "974", "749", "794"), warn = FALSE)


## ----charlson497--------------------------------------------------------------
match("497", icd9_map_charlson)


## ----neighbor497--------------------------------------------------------------
"49699" %in% icd9_map_quan_deyo[["Pulmonary"]]
"496999" %in% icd9_map_charlson


## ----moregenerous-------------------------------------------------------------
alice <- data.frame(id = "alice", icd9 = "49699")
comorbid_charlson(alice, return_df = TRUE)[["Pulmonary"]]


## ----elixrange----------------------------------------------------------------
head("243" %i9da% "244.2")
"244" %in% ("243" %i9da% "244.2")


## ----nhdscharlsonexample------------------------------------------------------
adults2010_charl <- adults2010
adults2010_charl["charlson"] <- charlson(adults2010)
adults2010_charl["adm_type"] <- factor(adults2010_charl[["adm_type"]], 
                                    c("emergency", "urgent", "elective"))
boxplot(charlson ~ adm_type,
        data = adults2010_charl,
        las = 2, 
        varwidth = TRUE, 
        outline = FALSE,
        xlab = NULL,
        ylab = "Charlson Score")


## ----validity-----------------------------------------------------------------
is_valid(c("441", "441.0", "441.01", "XXX"))
is_leaf(c("441", "441.0", "441.01", "XXX"))
is_billable(c("441", "441.0", "441.01", "XXX"))
head(
  data.frame(code = children("441"),
             billable = is_billable(children("441"))))


## ----hierarchy441-------------------------------------------------------------
children("441")


## ----explain4410--------------------------------------------------------------
explain_code(children("4410"))


## ----explaineach4410----------------------------------------------------------
explain_code(children("4410"), condense = FALSE)

