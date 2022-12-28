---
title: Insomnia, Depression, and Sleep Duration
author: Andrew S. Tubbs
date: 'Published January 24, 2020'
slug: PHQ-insomnia
categories: []
tags:
  - depression
  - insomnia
  - PHQ-9
  - SHADES
subtitle: ''
summary: ''
authors: []
lastmod: '2022-12-27T07:31:16-07:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
output:
  html_document:
    code_download: true
    code_folding: hide
---

<style type="text/css">
pre {
  max-height: 300px;
  overflow-y: auto;
}
</style>

## Summary

These analyses were published as [“Relationship between insomnia and depression in a community sample depends on habitual sleep duration”](https://doi.org/10.1007/s41105-020-00255-z) in the journal *Sleep and Biological Rhythms*. They were inspired by a study by [Perlis et al.](https://pubmed.ncbi.nlm.nih.gov/9359976/) using canonical correlations to relate individual sleep EEG features to specific depressive symptoms. The principal findings were that 1) insomnia severity and sleep duration were both associated with individual depression symptoms, 2) insomnia, but not sleep duration, was related to depression severity among depressed individuals (PHQ-9 score \> 10), and 3) the interaction between insomnia and short sleep duration in predicting depression severity was small and negative, suggesting shared variance between insomnia and sleep duration.

## Dataset

The data used for these analyses derive from the Sleep and Healthy Activity, Diet, Environment, and Socialization [(SHADES)](https://www.sleephealthresearch.com/shades/index.html) Study conducted by Michael Grandner and funded by the National Institute of Environmental Health Sciences (R21ES022931).

## Variables

Insomnia severity was measured using the Insomnia Severity Index. Depression severity and symptoms were measured using the PHQ-9. Symptoms were measured individually and combined into an adjusted PHQ-9 total score (excluding the sleep item to avoid collinearity). Sleep duration was derived from a single self-report question used in the NHANES and categorized as Short Sleep (\< 7 hours), Recommended Sleep (7-8 hours), and Long Sleep (\> 8 hours). Covariates were age, sex, race, education, and BMI (by self-report).

``` r
#### Load Packages ####
library(gtsummary)
library(gt)
library(nnet)
library(foreign)
library(ggpubr)
library(car)
library(broom)
library(tidyverse)

#### Load Data ####
phq9.data <- read.dta("c:/users/atala/sync/research/data/shades/shades.dta") %>% as_tibble() %>%
  transmute(
    "ID" = response_id, 
    "ISItotal" = isi,
    "ISIfalling" = isi1_falling, 
    "ISIstaying" = isi2_staying, 
    "ISIema" = isi3_ema,
    "ISIsatisfied" = as.numeric(isi4_satisfied) %>% 
      factor(., levels = c("1","2","3","4","5"),labels = c("Very Satisfied","Satisfied","Neutral","Dissatisfied","Very Dissatisfied")), 
    "ISIinterfere" = isi5_interfere, 
    "ISInoticeable" = isi6_noticeable, 
    "ISIworried" = isi7_worried,
    "ISIscore" = case_when(ISItotal < 8 ~ "No Insomnia",
                           ISItotal %in% 8:14 ~ "Subthreshold Insomnia",
                           ISItotal %in% 15:21 ~ "Moderate Insomnia",
                           ISItotal > 21 ~ "Severe Insomnia") %>%
      factor(levels=c("No Insomnia","Subthreshold Insomnia","Moderate Insomnia","Severe Insomnia")),
    "PHQinterest" = phq1_interest, 
    "PHQdepress" = phq2_depress, 
    "PHQsleep" = phq3_sleep, 
    "PHQtired" = phq4_tired,
    "PHQappetite" = phq5_appetite,  
    "PHQfailure" = phq6_failure, 
    "PHQconcentrate" = phq7_concentrate,
    "PHQpsychomotor" = phq8_psychomotor, 
    "PHQsuicide" = phq9_suicide, 
    "PHQdifficulty" = phq10_difficulty,
    "PHQtotal" = phq_total,
    "PHQtotal2" = PHQtotal - PHQsleep,
    "PHQscore" = case_when(PHQtotal < 5 ~ "Minimal Depression",
                            PHQtotal %in% 5:9 ~ "Mild Depression",
                            PHQtotal %in% 10:14 ~ "Moderate Depression",
                            PHQtotal %in% 15:19 ~ "Moderately-Severe Depression",
                            PHQtotal > 19 ~ "Severe Depression") %>%
      factor(levels=c("Minimal Depression","Mild Depression","Moderate Depression",
                      "Moderately-Severe Depression","Severe Depression")),
    "Depressed" = case_when(PHQtotal > 9 ~ "Depressed",
                             PHQtotal < 10 ~ "Not Depressed") %>%
      factor(levels = c("Not Depressed","Depressed")),
    "TST" = nhanes_tst, 
    "SleepDuration" = case_when(TST>8~"Long Sleep", 
                                 TST<7~"Short Sleep", 
                                 TST %in% 7:8~"Recommended Sleep") %>% 
      factor(levels = c("Recommended Sleep","Short Sleep","Long Sleep")),
    "Age" = demo_age_years2,
    "Sex" = demo_sex %>% factor(0:1,c("Male","Female")),
    "Race" = case_when(demo_race_white_only==1~"White",
                       demo_race_black_only==1~"Black",
                       demo_race_asian_only==1~"Asian",
                       demo_race_hisp_only==1~"Hispanic",
                       demo_race_native_only==1~"Native",
                       demo_race_pacisland_only==1~"Other",
                       demo_race_multiracial==1~"Other") %>% 
      factor(levels = c("White","Black","Asian","Hispanic","Native","Other")) %>%
      replace_na(., "Other"),
    "Education" = factor(demo_education2, 0:3,c("College","Some College","High School","Less than high school")),
    "BMI" = body_weight_curr_kg/((body_height_cm)/100)^2
  )
```

## Table 1

The characteristics of the sample are presented below in Table 1.

``` r
#### Table 1: Demographics and Response Characteristics of the Sample ####
tbl_summary(
  phq9.data,
  label = list("Age" = "Age (years)", "BMI" = "BMI (kg/m^2)", "ISIscore" = "ISI Category","PHQscore" = "PHQ-9 Category"),
  by="SleepDuration",
  include = c("SleepDuration","Age","BMI","Sex","Education","Race","ISIscore","PHQscore"),
  statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_continuous() ~ c(1,2), all_categorical() ~ c(0,1))
) %>% add_overall() %>% add_p(test = list(all_continuous() ~ "aov", all_categorical() ~ "chisq.test"))
```

<div id="xrqeupcuvj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#xrqeupcuvj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#xrqeupcuvj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xrqeupcuvj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#xrqeupcuvj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#xrqeupcuvj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#xrqeupcuvj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrqeupcuvj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#xrqeupcuvj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#xrqeupcuvj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#xrqeupcuvj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#xrqeupcuvj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#xrqeupcuvj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#xrqeupcuvj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#xrqeupcuvj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#xrqeupcuvj .gt_from_md > :first-child {
  margin-top: 0;
}

#xrqeupcuvj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#xrqeupcuvj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#xrqeupcuvj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#xrqeupcuvj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#xrqeupcuvj .gt_row_group_first td {
  border-top-width: 2px;
}

#xrqeupcuvj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrqeupcuvj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#xrqeupcuvj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#xrqeupcuvj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrqeupcuvj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrqeupcuvj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#xrqeupcuvj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#xrqeupcuvj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#xrqeupcuvj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xrqeupcuvj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrqeupcuvj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#xrqeupcuvj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#xrqeupcuvj .gt_left {
  text-align: left;
}

#xrqeupcuvj .gt_center {
  text-align: center;
}

#xrqeupcuvj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#xrqeupcuvj .gt_font_normal {
  font-weight: normal;
}

#xrqeupcuvj .gt_font_bold {
  font-weight: bold;
}

#xrqeupcuvj .gt_font_italic {
  font-style: italic;
}

#xrqeupcuvj .gt_super {
  font-size: 65%;
}

#xrqeupcuvj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#xrqeupcuvj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#xrqeupcuvj .gt_indent_1 {
  text-indent: 5px;
}

#xrqeupcuvj .gt_indent_2 {
  text-indent: 10px;
}

#xrqeupcuvj .gt_indent_3 {
  text-indent: 15px;
}

#xrqeupcuvj .gt_indent_4 {
  text-indent: 20px;
}

#xrqeupcuvj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Overall&lt;/strong&gt;, N = 1,007&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Overall</strong>, N = 1,007<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Recommended Sleep&lt;/strong&gt;, N = 480&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Recommended Sleep</strong>, N = 480<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Short Sleep&lt;/strong&gt;, N = 477&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Short Sleep</strong>, N = 477<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Long Sleep&lt;/strong&gt;, N = 50&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>Long Sleep</strong>, N = 50<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Age (years)</td>
<td headers="stat_0" class="gt_row gt_center">34.0 (9.45)</td>
<td headers="stat_1" class="gt_row gt_center">32.8 (9.33)</td>
<td headers="stat_2" class="gt_row gt_center">35.6 (9.54)</td>
<td headers="stat_3" class="gt_row gt_center">30.2 (6.51)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">BMI (kg/m^2)</td>
<td headers="stat_0" class="gt_row gt_center">26.6 (7.85)</td>
<td headers="stat_1" class="gt_row gt_center">25.3 (5.63)</td>
<td headers="stat_2" class="gt_row gt_center">27.9 (9.19)</td>
<td headers="stat_3" class="gt_row gt_center">27.4 (10.01)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Sex</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center">0.082</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Male</td>
<td headers="stat_0" class="gt_row gt_center">388 (38.5%)</td>
<td headers="stat_1" class="gt_row gt_center">169 (35.2%)</td>
<td headers="stat_2" class="gt_row gt_center">201 (42.1%)</td>
<td headers="stat_3" class="gt_row gt_center">18 (36.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Female</td>
<td headers="stat_0" class="gt_row gt_center">619 (61.5%)</td>
<td headers="stat_1" class="gt_row gt_center">311 (64.8%)</td>
<td headers="stat_2" class="gt_row gt_center">276 (57.9%)</td>
<td headers="stat_3" class="gt_row gt_center">32 (64.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Education</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    College</td>
<td headers="stat_0" class="gt_row gt_center">563 (55.9%)</td>
<td headers="stat_1" class="gt_row gt_center">323 (67.3%)</td>
<td headers="stat_2" class="gt_row gt_center">205 (43.0%)</td>
<td headers="stat_3" class="gt_row gt_center">35 (70.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Some College</td>
<td headers="stat_0" class="gt_row gt_center">312 (31.0%)</td>
<td headers="stat_1" class="gt_row gt_center">118 (24.6%)</td>
<td headers="stat_2" class="gt_row gt_center">181 (37.9%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (26.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    High School</td>
<td headers="stat_0" class="gt_row gt_center">106 (10.5%)</td>
<td headers="stat_1" class="gt_row gt_center">31 (6.5%)</td>
<td headers="stat_2" class="gt_row gt_center">73 (15.3%)</td>
<td headers="stat_3" class="gt_row gt_center">2 (4.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Less than high school</td>
<td headers="stat_0" class="gt_row gt_center">26 (2.6%)</td>
<td headers="stat_1" class="gt_row gt_center">8 (1.7%)</td>
<td headers="stat_2" class="gt_row gt_center">18 (3.8%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">Race</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    White</td>
<td headers="stat_0" class="gt_row gt_center">597 (59.3%)</td>
<td headers="stat_1" class="gt_row gt_center">324 (67.5%)</td>
<td headers="stat_2" class="gt_row gt_center">242 (50.7%)</td>
<td headers="stat_3" class="gt_row gt_center">31 (62.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Black</td>
<td headers="stat_0" class="gt_row gt_center">251 (24.9%)</td>
<td headers="stat_1" class="gt_row gt_center">83 (17.3%)</td>
<td headers="stat_2" class="gt_row gt_center">162 (34.0%)</td>
<td headers="stat_3" class="gt_row gt_center">6 (12.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Asian</td>
<td headers="stat_0" class="gt_row gt_center">55 (5.5%)</td>
<td headers="stat_1" class="gt_row gt_center">32 (6.7%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (4.8%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Hispanic</td>
<td headers="stat_0" class="gt_row gt_center">46 (4.6%)</td>
<td headers="stat_1" class="gt_row gt_center">17 (3.5%)</td>
<td headers="stat_2" class="gt_row gt_center">23 (4.8%)</td>
<td headers="stat_3" class="gt_row gt_center">6 (12.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Native</td>
<td headers="stat_0" class="gt_row gt_center">3 (0.3%)</td>
<td headers="stat_1" class="gt_row gt_center">1 (0.2%)</td>
<td headers="stat_2" class="gt_row gt_center">2 (0.4%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Other</td>
<td headers="stat_0" class="gt_row gt_center">55 (5.5%)</td>
<td headers="stat_1" class="gt_row gt_center">23 (4.8%)</td>
<td headers="stat_2" class="gt_row gt_center">25 (5.2%)</td>
<td headers="stat_3" class="gt_row gt_center">7 (14.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">ISI Category</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    No Insomnia</td>
<td headers="stat_0" class="gt_row gt_center">350 (34.8%)</td>
<td headers="stat_1" class="gt_row gt_center">252 (52.5%)</td>
<td headers="stat_2" class="gt_row gt_center">78 (16.4%)</td>
<td headers="stat_3" class="gt_row gt_center">20 (40.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Subthreshold Insomnia</td>
<td headers="stat_0" class="gt_row gt_center">389 (38.6%)</td>
<td headers="stat_1" class="gt_row gt_center">171 (35.6%)</td>
<td headers="stat_2" class="gt_row gt_center">199 (41.7%)</td>
<td headers="stat_3" class="gt_row gt_center">19 (38.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Moderate Insomnia</td>
<td headers="stat_0" class="gt_row gt_center">212 (21.1%)</td>
<td headers="stat_1" class="gt_row gt_center">55 (11.5%)</td>
<td headers="stat_2" class="gt_row gt_center">146 (30.6%)</td>
<td headers="stat_3" class="gt_row gt_center">11 (22.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Severe Insomnia</td>
<td headers="stat_0" class="gt_row gt_center">56 (5.6%)</td>
<td headers="stat_1" class="gt_row gt_center">2 (0.4%)</td>
<td headers="stat_2" class="gt_row gt_center">54 (11.3%)</td>
<td headers="stat_3" class="gt_row gt_center">0 (0.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">PHQ-9 Category</td>
<td headers="stat_0" class="gt_row gt_center"></td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td>
<td headers="stat_3" class="gt_row gt_center"></td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Minimal Depression</td>
<td headers="stat_0" class="gt_row gt_center">308 (30.6%)</td>
<td headers="stat_1" class="gt_row gt_center">201 (41.9%)</td>
<td headers="stat_2" class="gt_row gt_center">94 (19.7%)</td>
<td headers="stat_3" class="gt_row gt_center">13 (26.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mild Depression</td>
<td headers="stat_0" class="gt_row gt_center">309 (30.7%)</td>
<td headers="stat_1" class="gt_row gt_center">154 (32.1%)</td>
<td headers="stat_2" class="gt_row gt_center">138 (28.9%)</td>
<td headers="stat_3" class="gt_row gt_center">17 (34.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Moderate Depression</td>
<td headers="stat_0" class="gt_row gt_center">209 (20.8%)</td>
<td headers="stat_1" class="gt_row gt_center">78 (16.2%)</td>
<td headers="stat_2" class="gt_row gt_center">123 (25.8%)</td>
<td headers="stat_3" class="gt_row gt_center">8 (16.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Moderately-Severe Depression</td>
<td headers="stat_0" class="gt_row gt_center">108 (10.7%)</td>
<td headers="stat_1" class="gt_row gt_center">28 (5.8%)</td>
<td headers="stat_2" class="gt_row gt_center">74 (15.5%)</td>
<td headers="stat_3" class="gt_row gt_center">6 (12.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Severe Depression</td>
<td headers="stat_0" class="gt_row gt_center">73 (7.2%)</td>
<td headers="stat_1" class="gt_row gt_center">19 (4.0%)</td>
<td headers="stat_2" class="gt_row gt_center">48 (10.1%)</td>
<td headers="stat_3" class="gt_row gt_center">6 (12.0%)</td>
<td headers="p.value" class="gt_row gt_center"></td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="6"><sup class="gt_footnote_marks">1</sup> Mean (SD); n (%)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="6"><sup class="gt_footnote_marks">2</sup> One-way ANOVA; Pearson's Chi-squared test</td>
    </tr>
  </tfoot>
</table>
</div>

## Data Distribution

Let’s look at the distribution of the data.

``` r
#### Histograms to justify QP modeling ####
phq9.hist <- phq9.data %>% select(ID,PHQinterest,PHQdepress,PHQsleep,PHQtired,PHQappetite,
                                  PHQfailure,PHQconcentrate,PHQpsychomotor,PHQsuicide) %>% 
  gather("Symptom","Frequency",-"ID") %>%
  mutate(Symptom = factor(Symptom,levels=c("PHQinterest","PHQdepress","PHQsleep","PHQtired","PHQappetite",
                                            "PHQfailure","PHQconcentrate","PHQpsychomotor","PHQsuicide"),
                           labels = c("Anhedonia","Depressed Mood","Disturbed Sleep","Fatigue","Appetite Dysregulation",
                                      "Feelings of Failure","Difficulty Concentrating","Psychomotor Symptoms","Suicidal Behavior"))
         )
totalphq.plot <- gghistogram(phq9.data,x="PHQtotal",legend = "none",ylab = "Number of Responses",xlab = "PHQ - 9 Total Score", 
                             title = "(A)",bins=28,ggtheme=theme_pubr(),fill="black")
itemphq.plot <- gghistogram(phq9.hist,x="Frequency",legend = "none",ylab = "Number of Responses",xlab = "PHQ - 9 Total Score",
                            title = "(B)",bins=28,facet.by = "Symptom",ggtheme=theme_pubr(),fill="black",binwidth = 1)
```

The simplest approach to relate insomnia and sleep duration to depression would be using linear regression. This assumes, however, that the outcome (i.e., depression severity and frequency of symptoms) are normally distributed. Using histograms, however, it is clear these data are not normally distributed. In fact, they follow a Poisson-like distribution, with most responses skewed toward smaller values.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

## Model the Whole Sample

The following code models the adjusted PHQ-9 score (“PHQtotal2”) as a function of insomnia severity and sleep duration. Models are unadjusted and adjusted for age, sex, race, and ethnicity.

``` r
#### Analysis 1) Model adjPHQ-9 Score as a function of ISI score and sleep duration in the whole sample ####
## Total Score
# Unadjusted
phqscore.qp <- glm(PHQtotal2~ISItotal*SleepDuration,family="quasipoisson",phq9.data)
phqscore.qp.maineff <- bind_cols("Symptom" = rep("Total",5),
                                 "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                 bind_rows(deltaMethod(phqscore.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(phqscore.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(phqscore.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(phqscore.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(phqscore.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                 "p-value" = summary(phqscore.qp)$coefficients[2:6,4])
# Adjusted for Age, Sex, Race, Education, BMI
phqscore.qp.full <- glm(PHQtotal2~ISItotal*SleepDuration+Age+Sex+Race+Education+BMI,family="quasipoisson",phq9.data)
phqscore.qp.full.maineff <- bind_cols("Symptom" = rep("Total",5),
                                      "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                      bind_rows(deltaMethod(phqscore.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(phqscore.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(phqscore.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(phqscore.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(phqscore.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                      "p-value" = summary(phqscore.qp.full)$coefficients[c(2:4,16,17),4])
# Plot PHQScore vs. ISItotal by SleepDuration
phqscore.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQtotal2),
                   predict(phqscore.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  geom_point(aes(y = PHQtotal2),position="jitter")+
  labs(x = "ISI Score",y="Adjusted PHQ - 9 Score")+
  theme_pubr()+theme(legend.title = element_blank())

## Individual Symptoms
# Anhedonia
interest.qp <- glm(PHQinterest~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
interest.qp.maineff <- bind_cols("Symptom" = rep("Interest",5),
                                 "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                 bind_rows(deltaMethod(interest.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(interest.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(interest.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(interest.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(interest.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                 "p-value" = summary(interest.qp)$coefficients[2:6,4])
interest.qp.full <- glm(PHQinterest~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
interest.qp.full.maineff <- bind_cols("Symptom" = rep("Interest",5),
                                      "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                      bind_rows(deltaMethod(interest.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(interest.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(interest.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(interest.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(interest.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                      "p-value" = summary(interest.qp.full)$coefficients[c(2:4,16,17),4])
interest.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQinterest),
                   predict(interest.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Anhedonia")+
  theme_pubr()+theme(legend.position = "none")

# Depressed Mood
depress.qp <- glm(PHQdepress~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
depress.qp.maineff <- bind_cols("Symptom" = rep("Depress",5),
                                "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                bind_rows(deltaMethod(depress.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(depress.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(depress.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(depress.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(depress.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                "p-value" = summary(depress.qp)$coefficients[2:6,4])
depress.qp.full <- glm(PHQdepress~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
depress.qp.full.maineff <- bind_cols("Symptom" = rep("Depress",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(depress.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(depress.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(depress.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(depress.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(depress.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(depress.qp.full)$coefficients[c(2:4,16,17),4])

depress.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQdepress),
                   predict(depress.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Depressed Mood")+
  theme_pubr()+theme(legend.position = "none")

# Appetite
appetite.qp <- glm(PHQappetite~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
appetite.qp.maineff <- bind_cols("Symptom" = rep("Appetite",5),
                                 "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                 bind_rows(deltaMethod(appetite.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(appetite.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(appetite.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(appetite.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                           deltaMethod(appetite.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                 "p-value" = summary(appetite.qp)$coefficients[2:6,4])
appetite.qp.full <- glm(PHQappetite~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
appetite.qp.full.maineff <- bind_cols("Symptom" = rep("Appetite",5),
                                      "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                      bind_rows(deltaMethod(appetite.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(appetite.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(appetite.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(appetite.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                deltaMethod(appetite.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                      "p-value" = summary(appetite.qp.full)$coefficients[c(2:4,16,17),4])
appetite.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQappetite),
                   predict(appetite.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Appetite Dysregulation")+
  theme_pubr()+theme(legend.position = "none")

# Fatigue
tired.qp <- glm(PHQtired~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
tired.qp.maineff <- bind_cols("Symptom" = rep("Tired",5),
                              "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                              bind_rows(deltaMethod(tired.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                        deltaMethod(tired.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                        deltaMethod(tired.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                        deltaMethod(tired.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                        deltaMethod(tired.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                              "p-value" = summary(tired.qp)$coefficients[2:6,4])
tired.qp.full <- glm(PHQtired~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
tired.qp.full.maineff <- bind_cols("Symptom" = rep("Tired",5),
                                   "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                   bind_rows(deltaMethod(tired.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                             deltaMethod(tired.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                             deltaMethod(tired.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                             deltaMethod(tired.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                             deltaMethod(tired.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                   "p-value" = summary(tired.qp.full)$coefficients[c(2:4,16,17),4])
tired.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQtired),
                   predict(tired.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Fatigue")+
  theme_pubr()+theme(legend.position = "none")

# Difficulty Concentrating
concentrate.qp <- glm(PHQconcentrate~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
concentrate.qp.maineff <- bind_cols("Symptom" = rep("Concentrate",5),
                                    "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                    bind_rows(deltaMethod(concentrate.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(concentrate.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(concentrate.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(concentrate.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(concentrate.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                    "p-value" = summary(concentrate.qp)$coefficients[2:6,4])
concentrate.qp.full <- glm(PHQconcentrate~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
concentrate.qp.full.maineff <- bind_cols("Symptom" = rep("Concentrate",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(concentrate.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(concentrate.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(concentrate.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(concentrate.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(concentrate.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(concentrate.qp.full)$coefficients[c(2:4,16,17),4])
concentrate.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQconcentrate),
                   predict(concentrate.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Difficulty Concentrating")+
  theme_pubr()+theme(legend.position = "none")

# Feelings of Failure
failure.qp <- glm(PHQfailure~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
failure.qp.maineff <- bind_cols("Symptom" = rep("Failure",5),
                                "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                bind_rows(deltaMethod(failure.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(failure.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(failure.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(failure.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(failure.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                "p-value" = summary(failure.qp)$coefficients[2:6,4])
failure.qp.full <- glm(PHQfailure~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
failure.qp.full.maineff <- bind_cols("Symptom" = rep("Failure",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(failure.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(failure.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(failure.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(failure.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(failure.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(failure.qp.full)$coefficients[c(2:4,16,17),4])
failure.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQfailure),
                   predict(failure.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Feelings of Failure")+
  theme_pubr()+theme(legend.position = "none")

# Psychomotor Disturbance
psychomotor.qp <- glm(PHQpsychomotor~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
psychomotor.qp.maineff <- bind_cols("Symptom" = rep("Psychomotor",5),
                                    "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                    bind_rows(deltaMethod(psychomotor.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(psychomotor.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(psychomotor.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(psychomotor.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(psychomotor.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                    "p-value" = summary(psychomotor.qp)$coefficients[2:6,4])
psychomotor.qp.full <- glm(PHQpsychomotor~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
psychomotor.qp.full.maineff <- bind_cols("Symptom" = rep("Psychomotor",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(psychomotor.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(psychomotor.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(psychomotor.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(psychomotor.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(psychomotor.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(psychomotor.qp.full)$coefficients[c(2:4,16,17),4])
psychomotor.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQpsychomotor),
                   predict(psychomotor.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Psychomotor Disturbance")+
  theme_pubr()+theme(legend.position = "none")

# Suicidal behavior
suicide.qp <- glm(PHQsuicide~ISItotal*SleepDuration,phq9.data,family="quasipoisson")
suicide.qp.maineff <- bind_cols("Symptom" = rep("Suicide",5),
                                "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                bind_rows(deltaMethod(suicide.qp, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(suicide.qp, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(suicide.qp, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(suicide.qp, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                          deltaMethod(suicide.qp, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                "p-value" = summary(suicide.qp)$coefficients[2:6,4])
suicide.qp.full <- glm(PHQsuicide~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.data,family="quasipoisson")
suicide.qp.full.maineff <- bind_cols("Symptom" = rep("Suicide",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(suicide.qp.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(suicide.qp.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(suicide.qp.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(suicide.qp.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                               deltaMethod(suicide.qp.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(suicide.qp.full)$coefficients[c(2:4,16,17),4])
suicide.plot <- ggplot(
  data = bind_cols(phq9.data %>% select(ISItotal,SleepDuration,PHQsuicide),
                   predict(suicide.qp, type="response",se.fit=TRUE)),
  aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,
                  fill=SleepDuration),
              alpha=.25)+
  labs(x = "ISI Score",y="Suicidal Behavior")+
  theme_pubr()+theme(legend.position = "none")

#### Table 2: Sleep influence by symptom in unadjusted and adjusted models ####
phq9.symptom.summary <- bind_cols(
  bind_rows(phqscore.qp.maineff,interest.qp.maineff,depress.qp.maineff,appetite.qp.maineff,tired.qp.maineff,
            concentrate.qp.maineff,failure.qp.maineff,psychomotor.qp.maineff,suicide.qp.maineff) %>%
    transmute(Symptom = factor(Symptom,levels = c("Total","Interest","Depress","Appetite","Tired","Concentrate","Failure","Psychomotor","Suicide"),
                            labels = c("Adjusted PHQ-9 Score","Anhedonia","Depressed Mood","Appetite dysregulation","Fatigue",
                                       "Difficulty concentrating","Feelings of failure","Psychomotor disturbance","Suicidal behavior")),
           Parameter,
           "PR" = round(Estimate,2),
           "SE" = round(SE,2),
           "95% CI" = paste0("[",round(`2.5 %`,2),", ",round(`97.5 %`,2),"]"),
           "p-value" = round(`p-value`,4)),
  bind_rows(phqscore.qp.full.maineff,interest.qp.full.maineff,depress.qp.full.maineff,appetite.qp.full.maineff,tired.qp.full.maineff,
            concentrate.qp.full.maineff,failure.qp.full.maineff,psychomotor.qp.full.maineff,suicide.qp.full.maineff) %>%
    transmute("aPR" = round(Estimate,2),
              "aSE" = round(SE,2),
              "a95% CI" = paste0("[",round(`2.5 %`,2),", ",round(`97.5 %`,2),"]"),
              "ap-value" = round(`p-value`,4))
)
```

The results of these models are presented in the table below. The predictors of interest are ISI score, sleep duration, and their interaction. These are reported for each symptom and the overall PHQ-9 severity score (minus the sleep item due to collinearity). The adjusted model results are in the right half of the table with headers appended by “a”.

<div id="fmyfytfflh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fmyfytfflh .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fmyfytfflh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fmyfytfflh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fmyfytfflh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fmyfytfflh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fmyfytfflh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fmyfytfflh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fmyfytfflh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fmyfytfflh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fmyfytfflh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fmyfytfflh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fmyfytfflh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fmyfytfflh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fmyfytfflh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fmyfytfflh .gt_from_md > :first-child {
  margin-top: 0;
}

#fmyfytfflh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fmyfytfflh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fmyfytfflh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#fmyfytfflh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fmyfytfflh .gt_row_group_first td {
  border-top-width: 2px;
}

#fmyfytfflh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fmyfytfflh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fmyfytfflh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fmyfytfflh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fmyfytfflh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fmyfytfflh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fmyfytfflh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fmyfytfflh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fmyfytfflh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fmyfytfflh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fmyfytfflh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fmyfytfflh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fmyfytfflh .gt_left {
  text-align: left;
}

#fmyfytfflh .gt_center {
  text-align: center;
}

#fmyfytfflh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fmyfytfflh .gt_font_normal {
  font-weight: normal;
}

#fmyfytfflh .gt_font_bold {
  font-weight: bold;
}

#fmyfytfflh .gt_font_italic {
  font-style: italic;
}

#fmyfytfflh .gt_super {
  font-size: 65%;
}

#fmyfytfflh .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#fmyfytfflh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fmyfytfflh .gt_indent_1 {
  text-indent: 5px;
}

#fmyfytfflh .gt_indent_2 {
  text-indent: 10px;
}

#fmyfytfflh .gt_indent_3 {
  text-indent: 15px;
}

#fmyfytfflh .gt_indent_4 {
  text-indent: 20px;
}

#fmyfytfflh .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Symptom">Symptom</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Parameter">Parameter</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="PR">PR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SE">SE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="95% CI">95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aPR">aPR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aSE">aSE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a95% CI">a95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ap-value">ap-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.09</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.08, 1.11]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.09</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.08, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.52</td>
<td headers="SE" class="gt_row gt_right">0.16</td>
<td headers="95% CI" class="gt_row gt_left">[1.21, 1.83]</td>
<td headers="p-value" class="gt_row gt_right">0.0001</td>
<td headers="aPR" class="gt_row gt_right">1.48</td>
<td headers="aSE" class="gt_row gt_right">0.15</td>
<td headers="a95% CI" class="gt_row gt_left">[1.18, 1.78]</td>
<td headers="ap-value" class="gt_row gt_right">0.0002</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.40</td>
<td headers="SE" class="gt_row gt_right">0.31</td>
<td headers="95% CI" class="gt_row gt_left">[0.8, 2]</td>
<td headers="p-value" class="gt_row gt_right">0.1247</td>
<td headers="aPR" class="gt_row gt_right">1.35</td>
<td headers="aSE" class="gt_row gt_right">0.30</td>
<td headers="a95% CI" class="gt_row gt_left">[0.77, 1.93]</td>
<td headers="ap-value" class="gt_row gt_right">0.1708</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.97</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 0.98]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0001</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.99</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.4114</td>
<td headers="aPR" class="gt_row gt_right">0.99</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.4774</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.08</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.06, 1.1]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.08</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.06, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.08</td>
<td headers="SE" class="gt_row gt_right">0.17</td>
<td headers="95% CI" class="gt_row gt_left">[0.76, 1.41]</td>
<td headers="p-value" class="gt_row gt_right">0.6076</td>
<td headers="aPR" class="gt_row gt_right">1.07</td>
<td headers="aSE" class="gt_row gt_right">0.17</td>
<td headers="a95% CI" class="gt_row gt_left">[0.74, 1.39]</td>
<td headers="ap-value" class="gt_row gt_right">0.6766</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.16</td>
<td headers="SE" class="gt_row gt_right">0.37</td>
<td headers="95% CI" class="gt_row gt_left">[0.44, 1.88]</td>
<td headers="p-value" class="gt_row gt_right">0.6347</td>
<td headers="aPR" class="gt_row gt_right">1.15</td>
<td headers="aSE" class="gt_row gt_right">0.37</td>
<td headers="a95% CI" class="gt_row gt_left">[0.43, 1.86]</td>
<td headers="ap-value" class="gt_row gt_right">0.6710</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.99</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.97, 1.01]</td>
<td headers="p-value" class="gt_row gt_right">0.2291</td>
<td headers="aPR" class="gt_row gt_right">0.99</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.97, 1.01]</td>
<td headers="ap-value" class="gt_row gt_right">0.2924</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.01</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.06]</td>
<td headers="p-value" class="gt_row gt_right">0.6572</td>
<td headers="aPR" class="gt_row gt_right">1.01</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 1.06]</td>
<td headers="ap-value" class="gt_row gt_right">0.6143</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.09</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.07, 1.1]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.08</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.07, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.32</td>
<td headers="SE" class="gt_row gt_right">0.18</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.68]</td>
<td headers="p-value" class="gt_row gt_right">0.0430</td>
<td headers="aPR" class="gt_row gt_right">1.29</td>
<td headers="aSE" class="gt_row gt_right">0.18</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 1.64]</td>
<td headers="ap-value" class="gt_row gt_right">0.0699</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.32</td>
<td headers="SE" class="gt_row gt_right">0.38</td>
<td headers="95% CI" class="gt_row gt_left">[0.57, 2.07]</td>
<td headers="p-value" class="gt_row gt_right">0.3342</td>
<td headers="aPR" class="gt_row gt_right">1.31</td>
<td headers="aSE" class="gt_row gt_right">0.38</td>
<td headers="a95% CI" class="gt_row gt_left">[0.56, 2.06]</td>
<td headers="ap-value" class="gt_row gt_right">0.3546</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.97</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="p-value" class="gt_row gt_right">0.0033</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0096</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.99</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.6561</td>
<td headers="aPR" class="gt_row gt_right">0.99</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.6471</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.09</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.07, 1.11]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.09</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.07, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.86</td>
<td headers="SE" class="gt_row gt_right">0.26</td>
<td headers="95% CI" class="gt_row gt_left">[1.34, 2.38]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.82</td>
<td headers="aSE" class="gt_row gt_right">0.26</td>
<td headers="a95% CI" class="gt_row gt_left">[1.31, 2.33]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.89</td>
<td headers="SE" class="gt_row gt_right">0.55</td>
<td headers="95% CI" class="gt_row gt_left">[0.81, 2.97]</td>
<td headers="p-value" class="gt_row gt_right">0.0291</td>
<td headers="aPR" class="gt_row gt_right">1.76</td>
<td headers="aSE" class="gt_row gt_right">0.51</td>
<td headers="a95% CI" class="gt_row gt_left">[0.76, 2.76]</td>
<td headers="ap-value" class="gt_row gt_right">0.0508</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.96</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.94, 0.98]</td>
<td headers="p-value" class="gt_row gt_right">0.0004</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0009</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.96</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.92, 1.01]</td>
<td headers="p-value" class="gt_row gt_right">0.1135</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.92, 1.01]</td>
<td headers="ap-value" class="gt_row gt_right">0.1486</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.08</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.07, 1.09]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.08</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.07, 1.09]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.51</td>
<td headers="SE" class="gt_row gt_right">0.14</td>
<td headers="95% CI" class="gt_row gt_left">[1.22, 1.79]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.49</td>
<td headers="aSE" class="gt_row gt_right">0.14</td>
<td headers="a95% CI" class="gt_row gt_left">[1.21, 1.77]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.16</td>
<td headers="SE" class="gt_row gt_right">0.25</td>
<td headers="95% CI" class="gt_row gt_left">[0.68, 1.65]</td>
<td headers="p-value" class="gt_row gt_right">0.4775</td>
<td headers="aPR" class="gt_row gt_right">1.13</td>
<td headers="aSE" class="gt_row gt_right">0.24</td>
<td headers="a95% CI" class="gt_row gt_left">[0.66, 1.6]</td>
<td headers="ap-value" class="gt_row gt_right">0.5616</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.97</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 0.98]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0001</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.99</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.6999</td>
<td headers="aPR" class="gt_row gt_right">0.99</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.7458</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.10</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.08, 1.12]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.09</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.07, 1.11]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.61</td>
<td headers="SE" class="gt_row gt_right">0.26</td>
<td headers="95% CI" class="gt_row gt_left">[1.1, 2.12]</td>
<td headers="p-value" class="gt_row gt_right">0.0031</td>
<td headers="aPR" class="gt_row gt_right">1.58</td>
<td headers="aSE" class="gt_row gt_right">0.26</td>
<td headers="a95% CI" class="gt_row gt_left">[1.08, 2.09]</td>
<td headers="ap-value" class="gt_row gt_right">0.0049</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.23</td>
<td headers="SE" class="gt_row gt_right">0.45</td>
<td headers="95% CI" class="gt_row gt_left">[0.34, 2.12]</td>
<td headers="p-value" class="gt_row gt_right">0.5797</td>
<td headers="aPR" class="gt_row gt_right">1.13</td>
<td headers="aSE" class="gt_row gt_right">0.42</td>
<td headers="a95% CI" class="gt_row gt_left">[0.31, 1.94]</td>
<td headers="ap-value" class="gt_row gt_right">0.7432</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.96</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.94, 0.99]</td>
<td headers="p-value" class="gt_row gt_right">0.0013</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0064</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.98</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.5120</td>
<td headers="aPR" class="gt_row gt_right">0.98</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[0.93, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.5859</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.08</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.07, 1.1]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.08</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.06, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.58</td>
<td headers="SE" class="gt_row gt_right">0.23</td>
<td headers="95% CI" class="gt_row gt_left">[1.12, 2.04]</td>
<td headers="p-value" class="gt_row gt_right">0.0020</td>
<td headers="aPR" class="gt_row gt_right">1.54</td>
<td headers="aSE" class="gt_row gt_right">0.23</td>
<td headers="a95% CI" class="gt_row gt_left">[1.09, 1.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0040</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.45</td>
<td headers="SE" class="gt_row gt_right">0.44</td>
<td headers="95% CI" class="gt_row gt_left">[0.58, 2.32]</td>
<td headers="p-value" class="gt_row gt_right">0.2256</td>
<td headers="aPR" class="gt_row gt_right">1.43</td>
<td headers="aSE" class="gt_row gt_right">0.44</td>
<td headers="a95% CI" class="gt_row gt_left">[0.57, 2.3]</td>
<td headers="ap-value" class="gt_row gt_right">0.2404</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.97</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="p-value" class="gt_row gt_right">0.0018</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0047</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.99</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.94, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.6448</td>
<td headers="aPR" class="gt_row gt_right">0.99</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.6494</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.13</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1.1, 1.17]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.12</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[1.09, 1.15]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.74</td>
<td headers="SE" class="gt_row gt_right">0.44</td>
<td headers="95% CI" class="gt_row gt_left">[0.87, 2.61]</td>
<td headers="p-value" class="gt_row gt_right">0.0298</td>
<td headers="aPR" class="gt_row gt_right">1.50</td>
<td headers="aSE" class="gt_row gt_right">0.38</td>
<td headers="a95% CI" class="gt_row gt_left">[0.76, 2.24]</td>
<td headers="ap-value" class="gt_row gt_right">0.1078</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.65</td>
<td headers="SE" class="gt_row gt_right">0.94</td>
<td headers="95% CI" class="gt_row gt_left">[-0.2, 3.5]</td>
<td headers="p-value" class="gt_row gt_right">0.3821</td>
<td headers="aPR" class="gt_row gt_right">1.46</td>
<td headers="aSE" class="gt_row gt_right">0.83</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.16, 3.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.5075</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.96</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 0.99]</td>
<td headers="p-value" class="gt_row gt_right">0.0176</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0761</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.96</td>
<td headers="SE" class="gt_row gt_right">0.04</td>
<td headers="95% CI" class="gt_row gt_left">[0.88, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.3819</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.04</td>
<td headers="a95% CI" class="gt_row gt_left">[0.89, 1.06]</td>
<td headers="ap-value" class="gt_row gt_right">0.5552</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.16</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1.12, 1.2]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.15</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[1.11, 1.19]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">2.54</td>
<td headers="SE" class="gt_row gt_right">0.87</td>
<td headers="95% CI" class="gt_row gt_left">[0.83, 4.26]</td>
<td headers="p-value" class="gt_row gt_right">0.0068</td>
<td headers="aPR" class="gt_row gt_right">2.38</td>
<td headers="aSE" class="gt_row gt_right">0.81</td>
<td headers="a95% CI" class="gt_row gt_left">[0.79, 3.96]</td>
<td headers="ap-value" class="gt_row gt_right">0.0112</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">2.75</td>
<td headers="SE" class="gt_row gt_right">1.78</td>
<td headers="95% CI" class="gt_row gt_left">[-0.74, 6.24]</td>
<td headers="p-value" class="gt_row gt_right">0.1193</td>
<td headers="aPR" class="gt_row gt_right">2.75</td>
<td headers="aSE" class="gt_row gt_right">1.80</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.77, 6.27]</td>
<td headers="ap-value" class="gt_row gt_right">0.1213</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.94</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.89, 0.98]</td>
<td headers="p-value" class="gt_row gt_right">0.0041</td>
<td headers="aPR" class="gt_row gt_right">0.94</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.89, 0.98]</td>
<td headers="ap-value" class="gt_row gt_right">0.0046</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.96</td>
<td headers="SE" class="gt_row gt_right">0.05</td>
<td headers="95% CI" class="gt_row gt_left">[0.87, 1.05]</td>
<td headers="p-value" class="gt_row gt_right">0.4221</td>
<td headers="aPR" class="gt_row gt_right">0.97</td>
<td headers="aSE" class="gt_row gt_right">0.05</td>
<td headers="a95% CI" class="gt_row gt_left">[0.87, 1.06]</td>
<td headers="ap-value" class="gt_row gt_right">0.4555</td></tr>
  </tbody>
  
  
</table>
</div>

Figure 2 below illustrates how changes in ISI score are related to adjusted PHQ-9 score by sleep duration category.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

## Model Depressed Subjects Only

Let’s look at only the N=390 individuals with clinically significant depression symptoms (PHQ-9 \> 10).

``` r
#### Analysis 2: Depressed Subjects Only ####
## Total Score
# Unadjusted
phq9.subset <- phq9.data[phq9.data$PHQtotal>9,]
phqscore.qp.sub <- glm(PHQtotal2~ISItotal*SleepDuration,family="quasipoisson",phq9.subset)
phqscore.qp.sub.maineff <- bind_cols("Symptom" = rep("Total",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(phqscore.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(phqscore.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(phqscore.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(phqscore.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(phqscore.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(phqscore.qp.sub)$coefficients[2:6,4])
# Adjusted for Age, Sex, Race, Education, and BMI
phqscore.qp.sub.full <- glm(PHQtotal2~ISItotal*SleepDuration+Age+Sex+Race+Education+BMI,family="quasipoisson",phq9.subset)
phqscore.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Total",5),
                                          "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                          bind_rows(deltaMethod(phqscore.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(phqscore.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(phqscore.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(phqscore.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(phqscore.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                          "p-value" = summary(phqscore.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict <- predict(phqscore.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
phqscore.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  geom_point(aes(y = PHQtotal2),position="jitter")+
  labs(x = "ISI Score",y="Adjusted PHQ - 9 Score")+
  theme_pubr()+theme(legend.title = element_blank())

## Individual Symptoms
# Anhedonia
interest.qp.sub <- glm(PHQinterest~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
interest.qp.sub.maineff <- bind_cols("Symptom" = rep("Interest",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(interest.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(interest.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(interest.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(interest.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(interest.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(interest.qp.sub)$coefficients[2:6,4])
interest.qp.sub.full <- glm(PHQinterest~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
interest.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Interest",5),
                                          "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                          bind_rows(deltaMethod(interest.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(interest.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(interest.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(interest.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(interest.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                          "p-value" = summary(interest.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(interest.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
interest.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Anhedonia")+
  theme_pubr()+theme(legend.position = "none")

# Depressed Mood
depress.qp.sub <- glm(PHQdepress~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
depress.qp.sub.maineff <- bind_cols("Symptom" = rep("Depress",5),
                                    "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                    bind_rows(deltaMethod(depress.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(depress.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(depress.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(depress.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(depress.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                    "p-value" = summary(depress.qp.sub)$coefficients[2:6,4])
depress.qp.sub.full <- glm(PHQdepress~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
depress.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Depress",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(depress.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(depress.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(depress.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(depress.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(depress.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(depress.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(depress.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
depress.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Depressed Mood")+
  theme_pubr()+theme(legend.position = "none")

# Appetite
appetite.qp.sub <- glm(PHQappetite~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
appetite.qp.sub.maineff <- bind_cols("Symptom" = rep("Appetite",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(appetite.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(appetite.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(appetite.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(appetite.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(appetite.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(appetite.qp.sub)$coefficients[2:6,4])
appetite.qp.sub.full <- glm(PHQappetite~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
appetite.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Appetite",5),
                                          "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                          bind_rows(deltaMethod(appetite.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(appetite.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(appetite.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(appetite.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(appetite.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                          "p-value" = summary(appetite.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(appetite.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
appetite.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Appetite Dysregulation")+
  theme_pubr()+theme(legend.position = "none")

# Fatigue
tired.qp.sub <- glm(PHQtired~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
tired.qp.sub.maineff <- bind_cols("Symptom" = rep("Tired",5),
                                  "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                  bind_rows(deltaMethod(tired.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                            deltaMethod(tired.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                            deltaMethod(tired.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                            deltaMethod(tired.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                            deltaMethod(tired.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                  "p-value" = summary(tired.qp.sub)$coefficients[2:6,4])
tired.qp.sub.full <- glm(PHQtired~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
tired.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Tired",5),
                                       "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                       bind_rows(deltaMethod(tired.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                 deltaMethod(tired.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                 deltaMethod(tired.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                 deltaMethod(tired.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                 deltaMethod(tired.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                       "p-value" = summary(tired.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(tired.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
tired.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Fatigue")+
  theme_pubr()+theme(legend.position = "none")

# Difficulty Concentrating
concentrate.qp.sub <- glm(PHQconcentrate~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
concentrate.qp.sub.maineff <- bind_cols("Symptom" = rep("Concentrate",5),
                                        "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                        bind_rows(deltaMethod(concentrate.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(concentrate.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(concentrate.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(concentrate.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(concentrate.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                        "p-value" = summary(concentrate.qp.sub)$coefficients[2:6,4])
concentrate.qp.sub.full <- glm(PHQconcentrate~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
concentrate.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Concentrate",5),
                                             "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                             bind_rows(deltaMethod(concentrate.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(concentrate.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(concentrate.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(concentrate.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(concentrate.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                             "p-value" = summary(concentrate.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(concentrate.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
concentrate.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Difficulty Concentrating")+
  theme_pubr()+theme(legend.position = "none")

# Feelings of Failure
failure.qp.sub <- glm(PHQfailure~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
failure.qp.sub.maineff <- bind_cols("Symptom" = rep("Failure",5),
                                    "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                    bind_rows(deltaMethod(failure.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(failure.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(failure.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(failure.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(failure.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                    "p-value" = summary(failure.qp.sub)$coefficients[2:6,4])
failure.qp.sub.full <- glm(PHQfailure~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
failure.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Failure",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(failure.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(failure.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(failure.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(failure.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(failure.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(failure.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(failure.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
failure.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Feelings of Failure")+
  theme_pubr()+theme(legend.position = "none")

# Psychomotor Disturbance
psychomotor.qp.sub <- glm(PHQpsychomotor~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
psychomotor.qp.sub.maineff <- bind_cols("Symptom" = rep("Psychomotor",5),
                                        "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                        bind_rows(deltaMethod(psychomotor.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(psychomotor.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(psychomotor.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(psychomotor.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                  deltaMethod(psychomotor.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                        "p-value" = summary(psychomotor.qp.sub)$coefficients[2:6,4])
psychomotor.qp.sub.full <- glm(PHQpsychomotor~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
psychomotor.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Psychomotor",5),
                                             "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                             bind_rows(deltaMethod(psychomotor.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(psychomotor.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(psychomotor.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(psychomotor.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                       deltaMethod(psychomotor.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                             "p-value" = summary(psychomotor.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(psychomotor.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
psychomotor.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Psychomotor Disturbance")+
  theme_pubr()+theme(legend.position = "none")

# Suicidal behavior
suicide.qp.sub <- glm(PHQsuicide~ISItotal*SleepDuration,phq9.subset,family="quasipoisson")
suicide.qp.sub.maineff <- bind_cols("Symptom" = rep("Suicide",5),
                                    "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                    bind_rows(deltaMethod(suicide.qp.sub, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(suicide.qp.sub, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(suicide.qp.sub, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(suicide.qp.sub, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                              deltaMethod(suicide.qp.sub, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                    "p-value" = summary(suicide.qp.sub)$coefficients[2:6,4])
suicide.qp.sub.full <- glm(PHQsuicide~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset,family="quasipoisson")
suicide.qp.sub.full.maineff <- bind_cols("Symptom" = rep("Suicide",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(suicide.qp.sub.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(suicide.qp.sub.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(suicide.qp.sub.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(suicide.qp.sub.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                   deltaMethod(suicide.qp.sub.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(suicide.qp.sub.full)$coefficients[c(2:4,16,17),4])
phq9.subset.predict<- predict(suicide.qp.sub, type="response",se.fit=TRUE)
phq9.subset$fit <- as.numeric(phq9.subset.predict$fit)
phq9.subset$se.fit <- as.numeric(phq9.subset.predict$se.fit)
suicide.sub.plot <- ggplot(phq9.subset, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Suicidal Behavior")+
  theme_pubr()+theme(legend.position = "none")

#### Table 3: Sleep influence by symptom in unadjusted/adjusted models for depressed only ####
phq9.symptom.sub.summary <- bind_cols(
  bind_rows(phqscore.qp.sub.maineff,interest.qp.sub.maineff,depress.qp.sub.maineff,appetite.qp.sub.maineff,tired.qp.sub.maineff,
            concentrate.qp.sub.maineff,failure.qp.sub.maineff,psychomotor.qp.sub.maineff,suicide.qp.sub.maineff) %>%
    transmute(Symptom = factor(Symptom,levels = c("Total","Interest","Depress","Appetite","Tired","Concentrate","Failure","Psychomotor","Suicide"),
                               labels = c("Adjusted PHQ-9 Score","Anhedonia","Depressed Mood","Appetite dysregulation","Fatigue",
                                          "Difficulty concentrating","Feelings of failure","Psychomotor disturbance","Suicidal behavior")),
              Parameter,
              "PR" = Estimate,
              "SE" = round(SE,2),
              "95% CI" = paste0("[",round(`2.5 %`,2),", ",round(`97.5 %`,2),"]"),
              "p-value" = round(`p-value`,4)),
  bind_rows(phqscore.qp.sub.full.maineff,interest.qp.sub.full.maineff,depress.qp.sub.full.maineff,appetite.qp.sub.full.maineff,
            tired.qp.sub.full.maineff,concentrate.qp.sub.full.maineff,failure.qp.sub.full.maineff,psychomotor.qp.sub.full.maineff,
            suicide.qp.sub.full.maineff) %>%
    transmute("aPR" = Estimate,
              "aSE" = round(SE,2),
              "a95% CI" = paste0("[",round(`2.5 %`,2),", ",round(`97.5 %`,2),"]"),
              "ap-value" = round(`p-value`,4))
)
```

The table below shows their model results (as with the whole sample).

``` r
gt(phq9.symptom.sub.summary)
```

<div id="kftiyadzov" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kftiyadzov .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kftiyadzov .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kftiyadzov .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#kftiyadzov .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kftiyadzov .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kftiyadzov .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kftiyadzov .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kftiyadzov .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kftiyadzov .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kftiyadzov .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kftiyadzov .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kftiyadzov .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kftiyadzov .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#kftiyadzov .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kftiyadzov .gt_from_md > :first-child {
  margin-top: 0;
}

#kftiyadzov .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kftiyadzov .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kftiyadzov .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#kftiyadzov .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#kftiyadzov .gt_row_group_first td {
  border-top-width: 2px;
}

#kftiyadzov .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kftiyadzov .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#kftiyadzov .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#kftiyadzov .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kftiyadzov .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kftiyadzov .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kftiyadzov .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kftiyadzov .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kftiyadzov .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kftiyadzov .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kftiyadzov .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kftiyadzov .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kftiyadzov .gt_left {
  text-align: left;
}

#kftiyadzov .gt_center {
  text-align: center;
}

#kftiyadzov .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kftiyadzov .gt_font_normal {
  font-weight: normal;
}

#kftiyadzov .gt_font_bold {
  font-weight: bold;
}

#kftiyadzov .gt_font_italic {
  font-style: italic;
}

#kftiyadzov .gt_super {
  font-size: 65%;
}

#kftiyadzov .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#kftiyadzov .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#kftiyadzov .gt_indent_1 {
  text-indent: 5px;
}

#kftiyadzov .gt_indent_2 {
  text-indent: 10px;
}

#kftiyadzov .gt_indent_3 {
  text-indent: 15px;
}

#kftiyadzov .gt_indent_4 {
  text-indent: 20px;
}

#kftiyadzov .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Symptom">Symptom</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Parameter">Parameter</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="PR">PR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SE">SE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="95% CI">95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aPR">aPR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aSE">aSE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a95% CI">a95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ap-value">ap-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0178209</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.01, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.0013</td>
<td headers="aPR" class="gt_row gt_right">1.0161896</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.01, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.0038</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9415744</td>
<td headers="SE" class="gt_row gt_right">0.09</td>
<td headers="95% CI" class="gt_row gt_left">[0.77, 1.12]</td>
<td headers="p-value" class="gt_row gt_right">0.5225</td>
<td headers="aPR" class="gt_row gt_right">0.9304414</td>
<td headers="aSE" class="gt_row gt_right">0.09</td>
<td headers="a95% CI" class="gt_row gt_left">[0.76, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.4512</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.2831456</td>
<td headers="SE" class="gt_row gt_right">0.25</td>
<td headers="95% CI" class="gt_row gt_left">[0.79, 1.77]</td>
<td headers="p-value" class="gt_row gt_right">0.2002</td>
<td headers="aPR" class="gt_row gt_right">1.2820901</td>
<td headers="aSE" class="gt_row gt_right">0.25</td>
<td headers="a95% CI" class="gt_row gt_left">[0.79, 1.78]</td>
<td headers="ap-value" class="gt_row gt_right">0.2062</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0023778</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.99, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.7119</td>
<td headers="aPR" class="gt_row gt_right">1.0038357</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.5588</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9917789</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.5808</td>
<td headers="aPR" class="gt_row gt_right">0.9918640</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.5895</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0035278</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.98, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.7128</td>
<td headers="aPR" class="gt_row gt_right">1.0045149</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.6453</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.6228047</td>
<td headers="SE" class="gt_row gt_right">0.10</td>
<td headers="95% CI" class="gt_row gt_left">[0.42, 0.83]</td>
<td headers="p-value" class="gt_row gt_right">0.0046</td>
<td headers="aPR" class="gt_row gt_right">0.6395063</td>
<td headers="aSE" class="gt_row gt_right">0.11</td>
<td headers="a95% CI" class="gt_row gt_left">[0.43, 0.85]</td>
<td headers="ap-value" class="gt_row gt_right">0.0090</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0334275</td>
<td headers="SE" class="gt_row gt_right">0.34</td>
<td headers="95% CI" class="gt_row gt_left">[0.36, 1.7]</td>
<td headers="p-value" class="gt_row gt_right">0.9209</td>
<td headers="aPR" class="gt_row gt_right">1.0578502</td>
<td headers="aSE" class="gt_row gt_right">0.36</td>
<td headers="a95% CI" class="gt_row gt_left">[0.36, 1.76]</td>
<td headers="ap-value" class="gt_row gt_right">0.8680</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0276950</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1, 1.05]</td>
<td headers="p-value" class="gt_row gt_right">0.0169</td>
<td headers="aPR" class="gt_row gt_right">1.0263169</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1, 1.05]</td>
<td headers="ap-value" class="gt_row gt_right">0.0269</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0179864</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.97, 1.07]</td>
<td headers="p-value" class="gt_row gt_right">0.4791</td>
<td headers="aPR" class="gt_row gt_right">1.0174797</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[0.97, 1.07]</td>
<td headers="ap-value" class="gt_row gt_right">0.5023</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0207236</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.0243</td>
<td headers="aPR" class="gt_row gt_right">1.0212582</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.0244</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.8370318</td>
<td headers="SE" class="gt_row gt_right">0.13</td>
<td headers="95% CI" class="gt_row gt_left">[0.57, 1.1]</td>
<td headers="p-value" class="gt_row gt_right">0.2707</td>
<td headers="aPR" class="gt_row gt_right">0.8319131</td>
<td headers="aSE" class="gt_row gt_right">0.14</td>
<td headers="a95% CI" class="gt_row gt_left">[0.56, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.2669</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.5010144</td>
<td headers="SE" class="gt_row gt_right">0.48</td>
<td headers="95% CI" class="gt_row gt_left">[0.56, 2.44]</td>
<td headers="p-value" class="gt_row gt_right">0.2049</td>
<td headers="aPR" class="gt_row gt_right">1.5972814</td>
<td headers="aSE" class="gt_row gt_right">0.53</td>
<td headers="a95% CI" class="gt_row gt_left">[0.57, 2.63]</td>
<td headers="ap-value" class="gt_row gt_right">0.1553</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0022262</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.98, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.8383</td>
<td headers="aPR" class="gt_row gt_right">1.0027899</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.98, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.8038</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9785476</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.3841</td>
<td headers="aPR" class="gt_row gt_right">0.9736618</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.92, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.2992</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0239255</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.0203</td>
<td headers="aPR" class="gt_row gt_right">1.0214125</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.0400</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.1797149</td>
<td headers="SE" class="gt_row gt_right">0.20</td>
<td headers="95% CI" class="gt_row gt_left">[0.78, 1.58]</td>
<td headers="p-value" class="gt_row gt_right">0.3388</td>
<td headers="aPR" class="gt_row gt_right">1.1016521</td>
<td headers="aSE" class="gt_row gt_right">0.19</td>
<td headers="a95% CI" class="gt_row gt_left">[0.72, 1.48]</td>
<td headers="ap-value" class="gt_row gt_right">0.5811</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.9647520</td>
<td headers="SE" class="gt_row gt_right">0.68</td>
<td headers="95% CI" class="gt_row gt_left">[0.63, 3.3]</td>
<td headers="p-value" class="gt_row gt_right">0.0529</td>
<td headers="aPR" class="gt_row gt_right">1.8543470</td>
<td headers="aSE" class="gt_row gt_right">0.65</td>
<td headers="a95% CI" class="gt_row gt_left">[0.59, 3.12]</td>
<td headers="ap-value" class="gt_row gt_right">0.0773</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9935221</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.97, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.5825</td>
<td headers="aPR" class="gt_row gt_right">0.9968360</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.97, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.7921</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9567349</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.9, 1.01]</td>
<td headers="p-value" class="gt_row gt_right">0.1111</td>
<td headers="aPR" class="gt_row gt_right">0.9566688</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[0.9, 1.01]</td>
<td headers="ap-value" class="gt_row gt_right">0.1121</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0180811</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.0103</td>
<td headers="aPR" class="gt_row gt_right">1.0200161</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.01, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.0051</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0142583</td>
<td headers="SE" class="gt_row gt_right">0.12</td>
<td headers="95% CI" class="gt_row gt_left">[0.78, 1.25]</td>
<td headers="p-value" class="gt_row gt_right">0.9056</td>
<td headers="aPR" class="gt_row gt_right">0.9865207</td>
<td headers="aSE" class="gt_row gt_right">0.12</td>
<td headers="a95% CI" class="gt_row gt_left">[0.75, 1.22]</td>
<td headers="ap-value" class="gt_row gt_right">0.9105</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9923559</td>
<td headers="SE" class="gt_row gt_right">0.26</td>
<td headers="95% CI" class="gt_row gt_left">[0.49, 1.5]</td>
<td headers="p-value" class="gt_row gt_right">0.9765</td>
<td headers="aPR" class="gt_row gt_right">0.9950127</td>
<td headers="aSE" class="gt_row gt_right">0.26</td>
<td headers="a95% CI" class="gt_row gt_left">[0.49, 1.5]</td>
<td headers="ap-value" class="gt_row gt_right">0.9846</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9995985</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.98, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.9608</td>
<td headers="aPR" class="gt_row gt_right">1.0022814</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.7834</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0094111</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.97, 1.05]</td>
<td headers="p-value" class="gt_row gt_right">0.6328</td>
<td headers="aPR" class="gt_row gt_right">1.0062077</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.97, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.7520</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0104140</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.99, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.3482</td>
<td headers="aPR" class="gt_row gt_right">1.0094975</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.3993</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9278503</td>
<td headers="SE" class="gt_row gt_right">0.18</td>
<td headers="95% CI" class="gt_row gt_left">[0.58, 1.27]</td>
<td headers="p-value" class="gt_row gt_right">0.6917</td>
<td headers="aPR" class="gt_row gt_right">0.8792638</td>
<td headers="aSE" class="gt_row gt_right">0.17</td>
<td headers="a95% CI" class="gt_row gt_left">[0.55, 1.21]</td>
<td headers="ap-value" class="gt_row gt_right">0.5035</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.2039904</td>
<td headers="SE" class="gt_row gt_right">0.50</td>
<td headers="95% CI" class="gt_row gt_left">[0.23, 2.18]</td>
<td headers="p-value" class="gt_row gt_right">0.6522</td>
<td headers="aPR" class="gt_row gt_right">1.0720115</td>
<td headers="aSE" class="gt_row gt_right">0.44</td>
<td headers="a95% CI" class="gt_row gt_left">[0.21, 1.93]</td>
<td headers="ap-value" class="gt_row gt_right">0.8656</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0048446</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.98, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.7108</td>
<td headers="aPR" class="gt_row gt_right">1.0095512</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.98, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.4747</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9832799</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.92, 1.05]</td>
<td headers="p-value" class="gt_row gt_right">0.6023</td>
<td headers="aPR" class="gt_row gt_right">0.9889700</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[0.93, 1.05]</td>
<td headers="ap-value" class="gt_row gt_right">0.7312</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0093809</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.99, 1.03]</td>
<td headers="p-value" class="gt_row gt_right">0.2936</td>
<td headers="aPR" class="gt_row gt_right">1.0085071</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.3495</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0274385</td>
<td headers="SE" class="gt_row gt_right">0.16</td>
<td headers="95% CI" class="gt_row gt_left">[0.72, 1.33]</td>
<td headers="p-value" class="gt_row gt_right">0.8588</td>
<td headers="aPR" class="gt_row gt_right">1.0298102</td>
<td headers="aSE" class="gt_row gt_right">0.16</td>
<td headers="a95% CI" class="gt_row gt_left">[0.72, 1.34]</td>
<td headers="ap-value" class="gt_row gt_right">0.8502</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.3028895</td>
<td headers="SE" class="gt_row gt_right">0.40</td>
<td headers="95% CI" class="gt_row gt_left">[0.52, 2.09]</td>
<td headers="p-value" class="gt_row gt_right">0.3896</td>
<td headers="aPR" class="gt_row gt_right">1.3473157</td>
<td headers="aSE" class="gt_row gt_right">0.42</td>
<td headers="a95% CI" class="gt_row gt_left">[0.52, 2.18]</td>
<td headers="ap-value" class="gt_row gt_right">0.3440</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9960146</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.98, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.7051</td>
<td headers="aPR" class="gt_row gt_right">0.9955016</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.97, 1.02]</td>
<td headers="ap-value" class="gt_row gt_right">0.6764</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9938423</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.95, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.7957</td>
<td headers="aPR" class="gt_row gt_right">0.9903489</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.6930</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0336489</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1, 1.07]</td>
<td headers="p-value" class="gt_row gt_right">0.0694</td>
<td headers="aPR" class="gt_row gt_right">1.0212463</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.06]</td>
<td headers="ap-value" class="gt_row gt_right">0.2448</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9574616</td>
<td headers="SE" class="gt_row gt_right">0.30</td>
<td headers="95% CI" class="gt_row gt_left">[0.36, 1.55]</td>
<td headers="p-value" class="gt_row gt_right">0.8909</td>
<td headers="aPR" class="gt_row gt_right">0.9233209</td>
<td headers="aSE" class="gt_row gt_right">0.29</td>
<td headers="a95% CI" class="gt_row gt_left">[0.35, 1.5]</td>
<td headers="ap-value" class="gt_row gt_right">0.8016</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.4870082</td>
<td headers="SE" class="gt_row gt_right">1.02</td>
<td headers="95% CI" class="gt_row gt_left">[-0.52, 3.49]</td>
<td headers="p-value" class="gt_row gt_right">0.5650</td>
<td headers="aPR" class="gt_row gt_right">1.3809087</td>
<td headers="aSE" class="gt_row gt_right">0.95</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.47, 3.23]</td>
<td headers="ap-value" class="gt_row gt_right">0.6377</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0035784</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.05]</td>
<td headers="p-value" class="gt_row gt_right">0.8663</td>
<td headers="aPR" class="gt_row gt_right">1.0108569</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.97, 1.05]</td>
<td headers="ap-value" class="gt_row gt_right">0.6121</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9674465</td>
<td headers="SE" class="gt_row gt_right">0.05</td>
<td headers="95% CI" class="gt_row gt_left">[0.87, 1.07]</td>
<td headers="p-value" class="gt_row gt_right">0.5359</td>
<td headers="aPR" class="gt_row gt_right">0.9825010</td>
<td headers="aSE" class="gt_row gt_right">0.05</td>
<td headers="a95% CI" class="gt_row gt_left">[0.88, 1.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.7396</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0453295</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1, 1.09]</td>
<td headers="p-value" class="gt_row gt_right">0.0540</td>
<td headers="aPR" class="gt_row gt_right">1.0355107</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.99, 1.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.1286</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.2183004</td>
<td headers="SE" class="gt_row gt_right">0.49</td>
<td headers="95% CI" class="gt_row gt_left">[0.25, 2.19]</td>
<td headers="p-value" class="gt_row gt_right">0.6262</td>
<td headers="aPR" class="gt_row gt_right">1.3643363</td>
<td headers="aSE" class="gt_row gt_right">0.55</td>
<td headers="a95% CI" class="gt_row gt_left">[0.28, 2.45]</td>
<td headers="ap-value" class="gt_row gt_right">0.4428</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0787691</td>
<td headers="SE" class="gt_row gt_right">0.89</td>
<td headers="95% CI" class="gt_row gt_left">[-0.66, 2.82]</td>
<td headers="p-value" class="gt_row gt_right">0.9266</td>
<td headers="aPR" class="gt_row gt_right">1.2844599</td>
<td headers="aSE" class="gt_row gt_right">1.08</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.83, 3.4]</td>
<td headers="ap-value" class="gt_row gt_right">0.7656</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9841912</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.5538</td>
<td headers="aPR" class="gt_row gt_right">0.9799659</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[0.93, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.4527</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0245926</td>
<td headers="SE" class="gt_row gt_right">0.06</td>
<td headers="95% CI" class="gt_row gt_left">[0.91, 1.14]</td>
<td headers="p-value" class="gt_row gt_right">0.6807</td>
<td headers="aPR" class="gt_row gt_right">1.0246740</td>
<td headers="aSE" class="gt_row gt_right">0.06</td>
<td headers="a95% CI" class="gt_row gt_left">[0.9, 1.15]</td>
<td headers="ap-value" class="gt_row gt_right">0.6866</td></tr>
  </tbody>
  
  
</table>
</div>

The figure below shows how changing ISI score is related to adjusted PHQ-9 score by sleep duration in this group.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## Model non-depressed subjects

Finally, let’s model the data for N=617 non-depressed individuals.

``` r
#### Analysis 3: Non-depressed Subjects Only ####
## Full Score
# Unadjusted
phq9.subset1 <- phq9.data[phq9.data$PHQtotal<10,]
phqscore.qp.sub1 <- glm(PHQtotal2~ISItotal*SleepDuration,family="quasipoisson",phq9.subset1)
phqscore.qp.sub1.maineff <- bind_cols("Symptom" = rep("Total",5),
                                      "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                      bind_rows(deltaMethod(phqscore.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(phqscore.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(phqscore.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(phqscore.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(phqscore.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                      "p-value" = summary(phqscore.qp.sub1)$coefficients[2:6,4])
# Adjusted for Age, Sex, Race, Education, and BMI
phqscore.qp.sub1.full <- glm(PHQtotal2~ISItotal*SleepDuration+Age+Sex+Race+Education+BMI,family="quasipoisson",phq9.subset1)
phqscore.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Total",5),
                                           "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                           bind_rows(deltaMethod(phqscore.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(phqscore.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(phqscore.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(phqscore.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(phqscore.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                           "p-value" = summary(phqscore.qp.sub1.full)$coefficients[c(2:4,16,17),4])
# Plot
phq9.subset1.predict <- predict(phqscore.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
phqscore.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  geom_point(aes(y = PHQtotal2),position="jitter")+
  labs(x = "ISI Score",y="Adjusted PHQ - 9 Score")+
  theme_pubr()+theme(legend.title = element_blank())

## Individual Symptoms
# Anhedonia
interest.qp.sub1 <- glm(PHQinterest~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
interest.qp.sub1.maineff <- bind_cols("Symptom" = rep("Interest",5),
                                      "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                      bind_rows(deltaMethod(interest.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(interest.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(interest.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(interest.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(interest.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                      "p-value" = summary(interest.qp.sub1)$coefficients[2:6,4])
interest.qp.sub1.full <- glm(PHQinterest~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
interest.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Interest",5),
                                           "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                           bind_rows(deltaMethod(interest.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(interest.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(interest.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(interest.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(interest.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                           "p-value" = summary(interest.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(interest.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
interest.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Anhedonia")+
  theme_pubr()+theme(legend.position = "none")

# Depressed Mood
depress.qp.sub1 <- glm(PHQdepress~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
depress.qp.sub1.maineff <- bind_cols("Symptom" = rep("Depress",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(depress.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(depress.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(depress.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(depress.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(depress.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(depress.qp.sub1)$coefficients[2:6,4])
depress.qp.sub1.full <- glm(PHQdepress~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
depress.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Depress",5),
                                          "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                          bind_rows(deltaMethod(depress.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(depress.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(depress.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(depress.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(depress.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                          "p-value" = summary(depress.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(depress.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
depress.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Depressed Mood")+
  theme_pubr()+theme(legend.position = "none")

# Appetite
appetite.qp.sub1 <- glm(PHQappetite~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
appetite.qp.sub1.maineff <- bind_cols("Symptom" = rep("Appetite",5),
                                      "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                      bind_rows(deltaMethod(appetite.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(appetite.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(appetite.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(appetite.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                deltaMethod(appetite.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                      "p-value" = summary(appetite.qp.sub1)$coefficients[2:6,4])
appetite.qp.sub1.full <- glm(PHQappetite~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
appetite.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Appetite",5),
                                           "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                           bind_rows(deltaMethod(appetite.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(appetite.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(appetite.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(appetite.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                     deltaMethod(appetite.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                           "p-value" = summary(appetite.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(appetite.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
appetite.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Appetite Dysregulation")+
  theme_pubr()+theme(legend.position = "none")

# Fatigue
tired.qp.sub1 <- glm(PHQtired~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
tired.qp.sub1.maineff <- bind_cols("Symptom" = rep("Tired",5),
                                   "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                   bind_rows(deltaMethod(tired.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                             deltaMethod(tired.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                             deltaMethod(tired.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                             deltaMethod(tired.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                             deltaMethod(tired.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                   "p-value" = summary(tired.qp.sub1)$coefficients[2:6,4])
tired.qp.sub1.full <- glm(PHQtired~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
tired.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Tired",5),
                                        "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                        bind_rows(deltaMethod(tired.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                  deltaMethod(tired.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                  deltaMethod(tired.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                  deltaMethod(tired.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                  deltaMethod(tired.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                        "p-value" = summary(tired.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(tired.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
tired.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Fatigue")+
  theme_pubr()+theme(legend.position = "none")

# Difficulty Concentrating
concentrate.qp.sub1 <- glm(PHQconcentrate~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
concentrate.qp.sub1.maineff <- bind_cols("Symptom" = rep("Concentrate",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(concentrate.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(concentrate.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(concentrate.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(concentrate.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(concentrate.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(concentrate.qp.sub1)$coefficients[2:6,4])
concentrate.qp.sub1.full <- glm(PHQconcentrate~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
concentrate.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Concentrate",5),
                                              "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                              bind_rows(deltaMethod(concentrate.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(concentrate.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(concentrate.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(concentrate.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(concentrate.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                              "p-value" = summary(concentrate.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(concentrate.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
concentrate.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Difficulty Concentrating")+
  theme_pubr()+theme(legend.position = "none")

# Feelings of Failure
failure.qp.sub1 <- glm(PHQfailure~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
failure.qp.sub1.maineff <- bind_cols("Symptom" = rep("Failure",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(failure.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(failure.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(failure.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(failure.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(failure.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(failure.qp.sub1)$coefficients[2:6,4])
failure.qp.sub1.full <- glm(PHQfailure~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
failure.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Failure",5),
                                          "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                          bind_rows(deltaMethod(failure.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(failure.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(failure.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(failure.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(failure.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                          "p-value" = summary(failure.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(failure.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
failure.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Feelings of Failure")+
  theme_pubr()+theme(legend.position = "none")

# Psychomotor Disturbance
psychomotor.qp.sub1 <- glm(PHQpsychomotor~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
psychomotor.qp.sub1.maineff <- bind_cols("Symptom" = rep("Psychomotor",5),
                                         "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                         bind_rows(deltaMethod(psychomotor.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(psychomotor.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(psychomotor.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(psychomotor.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                                   deltaMethod(psychomotor.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                         "p-value" = summary(psychomotor.qp.sub1)$coefficients[2:6,4])
psychomotor.qp.sub1.full <- glm(PHQpsychomotor~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
psychomotor.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Psychomotor",5),
                                              "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                              bind_rows(deltaMethod(psychomotor.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(psychomotor.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(psychomotor.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(psychomotor.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                        deltaMethod(psychomotor.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                              "p-value" = summary(psychomotor.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(psychomotor.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
psychomotor.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Psychomotor Disturbance")+
  theme_pubr()+theme(legend.position = "none")

# Suicidal behavior
suicide.qp.sub1 <- glm(PHQsuicide~ISItotal*SleepDuration,phq9.subset1,family="quasipoisson")
suicide.qp.sub1.maineff <- bind_cols("Symptom" = rep("Suicide",5),
                                     "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                     bind_rows(deltaMethod(suicide.qp.sub1, "exp(b1)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(suicide.qp.sub1, "exp(b2)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(suicide.qp.sub1, "exp(b3)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(suicide.qp.sub1, "exp(b4)", parameterNames=paste("b",0:5,sep="")),
                                               deltaMethod(suicide.qp.sub1, "exp(b5)", parameterNames=paste("b",0:5,sep=""))) %>% as_tibble(),
                                     "p-value" = summary(suicide.qp.sub1)$coefficients[2:6,4])
suicide.qp.sub1.full <- glm(PHQsuicide~ISItotal*SleepDuration+Age+Sex+Education+Race+BMI,phq9.subset1,family="quasipoisson")
suicide.qp.sub1.full.maineff <- bind_cols("Symptom" = rep("Suicide",5),
                                          "Parameter" = c("ISI","Short Sleep","Long Sleep","ISI:Short Sleep","ISI:Long Sleep"),
                                          bind_rows(deltaMethod(suicide.qp.sub1.full, "exp(b1)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(suicide.qp.sub1.full, "exp(b2)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(suicide.qp.sub1.full, "exp(b3)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(suicide.qp.sub1.full, "exp(b15)", parameterNames=paste("b",0:16,sep="")),
                                                    deltaMethod(suicide.qp.sub1.full, "exp(b16)", parameterNames=paste("b",0:16,sep=""))) %>% as_tibble(),
                                          "p-value" = summary(suicide.qp.sub1.full)$coefficients[c(2:4,16,17),4])
phq9.subset1.predict<- predict(suicide.qp.sub1, type="response",se.fit=TRUE)
phq9.subset1$fit <- as.numeric(phq9.subset1.predict$fit)
phq9.subset1$se.fit <- as.numeric(phq9.subset1.predict$se.fit)
suicide.sub1.plot <- ggplot(phq9.subset1, aes(x=ISItotal,y=fit,color=SleepDuration))+geom_line(size=2)+
  geom_ribbon(aes(ymin=fit-1.96*se.fit,
                  ymax=fit+1.96*se.fit,fill=SleepDuration),alpha=.25)+
  labs(x = "ISI Score",y="Suicidal Behavior")+
  theme_pubr()+theme(legend.position = "none")

#### Table 4: Sleep influence by symptom in unadjusted/adjusted models for non-depressed only ####
phq9.symptom.sub1.summary <- bind_cols(
  bind_rows(phqscore.qp.sub1.maineff,interest.qp.sub1.maineff,depress.qp.sub1.maineff,appetite.qp.sub1.maineff,tired.qp.sub1.maineff,
            concentrate.qp.sub1.maineff,failure.qp.sub1.maineff,psychomotor.qp.sub1.maineff,suicide.qp.sub1.maineff) %>%
    transmute(Symptom = factor(Symptom,levels = c("Total","Interest","Depress","Appetite","Tired","Concentrate","Failure","Psychomotor","Suicide"),
                               labels = c("Adjusted PHQ-9 Score","Anhedonia","Depressed Mood","Appetite dysregulation","Fatigue",
                                          "Difficulty concentrating","Feelings of failure","Psychomotor disturbance","Suicidal behavior")),
              Parameter,
              "PR" = Estimate,
              "SE" = round(SE,2),
              "95% CI" = paste0("[",round(`2.5 %`,2),", ",round(`97.5 %`,2),"]"),
              "p-value" = round(`p-value`,4)),
  bind_rows(phqscore.qp.sub1.full.maineff,interest.qp.sub1.full.maineff,depress.qp.sub1.full.maineff,appetite.qp.sub1.full.maineff,
            tired.qp.sub1.full.maineff,concentrate.qp.sub1.full.maineff,failure.qp.sub1.full.maineff,psychomotor.qp.sub1.full.maineff,
            suicide.qp.sub1.full.maineff) %>%
    transmute("aPR" = Estimate,
              "aSE" = round(SE,2),
              "a95% CI" = paste0("[",round(`2.5 %`,2),", ",round(`97.5 %`,2),"]"),
              "ap-value" = round(`p-value`,4))
)
```

The table below shows their model results (as with the whole sample).

``` r
gt(phq9.symptom.sub1.summary)
```

<div id="cuntljxfis" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cuntljxfis .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cuntljxfis .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cuntljxfis .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cuntljxfis .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cuntljxfis .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cuntljxfis .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cuntljxfis .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cuntljxfis .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cuntljxfis .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cuntljxfis .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cuntljxfis .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cuntljxfis .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cuntljxfis .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#cuntljxfis .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cuntljxfis .gt_from_md > :first-child {
  margin-top: 0;
}

#cuntljxfis .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cuntljxfis .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cuntljxfis .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#cuntljxfis .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#cuntljxfis .gt_row_group_first td {
  border-top-width: 2px;
}

#cuntljxfis .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cuntljxfis .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cuntljxfis .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cuntljxfis .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cuntljxfis .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cuntljxfis .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cuntljxfis .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cuntljxfis .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cuntljxfis .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cuntljxfis .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cuntljxfis .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cuntljxfis .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cuntljxfis .gt_left {
  text-align: left;
}

#cuntljxfis .gt_center {
  text-align: center;
}

#cuntljxfis .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cuntljxfis .gt_font_normal {
  font-weight: normal;
}

#cuntljxfis .gt_font_bold {
  font-weight: bold;
}

#cuntljxfis .gt_font_italic {
  font-style: italic;
}

#cuntljxfis .gt_super {
  font-size: 65%;
}

#cuntljxfis .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#cuntljxfis .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cuntljxfis .gt_indent_1 {
  text-indent: 5px;
}

#cuntljxfis .gt_indent_2 {
  text-indent: 10px;
}

#cuntljxfis .gt_indent_3 {
  text-indent: 15px;
}

#cuntljxfis .gt_indent_4 {
  text-indent: 20px;
}

#cuntljxfis .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="Symptom">Symptom</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Parameter">Parameter</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="PR">PR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SE">SE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="95% CI">95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aPR">aPR</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="aSE">aSE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a95% CI">a95% CI</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="ap-value">ap-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0686013</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.05, 1.09]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.0684075</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.05, 1.09]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.4304891</td>
<td headers="SE" class="gt_row gt_right">0.17</td>
<td headers="95% CI" class="gt_row gt_left">[1.09, 1.77]</td>
<td headers="p-value" class="gt_row gt_right">0.0034</td>
<td headers="aPR" class="gt_row gt_right">1.4738678</td>
<td headers="aSE" class="gt_row gt_right">0.18</td>
<td headers="a95% CI" class="gt_row gt_left">[1.11, 1.84]</td>
<td headers="ap-value" class="gt_row gt_right">0.0020</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.8612373</td>
<td headers="SE" class="gt_row gt_right">0.23</td>
<td headers="95% CI" class="gt_row gt_left">[0.42, 1.31]</td>
<td headers="p-value" class="gt_row gt_right">0.5721</td>
<td headers="aPR" class="gt_row gt_right">0.8301561</td>
<td headers="aSE" class="gt_row gt_right">0.22</td>
<td headers="a95% CI" class="gt_row gt_left">[0.4, 1.26]</td>
<td headers="ap-value" class="gt_row gt_right">0.4827</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9625754</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.94, 0.98]</td>
<td headers="p-value" class="gt_row gt_right">0.0006</td>
<td headers="aPR" class="gt_row gt_right">0.9646551</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0020</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Adjusted PHQ-9 Score</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0046695</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.05]</td>
<td headers="p-value" class="gt_row gt_right">0.8424</td>
<td headers="aPR" class="gt_row gt_right">1.0069011</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 1.05]</td>
<td headers="ap-value" class="gt_row gt_right">0.7691</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0510897</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1.02, 1.08]</td>
<td headers="p-value" class="gt_row gt_right">0.0016</td>
<td headers="aPR" class="gt_row gt_right">1.0550867</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[1.02, 1.09]</td>
<td headers="ap-value" class="gt_row gt_right">0.0010</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.1877465</td>
<td headers="SE" class="gt_row gt_right">0.30</td>
<td headers="95% CI" class="gt_row gt_left">[0.61, 1.77]</td>
<td headers="p-value" class="gt_row gt_right">0.4899</td>
<td headers="aPR" class="gt_row gt_right">1.2250476</td>
<td headers="aSE" class="gt_row gt_right">0.31</td>
<td headers="a95% CI" class="gt_row gt_left">[0.61, 1.84]</td>
<td headers="ap-value" class="gt_row gt_right">0.4262</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.5938074</td>
<td headers="SE" class="gt_row gt_right">0.31</td>
<td headers="95% CI" class="gt_row gt_left">[-0.02, 1.21]</td>
<td headers="p-value" class="gt_row gt_right">0.3248</td>
<td headers="aPR" class="gt_row gt_right">0.6243789</td>
<td headers="aSE" class="gt_row gt_right">0.33</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.03, 1.27]</td>
<td headers="ap-value" class="gt_row gt_right">0.3754</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9620691</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.92, 1.01]</td>
<td headers="p-value" class="gt_row gt_right">0.0940</td>
<td headers="aPR" class="gt_row gt_right">0.9582441</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.91, 1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0767</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Anhedonia</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0495973</td>
<td headers="SE" class="gt_row gt_right">0.05</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.14]</td>
<td headers="p-value" class="gt_row gt_right">0.2710</td>
<td headers="aPR" class="gt_row gt_right">1.0455395</td>
<td headers="aSE" class="gt_row gt_right">0.05</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 1.14]</td>
<td headers="ap-value" class="gt_row gt_right">0.3118</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0477059</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.02, 1.07]</td>
<td headers="p-value" class="gt_row gt_right">0.0005</td>
<td headers="aPR" class="gt_row gt_right">1.0497628</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.02, 1.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.0003</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.3998214</td>
<td headers="SE" class="gt_row gt_right">0.28</td>
<td headers="95% CI" class="gt_row gt_left">[0.85, 1.95]</td>
<td headers="p-value" class="gt_row gt_right">0.0925</td>
<td headers="aPR" class="gt_row gt_right">1.4840074</td>
<td headers="aSE" class="gt_row gt_right">0.30</td>
<td headers="a95% CI" class="gt_row gt_left">[0.89, 2.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.0542</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.6269109</td>
<td headers="SE" class="gt_row gt_right">0.28</td>
<td headers="95% CI" class="gt_row gt_left">[0.08, 1.17]</td>
<td headers="p-value" class="gt_row gt_right">0.2949</td>
<td headers="aPR" class="gt_row gt_right">0.6079280</td>
<td headers="aSE" class="gt_row gt_right">0.27</td>
<td headers="a95% CI" class="gt_row gt_left">[0.08, 1.14]</td>
<td headers="ap-value" class="gt_row gt_right">0.2631</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9581394</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.92, 0.99]</td>
<td headers="p-value" class="gt_row gt_right">0.0241</td>
<td headers="aPR" class="gt_row gt_right">0.9546943</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.92, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0189</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Depressed Mood</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0372800</td>
<td headers="SE" class="gt_row gt_right">0.04</td>
<td headers="95% CI" class="gt_row gt_left">[0.96, 1.12]</td>
<td headers="p-value" class="gt_row gt_right">0.3405</td>
<td headers="aPR" class="gt_row gt_right">1.0396821</td>
<td headers="aSE" class="gt_row gt_right">0.04</td>
<td headers="a95% CI" class="gt_row gt_left">[0.96, 1.12]</td>
<td headers="ap-value" class="gt_row gt_right">0.3085</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0609296</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1.03, 1.09]</td>
<td headers="p-value" class="gt_row gt_right">0.0002</td>
<td headers="aPR" class="gt_row gt_right">1.0587363</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[1.03, 1.09]</td>
<td headers="ap-value" class="gt_row gt_right">0.0003</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.7426111</td>
<td headers="SE" class="gt_row gt_right">0.38</td>
<td headers="95% CI" class="gt_row gt_left">[1.01, 2.48]</td>
<td headers="p-value" class="gt_row gt_right">0.0102</td>
<td headers="aPR" class="gt_row gt_right">1.8629647</td>
<td headers="aSE" class="gt_row gt_right">0.41</td>
<td headers="a95% CI" class="gt_row gt_left">[1.06, 2.67]</td>
<td headers="ap-value" class="gt_row gt_right">0.0050</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.2526604</td>
<td headers="SE" class="gt_row gt_right">0.56</td>
<td headers="95% CI" class="gt_row gt_left">[0.16, 2.34]</td>
<td headers="p-value" class="gt_row gt_right">0.6114</td>
<td headers="aPR" class="gt_row gt_right">1.1705133</td>
<td headers="aSE" class="gt_row gt_right">0.52</td>
<td headers="a95% CI" class="gt_row gt_left">[0.15, 2.19]</td>
<td headers="ap-value" class="gt_row gt_right">0.7228</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9675740</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 1.01]</td>
<td headers="p-value" class="gt_row gt_right">0.1030</td>
<td headers="aPR" class="gt_row gt_right">0.9653307</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.93, 1.01]</td>
<td headers="ap-value" class="gt_row gt_right">0.0941</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Appetite dysregulation</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9835712</td>
<td headers="SE" class="gt_row gt_right">0.04</td>
<td headers="95% CI" class="gt_row gt_left">[0.9, 1.07]</td>
<td headers="p-value" class="gt_row gt_right">0.6977</td>
<td headers="aPR" class="gt_row gt_right">0.9926187</td>
<td headers="aSE" class="gt_row gt_right">0.04</td>
<td headers="a95% CI" class="gt_row gt_left">[0.91, 1.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.8616</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0857724</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[1.07, 1.1]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.0832091</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[1.06, 1.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.4311582</td>
<td headers="SE" class="gt_row gt_right">0.20</td>
<td headers="95% CI" class="gt_row gt_left">[1.05, 1.82]</td>
<td headers="p-value" class="gt_row gt_right">0.0093</td>
<td headers="aPR" class="gt_row gt_right">1.4279642</td>
<td headers="aSE" class="gt_row gt_right">0.20</td>
<td headers="a95% CI" class="gt_row gt_left">[1.03, 1.82]</td>
<td headers="ap-value" class="gt_row gt_right">0.0117</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0100373</td>
<td headers="SE" class="gt_row gt_right">0.30</td>
<td headers="95% CI" class="gt_row gt_left">[0.43, 1.59]</td>
<td headers="p-value" class="gt_row gt_right">0.9730</td>
<td headers="aPR" class="gt_row gt_right">0.9640999</td>
<td headers="aSE" class="gt_row gt_right">0.28</td>
<td headers="a95% CI" class="gt_row gt_left">[0.41, 1.52]</td>
<td headers="ap-value" class="gt_row gt_right">0.9011</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9610082</td>
<td headers="SE" class="gt_row gt_right">0.01</td>
<td headers="95% CI" class="gt_row gt_left">[0.94, 0.98]</td>
<td headers="p-value" class="gt_row gt_right">0.0010</td>
<td headers="aPR" class="gt_row gt_right">0.9697321</td>
<td headers="aSE" class="gt_row gt_right">0.01</td>
<td headers="a95% CI" class="gt_row gt_left">[0.95, 0.99]</td>
<td headers="ap-value" class="gt_row gt_right">0.0152</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Fatigue</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9841725</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.5455</td>
<td headers="aPR" class="gt_row gt_right">0.9908502</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 1.04]</td>
<td headers="ap-value" class="gt_row gt_right">0.7261</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0848619</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1.05, 1.12]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.0828414</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[1.05, 1.12]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.6476865</td>
<td headers="SE" class="gt_row gt_right">0.42</td>
<td headers="95% CI" class="gt_row gt_left">[0.82, 2.48]</td>
<td headers="p-value" class="gt_row gt_right">0.0524</td>
<td headers="aPR" class="gt_row gt_right">1.6833236</td>
<td headers="aSE" class="gt_row gt_right">0.45</td>
<td headers="a95% CI" class="gt_row gt_left">[0.8, 2.56]</td>
<td headers="ap-value" class="gt_row gt_right">0.0512</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.6408899</td>
<td headers="SE" class="gt_row gt_right">0.41</td>
<td headers="95% CI" class="gt_row gt_left">[-0.17, 1.45]</td>
<td headers="p-value" class="gt_row gt_right">0.4888</td>
<td headers="aPR" class="gt_row gt_right">0.5712258</td>
<td headers="aSE" class="gt_row gt_right">0.38</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.17, 1.31]</td>
<td headers="ap-value" class="gt_row gt_right">0.3951</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9430012</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.9, 0.99]</td>
<td headers="p-value" class="gt_row gt_right">0.0113</td>
<td headers="aPR" class="gt_row gt_right">0.9515255</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.91, 1]</td>
<td headers="ap-value" class="gt_row gt_right">0.0430</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Difficulty concentrating</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0030407</td>
<td headers="SE" class="gt_row gt_right">0.05</td>
<td headers="95% CI" class="gt_row gt_left">[0.9, 1.11]</td>
<td headers="p-value" class="gt_row gt_right">0.9556</td>
<td headers="aPR" class="gt_row gt_right">1.0050883</td>
<td headers="aSE" class="gt_row gt_right">0.06</td>
<td headers="a95% CI" class="gt_row gt_left">[0.9, 1.11]</td>
<td headers="ap-value" class="gt_row gt_right">0.9275</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.0394952</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[1.01, 1.07]</td>
<td headers="p-value" class="gt_row gt_right">0.0165</td>
<td headers="aPR" class="gt_row gt_right">1.0430680</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[1.01, 1.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.0114</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.2784439</td>
<td headers="SE" class="gt_row gt_right">0.30</td>
<td headers="95% CI" class="gt_row gt_left">[0.69, 1.86]</td>
<td headers="p-value" class="gt_row gt_right">0.2920</td>
<td headers="aPR" class="gt_row gt_right">1.3399599</td>
<td headers="aSE" class="gt_row gt_right">0.32</td>
<td headers="a95% CI" class="gt_row gt_left">[0.7, 1.98]</td>
<td headers="ap-value" class="gt_row gt_right">0.2273</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.8019526</td>
<td headers="SE" class="gt_row gt_right">0.40</td>
<td headers="95% CI" class="gt_row gt_left">[0.02, 1.58]</td>
<td headers="p-value" class="gt_row gt_right">0.6557</td>
<td headers="aPR" class="gt_row gt_right">0.7949801</td>
<td headers="aSE" class="gt_row gt_right">0.40</td>
<td headers="a95% CI" class="gt_row gt_left">[0.01, 1.58]</td>
<td headers="ap-value" class="gt_row gt_right">0.6470</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9800119</td>
<td headers="SE" class="gt_row gt_right">0.02</td>
<td headers="95% CI" class="gt_row gt_left">[0.94, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.3594</td>
<td headers="aPR" class="gt_row gt_right">0.9805971</td>
<td headers="aSE" class="gt_row gt_right">0.02</td>
<td headers="a95% CI" class="gt_row gt_left">[0.94, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.3994</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Feelings of failure</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0212248</td>
<td headers="SE" class="gt_row gt_right">0.05</td>
<td headers="95% CI" class="gt_row gt_left">[0.93, 1.11]</td>
<td headers="p-value" class="gt_row gt_right">0.6447</td>
<td headers="aPR" class="gt_row gt_right">1.0228449</td>
<td headers="aSE" class="gt_row gt_right">0.05</td>
<td headers="a95% CI" class="gt_row gt_left">[0.93, 1.11]</td>
<td headers="ap-value" class="gt_row gt_right">0.6226</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.1307922</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[1.07, 1.19]</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td>
<td headers="aPR" class="gt_row gt_right">1.1319852</td>
<td headers="aSE" class="gt_row gt_right">0.03</td>
<td headers="a95% CI" class="gt_row gt_left">[1.07, 1.19]</td>
<td headers="ap-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">1.3340265</td>
<td headers="SE" class="gt_row gt_right">0.57</td>
<td headers="95% CI" class="gt_row gt_left">[0.21, 2.46]</td>
<td headers="p-value" class="gt_row gt_right">0.5020</td>
<td headers="aPR" class="gt_row gt_right">1.2246660</td>
<td headers="aSE" class="gt_row gt_right">0.55</td>
<td headers="a95% CI" class="gt_row gt_left">[0.15, 2.3]</td>
<td headers="ap-value" class="gt_row gt_right">0.6520</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.6218032</td>
<td headers="SE" class="gt_row gt_right">0.68</td>
<td headers="95% CI" class="gt_row gt_left">[-0.71, 1.96]</td>
<td headers="p-value" class="gt_row gt_right">0.6644</td>
<td headers="aPR" class="gt_row gt_right">0.5269802</td>
<td headers="aSE" class="gt_row gt_right">0.61</td>
<td headers="a95% CI" class="gt_row gt_left">[-0.67, 1.72]</td>
<td headers="ap-value" class="gt_row gt_right">0.5805</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9553425</td>
<td headers="SE" class="gt_row gt_right">0.03</td>
<td headers="95% CI" class="gt_row gt_left">[0.89, 1.02]</td>
<td headers="p-value" class="gt_row gt_right">0.1887</td>
<td headers="aPR" class="gt_row gt_right">0.9584913</td>
<td headers="aSE" class="gt_row gt_right">0.04</td>
<td headers="a95% CI" class="gt_row gt_left">[0.89, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.2581</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Psychomotor disturbance</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">1.0061248</td>
<td headers="SE" class="gt_row gt_right">0.08</td>
<td headers="95% CI" class="gt_row gt_left">[0.84, 1.17]</td>
<td headers="p-value" class="gt_row gt_right">0.9414</td>
<td headers="aPR" class="gt_row gt_right">1.0046670</td>
<td headers="aSE" class="gt_row gt_right">0.09</td>
<td headers="a95% CI" class="gt_row gt_left">[0.83, 1.18]</td>
<td headers="ap-value" class="gt_row gt_right">0.9578</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI</td>
<td headers="PR" class="gt_row gt_right">1.1327105</td>
<td headers="SE" class="gt_row gt_right">0.05</td>
<td headers="95% CI" class="gt_row gt_left">[1.03, 1.23]</td>
<td headers="p-value" class="gt_row gt_right">0.0060</td>
<td headers="aPR" class="gt_row gt_right">1.1298284</td>
<td headers="aSE" class="gt_row gt_right">0.05</td>
<td headers="a95% CI" class="gt_row gt_left">[1.03, 1.23]</td>
<td headers="ap-value" class="gt_row gt_right">0.0090</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">Short Sleep</td>
<td headers="PR" class="gt_row gt_right">2.1137730</td>
<td headers="SE" class="gt_row gt_right">1.49</td>
<td headers="95% CI" class="gt_row gt_left">[-0.8, 5.03]</td>
<td headers="p-value" class="gt_row gt_right">0.2884</td>
<td headers="aPR" class="gt_row gt_right">2.7774162</td>
<td headers="aSE" class="gt_row gt_right">2.02</td>
<td headers="a95% CI" class="gt_row gt_left">[-1.19, 6.74]</td>
<td headers="ap-value" class="gt_row gt_right">0.1612</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">Long Sleep</td>
<td headers="PR" class="gt_row gt_right">3.8123764</td>
<td headers="SE" class="gt_row gt_right">4.29</td>
<td headers="95% CI" class="gt_row gt_left">[-4.6, 12.23]</td>
<td headers="p-value" class="gt_row gt_right">0.2352</td>
<td headers="aPR" class="gt_row gt_right">4.1331270</td>
<td headers="aSE" class="gt_row gt_right">4.57</td>
<td headers="a95% CI" class="gt_row gt_left">[-4.83, 13.1]</td>
<td headers="ap-value" class="gt_row gt_right">0.2001</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Short Sleep</td>
<td headers="PR" class="gt_row gt_right">0.9323474</td>
<td headers="SE" class="gt_row gt_right">0.06</td>
<td headers="95% CI" class="gt_row gt_left">[0.82, 1.04]</td>
<td headers="p-value" class="gt_row gt_right">0.2376</td>
<td headers="aPR" class="gt_row gt_right">0.9101974</td>
<td headers="aSE" class="gt_row gt_right">0.06</td>
<td headers="a95% CI" class="gt_row gt_left">[0.79, 1.03]</td>
<td headers="ap-value" class="gt_row gt_right">0.1502</td></tr>
    <tr><td headers="Symptom" class="gt_row gt_center">Suicidal behavior</td>
<td headers="Parameter" class="gt_row gt_left">ISI:Long Sleep</td>
<td headers="PR" class="gt_row gt_right">0.8988875</td>
<td headers="SE" class="gt_row gt_right">0.10</td>
<td headers="95% CI" class="gt_row gt_left">[0.7, 1.1]</td>
<td headers="p-value" class="gt_row gt_right">0.3489</td>
<td headers="aPR" class="gt_row gt_right">0.8853279</td>
<td headers="aSE" class="gt_row gt_right">0.10</td>
<td headers="a95% CI" class="gt_row gt_left">[0.69, 1.08]</td>
<td headers="ap-value" class="gt_row gt_right">0.2709</td></tr>
  </tbody>
  
  
</table>
</div>

The figure below shows how changing ISI score is related to adjusted PHQ-9 score by sleep duration in this group.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />
