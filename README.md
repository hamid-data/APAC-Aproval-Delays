# How long do cancer drugs take to reach Asia-Pacific patients?

A small data project looking at how long it takes for two cancer drugs, **pembrolizumab** and **trastuzumab deruxtecan (Td-XD)**, to get approved in Asia-Pacific countries after they are approved by the US FDA. I also checked whether drugs with a higher clinical benefit score get approved faster.

## Why I did this

New cancer drugs can make a real difference to survival and quality of life, but patients only benefit once the drug is approved where they live. I wanted to see how big the gaps are across the region and whether "more beneficial" drugs are actually approved sooner. It was also a good chance to practise cleaning real-world regulatory data and presenting it clearly in R.

## The question

1. How many months pass between FDA approval and local approval in each country?
2. Do delays differ by cancer type, indication, or whether the treatment aims to cure?
3. Do indications with a higher ESMO-MCBS score (a standard scale for clinical benefit) get approved faster?

## What's in this repository

```
├── README.md
├── code/
│   └── APAC_Approval_Delay_Analysis.R      # data prep, tables, figures, exploratory tests
└── report/
    └── APAC_Approval_Delays_Report.docx    # short write-up with tables and figures
```

## The data

- **Countries (10):** Australia, Hong Kong, Japan, Malaysia, New Zealand, the Philippines, South Korea, Singapore, Thailand and Vietnam.
- **Unit of analysis:** one row per drug, indication and country.
- **Main fields:** FDA approval date, local approval date, cancer type, indication, curative intent (yes/no), ESMO-MCBS score and category (high/low), and whether the indication is in ESMO guidelines.
- **Source:** publicly available regulatory information. No patient-level or confidential data were used.
- **The dataset itself is not included here.** Add your own copy and load it in R as a data frame called `data` before running the script (see "How to run").

## How I did it

- **Delay (months)** = (local approval date − FDA approval date) ÷ 30.44. A negative number means the country approved the drug *before* the FDA.
- Summarised delays with the **median and interquartile range (IQR)**, plus the minimum and maximum, because the data are skewed and have outliers.
- Broke the results down by drug, country, cancer type, indication and curative intent.
- Indications with **no FDA approval date** can't be given a delay, so they are listed separately instead of being dropped quietly.
- Made swimlane timelines, box plots and scatter plots to compare countries and look at the benefit score.
- Everything is descriptive. The report does not claim any cause and effect.

## What I found

- **Delays vary a lot.** For pembrolizumab, median delays ranged from about **6.7 months (Philippines)** to **21 months (Vietnam)**.
- **Td-XD varies even more**, from about **−7.9 months (Hong Kong, approved before the FDA)** to **40 months (New Zealand)**.
- **Tumour type and setting matter.** Delays differed noticeably by cancer type, indication and curative intent, and the pattern was not the same in every country.
- **Higher clinical benefit did not mean faster approval.** There was no clear link between ESMO-MCBS score and delay, so benefit alone doesn't seem to explain regulatory timing.

## How to run

1. Install R (4.2 or newer is a safe choice) and, if you like, RStudio.
2. Install the packages:
   ```r
   install.packages(c("dplyr", "stringr", "janitor", "ggplot2", "gt"))
   ```
3. Load your dataset into R as `data` (for example with `readxl::read_excel()` or `readr::read_csv()`).
4. Run `code/APAC_Approval_Delay_Analysis.R` from top to bottom. Tables and figures appear in the RStudio viewer and plot panes.

The script renames some long column names near the top. If your column names differ, adjust that `rename()` step first.

## Things to keep in mind

- Delays depend on the dates recorded in each country's public sources, which may be incomplete or recorded differently.
- Many country-indication groups are small (often one or two approvals), so medians and IQRs for them should be read with care.
- One implausible South Korea pembrolizumab (breast) approval date was corrected by hand in the script. It is clearly marked in the code.
- The analysis is observational and descriptive, and it covers only two drugs.

## Tools

R, with `dplyr`, `stringr`, `janitor`, `ggplot2` and `gt`.


## Contact

Questions or suggestions are welcome. You can open an issue on this repository or reach me through my GitHub profile.# APAC-Aproval-Delays
