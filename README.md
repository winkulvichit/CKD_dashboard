<p align="center">

<img src="https://raw.githubusercontent.com/EC-CCN/logo/master/EC-CCN%20logo%204K.png" width="160">

</p>

<h1 align="center">CKD Participant Dashboard</h1>

<p align="center">
Clinical research dashboard for CKD cohort longitudinal analysis
</p>

---

Below is a clean, professional README suitable for an academic Shiny dashboard project.
It is structured for clinical research, GitHub clarity, and future collaborators.

You can copy–paste directly into README.md.

⸻

CKD Participant Dashboard

Interactive Shiny dashboard for longitudinal analysis of chronic kidney disease (CKD) participants, including disease classification, follow-up trajectory, and geographic distribution.

Developed by EC-CCN (Excellence Center for Critical Care Nephrology).

⸻

Live Features

Participant filtering

Filter cohort dynamically by:
	•	age (minimum age)
	•	DM
	•	HTN
	•	DLP
	•	CVD
	•	smoking
	•	alcohol
	•	herbal medicine
	•	analgesic use
	•	supplement use
	•	ALBII (Positive / Negative)

All visualizations update automatically based on selected cohort.

⸻

Dashboard components

1. General Information

Overview of cohort structure:
	•	number of new participants per calendar year
	•	number of follow-up participants per year
	•	retention trajectory of participants across consecutive years
	•	number of participants completing 1–4 years of follow-up

Retention plot allows selecting starting year (2022–2025).

⸻

2. CKD Diagnostic Heatmaps

Heatmaps summarizing CKD diagnosis patterns across years:
	•	CKD diagnosed by:
	•	eGFR alone
	•	UACR alone
	•	both eGFR + UACR
	•	no CKD
	•	CKD G stage distribution
	•	CKD A stage distribution
	•	KDIGO CKD risk categories:
	•	Green
	•	Yellow
	•	Orange
	•	Red

Each heatmap displays:
	•	percentage distribution
	•	total participant count per year
	•	overall distribution

⸻

3. Longitudinal Trajectory Visualization

Alluvial plots illustrating progression across follow-up years:
	•	CKD G stage trajectory
	•	KDIGO risk trajectory

Plots include all participants, including those with incomplete follow-up.

Participants not seen in later years are labelled:

Not seen

⸻

4. Geographic Distribution Map

Subdistrict-level visualization of participants in Ban Phaeo district, Samut Sakhon.

Features:
	•	heatmap coloring based on percentage values
	•	labels inside each subdistrict
	•	numerator / denominator display
	•	selectable metrics:

Participant count
CKD prevalence
eGFR alone
UACR alone
Both eGFR + UACR
Green risk
Yellow risk
Orange risk
Red risk

Denominator logic:

Participant count:
participants in subdistrict / total filtered participants

Other metrics:
participants with condition / participants in subdistrict

⸻

Data structure

Longitudinal dataset format:

variable	description
study_id	unique participant ID
year	calendar year
followup_year	follow-up sequence (1–4)
egfr	estimated GFR
uacr	urine albumin creatinine ratio
g_stage	CKD G classification
a_stage	CKD A classification
risk_4cat	KDIGO risk group
diagnosis_type	CKD diagnostic category
subdistrict	participant residence
albii	ALBII status


⸻

Installation

Clone repository:

git clone https://github.com/winkulvichit/CKD_dashboard.git
cd CKD_dashboard

Install required packages:

install.packages(c(
  "shiny",
  "tidyverse",
  "ggplot2",
  "ggalluvial",
  "sf",
  "scales",
  "plotly"
))

Run locally:

shiny::runApp("app.R")


⸻


Version control

GitHub is used for version control.

Typical workflow:

git add .
git commit -m "Describe change"
git push

Tag stable versions:

git tag -a v1.0 -m "Stable version"
git push origin v1.0


⸻

Data privacy

Participant-level data are not stored in the public repository.

Local files excluded using .gitignore:
	•	csv
	•	xlsx
	•	shapefiles

⸻

Authors

Win Kulvichit MD MSc
Nephrologist
King Chulalongkorn Memorial Hospital

Excellence Center for Critical Care Nephrology (EC-CCN)

⸻

License

© 2026 EC-CCN. All rights reserved.

⸻
