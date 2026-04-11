<p align="center">

<img src="https://raw.githubusercontent.com/EC-CCN/logo/master/EC-CCN%20logo%204K.png" width="160">

</p>

<h1 align="center">CKD Participant Dashboard</h1>

<p align="center">
Clinical research dashboard for CKD cohort longitudinal analysis
</p>


https://ecccn-kcmh.shinyapps.io/ckd_dashboard/
⸻

CKD Participant Dashboard

Interactive Shiny dashboard for longitudinal analysis of chronic kidney disease (CKD) participants, including disease classification, follow-up trajectory, and geographic distribution.

Developed by EC-CCN (Excellence Center for Critical Care Nephrology).

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
