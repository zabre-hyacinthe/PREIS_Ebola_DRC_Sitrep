@echo off
echo ============================================================
echo PREIS GitHub Actions commit and push
echo ============================================================
cd /d "D:\PREIS_Ebola_DRC_Sitrep_FV_12.06.26"
git status
git add .github/workflows/preis_sitrep_monitor.yml
git add scripts/08_cloud_sitrep_monitor.R
git add alert_recipients.csv
git add data/monitor_state/preis_sitrep_email_state.csv
git add README_GITHUB_ACTIONS_SETUP.md
git commit -m "Add PREIS SitRep 24-7 GitHub Actions monitor"
git push
pause
