# PREIS Ebola DRC — GitHub Actions 24/7 SitRep Monitor

This package adds cloud monitoring for INSP DRC Ebola SitReps.

## Files to copy into your GitHub repository

- `.github/workflows/preis_sitrep_monitor.yml`
- `scripts/08_cloud_sitrep_monitor.R`
- `alert_recipients.csv`
- `data/monitor_state/preis_sitrep_email_state.csv`

## GitHub Secrets required

In your GitHub repository:

Settings → Secrets and variables → Actions → New repository secret

Create these secrets:

- `SMTP_USER` = your Gmail address, e.g. `zrhyacinthe@gmail.com`
- `SMTP_PASS` = Gmail app password, not your normal Gmail password
- `ALERT_FROM` = sender email, e.g. `zrhyacinthe@gmail.com`

## Recipients

Edit `alert_recipients.csv` at the root of the repository:

```csv
active,type,name,email
TRUE,to,Dr Zabre,raogoz@africacdc.org
TRUE,cc,Name,name@example.org
TRUE,bcc,Name,name@example.org
```

## Frequency

The workflow runs every 5 minutes:

```yaml
schedule:
  - cron: "*/5 * * * *"
```

## Manual run

In GitHub:

Actions → PREIS Ebola DRC SitRep Monitor → Run workflow

## How duplicate sending is avoided

The script writes the sent SitRep into:

`data/monitor_state/preis_sitrep_email_state.csv`

The workflow commits this file back to the repository after a successful send.
