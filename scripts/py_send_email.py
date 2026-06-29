#!/usr/bin/env python3
import os
import re
import ssl
import sys
import smtplib
from email.message import EmailMessage


def getenv(name, default=""):
    value = os.environ.get(name, default)
    if value is None:
        value = default
    return str(value).strip()


def split_emails(value):
    value = value or ""
    parts = re.split(r"[,;]", value)
    out = []
    for p in parts:
        p = p.strip()
        if p:
            out.append(p)
    return list(dict.fromkeys(out))


def redact(text):
    text = str(text)
    user = getenv("SMTP_USER")
    pwd = getenv("SMTP_PASS")
    if pwd:
        text = text.replace(pwd, "********")
    if user:
        text = text.replace(user, "SMTP_USER")
    return text


def require_email_list(values, name):
    if not values:
        raise ValueError(f"{name} est vide.")
    bad = [x for x in values if not re.match(r"^[^@\s]+@[^@\s]+\.[^@\s]+$", x)]
    if bad:
        raise ValueError(f"{name} contient email(s) invalide(s): {', '.join(bad)}")


def main():
    smtp_host = getenv("SMTP_HOST")
    smtp_port = int(getenv("SMTP_PORT", "465"))
    smtp_user = getenv("SMTP_USER")
    smtp_pass = getenv("SMTP_PASS")

    alert_from = getenv("ALERT_FROM")
    alert_to = split_emails(getenv("ALERT_TO"))
    alert_cc = split_emails(getenv("ALERT_CC"))
    alert_bcc = split_emails(getenv("ALERT_BCC"))

    subject = getenv("PREIS_EMAIL_SUBJECT")
    body_file = getenv("PREIS_EMAIL_BODY_FILE")
    attachment = getenv("PREIS_EMAIL_ATTACHMENT")

    if not smtp_host:
        raise ValueError("SMTP_HOST est vide.")
    if not smtp_user:
        raise ValueError("SMTP_USER est vide.")
    if not smtp_pass:
        raise ValueError("SMTP_PASS est vide.")
    if not alert_from:
        raise ValueError("ALERT_FROM est vide.")
    if not subject:
        raise ValueError("PREIS_EMAIL_SUBJECT est vide.")
    if not body_file or not os.path.exists(body_file):
        raise ValueError("PREIS_EMAIL_BODY_FILE est introuvable.")
    if not attachment or not os.path.exists(attachment):
        raise ValueError("PREIS_EMAIL_ATTACHMENT est introuvable.")

    require_email_list([alert_from], "ALERT_FROM")
    require_email_list(alert_to, "ALERT_TO")
    require_email_list(alert_cc, "ALERT_CC")
    require_email_list(alert_bcc, "ALERT_BCC")

    with open(body_file, "r", encoding="utf-8") as f:
        body = f.read()

    recipients = list(dict.fromkeys(alert_to + alert_cc + alert_bcc))

    msg = EmailMessage()
    msg["From"] = alert_from
    msg["To"] = ", ".join(alert_to)
    if alert_cc:
        msg["Cc"] = ", ".join(alert_cc)
    msg["Subject"] = subject
    msg.set_content(body, charset="utf-8")

    with open(attachment, "rb") as f:
        pdf_data = f.read()

    msg.add_attachment(
        pdf_data,
        maintype="application",
        subtype="pdf",
        filename=os.path.basename(attachment),
    )

    print(f"SMTP host: {smtp_host}")
    print(f"SMTP port: {smtp_port}")
    print("SMTP user: SMTP_USER")
    print(f"Recipients: {', '.join(recipients)}")
    print(f"Attachment: {attachment}")

    try:
        if smtp_port == 465:
            context = ssl.create_default_context()
            with smtplib.SMTP_SSL(smtp_host, smtp_port, timeout=90, context=context) as server:
                server.login(smtp_user, smtp_pass)
                server.send_message(msg, from_addr=alert_from, to_addrs=recipients)
        else:
            context = ssl.create_default_context()
            with smtplib.SMTP(smtp_host, smtp_port, timeout=90) as server:
                server.ehlo()
                server.starttls(context=context)
                server.ehlo()
                server.login(smtp_user, smtp_pass)
                server.send_message(msg, from_addr=alert_from, to_addrs=recipients)

        print("PYTHON_SMTP_EMAIL_SENT")
        return 0

    except Exception as e:
        print("PYTHON_SMTP_ERROR:", redact(repr(e)), file=sys.stderr)
        return 2


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as e:
        print("PYTHON_SMTP_FATAL:", redact(repr(e)), file=sys.stderr)
        sys.exit(2)
