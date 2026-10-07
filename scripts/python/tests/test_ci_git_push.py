# -*- coding: utf-8 -*-
"""Tests de scripts/ci_git_push.sh + .gitattributes (cause des doublons d'envoi du 07/10/2026).

Scenario reproduit : un run B (checkout ancien) a des lignes d'etat a pousser alors que main a avance
(un autre run a ajoute SA ligne dans les memes fichiers). Avant correction : `git pull --rebase ... || echo`
et `git push || echo` avalaient l'echec -> etat perdu -> mails renvoyes.
Apres correction : les lignes des DEUX cotes sont conservees (merge=union) et le push reussit.
"""
import os
import shutil
import subprocess
import tempfile
import unittest

REPO = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", ".."))
SCRIPT = os.path.join(REPO, "scripts", "ci_git_push.sh")
ATTR = os.path.join(REPO, ".gitattributes")


def sh(cmd, cwd, check=True):
    p = subprocess.run(cmd, cwd=cwd, shell=True, capture_output=True, text=True,
                       env=dict(os.environ, GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                                GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t"))
    if check and p.returncode:
        raise RuntimeError("%s\n%s%s" % (cmd, p.stdout, p.stderr))
    return p


class TestCiGitPush(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp(prefix="cigit_")
        sh("git init -q --bare -b main remote.git", self.d)
        for n in ("a", "b"):
            sh("git clone -q remote.git %s" % n, self.d, check=False)
            sh("git config user.email %s@x && git config user.name %s" % (n, n), os.path.join(self.d, n))
        a = os.path.join(self.d, "a")
        sh("git checkout -q -B main", a)
        os.makedirs(os.path.join(a, "data", "monitor_state"))
        os.makedirs(os.path.join(a, "scripts"))
        shutil.copy(ATTR, os.path.join(a, ".gitattributes"))
        shutil.copy(SCRIPT, os.path.join(a, "scripts", "ci_git_push.sh"))
        with open(os.path.join(a, "data", "preis_email_enrichi_state.csv"), "w") as f:
            f.write("sitrep_no,sent\n")
        with open(os.path.join(a, "data", "other.csv"), "w") as f:
            f.write("x\n")
        sh("git add -A && git commit -qm init && git push -q origin main", a)
        sh("git pull -q origin main", os.path.join(self.d, "b"))
        sh("git checkout -q -B main origin/main", os.path.join(self.d, "b"))

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_concurrent_state_rows_are_both_kept_and_push_succeeds(self):
        a, b = os.path.join(self.d, "a"), os.path.join(self.d, "b")
        with open(os.path.join(b, "data", "preis_email_enrichi_state.csv"), "a") as f:
            f.write("144,2026-10-07 11:43 UTC\n")
        sh("git add -A && git commit -qm runA && git push -q origin main", b)      # l'autre run a deja pousse
        with open(os.path.join(a, "data", "preis_email_enrichi_state.csv"), "a") as f:
            f.write("144,2026-10-07 12:02 UTC\n")
        with open(os.path.join(a, "data", "other.csv"), "a") as f:
            f.write("modification non indexee\n")                                  # fichier modifie mais non ajoute
        sh("git add data/preis_email_enrichi_state.csv", a)
        p = sh("bash scripts/ci_git_push.sh 'run B' --strict", a, check=False)
        self.assertEqual(p.returncode, 0, p.stdout + p.stderr)
        sh("git fetch -q origin && git diff --quiet origin/main HEAD", a)          # a et origin identiques
        state = open(os.path.join(a, "data", "preis_email_enrichi_state.csv")).read()
        self.assertIn("11:43", state)
        self.assertIn("12:02", state)                                              # aucune ligne perdue

    def test_nothing_to_commit_is_not_an_error(self):
        a = os.path.join(self.d, "a")
        p = sh("bash scripts/ci_git_push.sh 'rien' --strict", a, check=False)
        self.assertEqual(p.returncode, 0, p.stdout + p.stderr)

    def test_failed_push_is_visible_in_strict_mode(self):
        a = os.path.join(self.d, "a")
        with open(os.path.join(a, "data", "other.csv"), "a") as f:
            f.write("y\n")
        sh("git add -A", a)
        sh("git remote set-url origin /nonexistent/remote.git", a)
        p = sh("bash scripts/ci_git_push.sh 'echec' --strict", a, check=False)
        self.assertNotEqual(p.returncode, 0)
        self.assertIn("ECHEC du push", p.stdout + p.stderr)
        p = sh("bash scripts/ci_git_push.sh 'echec2'", a, check=False)             # non strict : n'echoue pas le run
        self.assertEqual(p.returncode, 0)


if __name__ == "__main__":
    unittest.main(verbosity=2)
