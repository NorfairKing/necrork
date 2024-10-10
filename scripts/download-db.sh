set -x
echo ".backup backup.sqlite3" | ssh reserved 'cd /www/nix-ci/staging/leader && sqlite3 nix-ci-leader.sqlite3'
scp -O reserved:/www/nix-ci/staging/leader/backup.sqlite3 staging.sqlite3
cp staging.sqlite3 nix-ci-leader/nix-ci-leader.sqlite3
