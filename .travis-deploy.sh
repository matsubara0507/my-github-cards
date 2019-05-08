#!/bin/bash
set -eux

# setup ssh-agent and provide the GitHub deploy key
eval "$(ssh-agent -s)"
openssl aes-256-cbc -K $encrypted_3b94903f5871_key -iv $encrypted_3b94903f5871_iv -in id_rsa.enc -out id_rsa -d
chmod 600 id_rsa
ssh-add id_rsa

# commit the assets in docs/ if changed, and push to GitHub using SSH
git config user.name "${GIT_NAME}"
git config user.email "${GIT_EMAIL}"
git status
git add docs
git diff --staged --quiet || git commit -m "[skip ci] Update docs by selfcat"
git push origin master
