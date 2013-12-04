# git helpers
function git-stash-recent-commits {
  for i in {0..$1}
  do
    export git_last_stash_name = git log --format=%B -n 1 HEAD | head -1 | sed -e 's/ /_/g';
    git revert HEAD~1
    git stash save $git_last_stash_name
  done
}
function gitdiff {
  fact; git diff | vim -R -;
}
function rebasei {
  fact; git rebase --interactive "$@";
}
function rebaseroot {
  git rebase --interactive --root $tip
}

# random facts
alias fact="/usr/local/Cellar/elinks/0.11.7/bin/elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
alias fact="/usr/local/Cellar/elinks/0.11.7/bin/elinks -dump randomfunfacts.com | sed -n '/^| /p' | tr -d \|"
PROMPT_COMMAND='git status 2>&1 | grep "On branch" | grep -v master'
