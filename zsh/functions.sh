function git-stash-recent-commits($n) {
  for ($i = 0; $i < $n, $i++) {
    export git_last_stash_name = git log --format=%B -n 1 HEAD | head -1 | sed -e 's/ /_/g';
    git revert HEAD~1
    git stash save $git_last_stash_name
  }
}
