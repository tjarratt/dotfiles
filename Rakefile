require 'find'

DOTFILES = ['emacs.d'].map { |d| ENV['HOME'] + '/.' + d}
task :default => [*DOTFILES, '~/.zshrc']

desc '~/.emacs.d'
task 'emacs.d' do
  sh "cp -r ./emacs.d/* #{ENV['HOME']}/.emacs.d/"
end

desc '~/.zsh'
task '.zsh' do
  sh "cp ./zsh/* #{ENV['HOME']}/.zsh"
end
