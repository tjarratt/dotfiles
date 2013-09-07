require 'find'

DOTFILES = ['emacs.d', 'zsh']
DOTFILES_DIRS = DOTFILES.map { |d| ENV['HOME'] + '/.' + d}

DOTFILES.each do |dir|
  path_to_dir = ENV['HOME'] + '/.' + dir
  task dir => path_to_dir

  desc "creates #{path_to_dir} if necessary"
  task path_to_dir do
    exists = File.exists?(path_to_dir)
    File.unlink(path_to_dir) if exists && !File.directory?(path_to_dir)
    Dir.mkdir path_to_dir unless exists
  end
end

task :default => DOTFILES
task :default => '~/.zshrc'

desc '~/.emacs.d'
task 'emacs.d' do
  sh "cp -r ./emacs.d/* #{ENV['HOME']}/.emacs.d/"
end

desc '~/.zsh'
task '.zsh' do
  sh "cp ./zsh/* #{ENV['HOME']}/.zsh"
end

desc '~/.zshrc'
task '~/.zshrc' do
  sh "cp .zshrc #{ENV['HOME']}/.zshrc"
end
