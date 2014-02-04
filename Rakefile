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
task :default => '~/.vimrc'
task :default => '~/.gitconfig'
task :default => '/usr/local/bin'

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

desc '~/.vimrc'
task '~/.vimrc' do
  sh "cp .vimrc #{ENV['HOME']}/.vimrc"
end

desc '~/.gitconfig'
task '~/.gitconfig' do
  sh "cp .gitconfig #{ENV['HOME']}/.gitconfig"
end

desc '/usr/local/bin'
task '/usr/local/bin' do
  Dir.glob(File.expand_path("__FILE__/../bin/*")).each do |file|
    sh "cp #{file} /usr/local/bin"
  end
end
