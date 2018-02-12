# TODO: Find shallow why to clone submodules.
git clone https://github.com/ibayer/dotfiles.git --recursive -b stow

# Link to dotfile configs:
stow vim emacs config

# Remove config links again:
stow -D vim emacs config
