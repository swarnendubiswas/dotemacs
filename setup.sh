# Helper script to install GNU Emacs if not already present, install

#!/bin/bash

# https://askubuntu.com/a/30157/8698
if ! [ $(id -u) = 0 ]; then
   echo "The script need to be run as root." >&2
   exit 1
fi

cd $HOME
mkdir -p github; cd github

git clone https://github.com/swarnendubiswas/dotemacs.git
git clone https://github.com/swarnendubiswas/dotfiles.git

rm -rf .emacs.d
ln -s $HOME/github/dotemacs .emacs.d

# Install distribution packages
sudo apt install aspell global exuberant-ctags libxml2-utils chktex shellcheck ruby-dev tidy python-pygments python-pip python3-pip npm clang-format imagemagick

# Install Python packages
python -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi pylint rope python-language-server[all] pycodestyle flake8 autopep8 importmagic --user
python3 -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi pylint rope python-language-server[all] pycodestyle flake8 autopep8 importmagic --user

# Install Node packages
sudo npm i -g npm eslint js-yaml less jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin
sudo npm i -g --unsafe-perm bash-language-server
sudo npm i -g stylelint --save-dev

# Install miscellaneous packages
sudo gem install scss_lint mdl
