install-sbcl:
	asdf plugin-add sbcl
	asdf install sbcl latest

install-emacs:
	echo "(load-file \"$$PWD/emacs/init.el\")" > ~/.emacs

install-shell:
	ln -sf /dias/dotfiles/emacs/init.el ~/.emacs
	ln -sf /dias/dotfiles/profile ~/.profile
	ln -sf /dias/dotfiles/xinitrc ~/.xinitrc
	ln -sf /dias/dotfiles/xmodmap ~/.xmodmap

install-asdf:
	git clone https://github.com/asdf-vm/asdf ./asdf
	ln -sdf ./asdf ~/.asdf

install-system: install-shell install-asdf

install-all: install-system install-emacs
