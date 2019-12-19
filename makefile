install-shell:
	ln -sf profile ~/.profile
	ln -sf xinitrc ~/.xinitrc
	ln -sf xmodmap ~/.xmodmap

install-asdf:
	git clone https://github.com/asdf-vm/asdf ./asdf
	ln -sdf ./asdf ~/.asdf

install-all: install-shell install-asdf
