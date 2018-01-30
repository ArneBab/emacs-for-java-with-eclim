Emacs for Java
==============


Required Setup
--------------

### Install eclipse into the subdir eclipse. 

Do NOT use the installer, instead use a specific package from http://www.eclipse.org/downloads/eclipse-packages/

    $ wget <eclipse_mirror>/eclipse-<version>-linux-gtk.tar.gz
    $ tar -zxf eclipse-*linux-gtk*.tar.gz

### install Eclim

http://eclim.org/install.html

    chmod +x eclim_2.7.1.bin
    ./eclim_2.7.1.bin # as USER, not root!
    # eclipse path: /path/to/eclipse/java-oxygen/eclipse/
	#   i.e. from $ realpath eclipse
	ln -s /path/to/eclipse/eclimd ~/.local/bin/eclimd
	ln -s /path/to/eclipse/plugins/*eclim*/bin/eclim ~/.local/bin/eclim

Customize the variables `eclim-java-documentation-root` and `eclim-default-workspace` (with `M-x customize-variable`)

Usage
-----

Simply use

    ./run-emacs

Just open any java file in a project you already setup in eclipse.

If you stumble over any errors, please tell me (Arne Babenhauserheide).
