Emacs for Java
==============


Required Setup
--------------

### Install eclipse into the subdir eclipse. 

Do NOT use the installer, instead use a specific package from http://www.eclipse.org/downloads/eclipse-packages/

$ wget <eclipse_mirror>/eclipse-<version>-linux-gtk.tar.gz
$ tar -zxf eclipse-<version>-linux-gtk.tar.gz

### install Eclim

http://eclim.org/install.html

    chmod +x eclim_2.7.1.bin
    ./eclim_2.7.1.bin # as USER, not root!
    # eclipse path: /path/to/here/eclipse/java-oxygen/eclipse/

Customize the variable ´eclim-java-documentation-root´

Usage
-----

Simply use

    ./run-emacs

Just open any java file in a project you already setup in eclipse.

If you stumble over any errors, please tell me (Arne Babenhauserheide).
