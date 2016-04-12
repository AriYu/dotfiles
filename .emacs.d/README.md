# my .emacs.d

This .emacs.d assume you use `C++`, `Python`, `Latex`.
And, this is managed by Cask and Pallet.

## Require
emacs 24.4 or later

By default on Ubuntu 14.04, emacs24.3 will be install via apt.  
So, you should install using follow url.  
http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/  
And, finally, you do not use `sudo make install`.  
Instead of `sudo make isntall`, you use `sudo checkinstall`.  
If you have not installed `checkinstall`,  

```bash
sudo apt-get install checkinstall
```

## Dependencies
For markdown-preview-mode

```
sudo apt-get install markdown
```

## Preparation

```bash
sudo apt-get install python-pip
sudo apt-get install llvm-dev clang libclang-dev cmake
sudo pip install virtualenv
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
```

## How to use
```
cd ~/.emacs.d
EMACS="emacs-24.4" cask install
```

run emacs,

```
M-x jedi:install-server
M-x irony-install-server
```

## If you get error

### update pip
```
sudo pip install --upgrade pip
```

### remove old version Emacs
```
sudo apt-get remove emacs23
```

then, you shuld install emacs24.4 or higher

### Permission denied
```
sudo emacs
M-x jedi:install-server
```

