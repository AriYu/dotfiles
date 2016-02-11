# my .emacs.d

## require
emacs 24.4 or later

By default on Ubuntu 14.04, emacs24.3 will be install via apt.  
So, you should install using follow url.  
http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/  
And, finally, you do not use `sudo make install`.  
Instead of `sudo make isntall`, you use `sudo checkinstall`.  
If you dont install `checkinstall`,  
```bash
sudo apt-get install checkinstall
```

## preparation

```bash
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
cd ~/.emacs.d
cask upgrade
cask install
```

## How to use
```
cd ~/.emacs.d
EMACS="emacs-24.4" cask install
```