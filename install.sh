#install script
# reference : http://tellme.tokyo/post/2015/07/16/dotfiles/

DOTPATH=~/dotfiles

# git が使えるなら git
if has "git"; then
    git clone --recursive "$GITHUB_URL" "$DOTPATH"

# 使えない場合は curl か wget を使用する
elif has "curl" || has "wget"; then
    tarball="https://github.com/AriYu/dotfiles/archive/master.tar.gz"
    
    # どっちかでダウンロードして，tar に流す
    if has "curl"; then
        curl -L "$tarball"

    elif has "wget"; then
        wget -O - "$tarball"

    fi | tar xv -
    
    # 解凍したら，DOTPATH に置く
    mv -f dotfiles-master "$DOTPATH"

else
    die "curl or wget required"
fi

./deploy.sh 

