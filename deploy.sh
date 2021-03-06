# deploy script
# reference : http://tellme.tokyo/post/2015/07/16/dotfiles/

DOTPATH=~/dotfiles

cd ~/dotfiles
if [ $? -ne 0 ]; then
    die "not found: $DOTPATH"
fi

# 移動できたらリンクを実行する
for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME/$f"
done
