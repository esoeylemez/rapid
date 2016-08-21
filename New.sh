#! /usr/bin/env zsh

setopt err_exit

for dir in $*; do
    name="`basename $dir`"
    git clone ~/src/Skel "$dir"
    (
        cd "$dir"
        git checkout -b master
        sed -i -e "s/SKELETON/$name/g" \
            README.md NOTICE Setup.lhs skeleton.cabal
        git mv skeleton.cabal "$name.cabal"
        git mv programs/skeleton.hs "programs/$name.hs"
        rm New.sh
        git commit -am "Project initialised."
    )
done
