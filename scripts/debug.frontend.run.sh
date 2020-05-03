#!/bin/bash
pushd ./src/frontend || exit

rm -rf ./public
mkdir ./public

cp ./src/index.html ./public
cp -r ./assets ./public/assets

elm-live ./src/Main.elm -d ./public -- --output=./public/index.js

popd || exit
