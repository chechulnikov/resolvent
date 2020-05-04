#!/bin/bash
pushd ./src/frontend || exit

rm -rf ./public
mkdir ./public

cp -r ./src/native/ ./public/
cp -r ./assets ./public/assets

elm-live ./src/Main.elm -d ./public -- --output=./public/index.js

popd || exit
