#!/bin/bash
pushd ./src/backend || exit

stack build

popd || exit