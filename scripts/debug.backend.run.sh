#!/bin/bash
pushd ./src/backend || exit

stack exec backend-exe

popd || exit