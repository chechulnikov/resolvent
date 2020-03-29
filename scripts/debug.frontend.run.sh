#!/bin/bash
pushd ./src || exit
elm reactor
popd || exit
