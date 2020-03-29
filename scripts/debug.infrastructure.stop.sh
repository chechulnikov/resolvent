#!/bin/bash
docker-compose -p resolvent-infra -f ./build/debug.infrastructure.docker-compose.yaml kill
docker-compose -p resolvent-infra -f ./build/debug.infrastructure.docker-compose.yaml rm -f