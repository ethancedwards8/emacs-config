#!/usr/bin/env bash

# source .profile for env vars
. ~/.profile

# screen locking on suspend, should probably use this everywhere
xss-lock -- slock &
