#!/bin/bash

make () {
    (cd src/$1; elm make Main.elm  --optimize --output=../../dist/$1.js)
}

make pythagoras
make dandelin
make mobius
make cantor
make viviani
make asoi
