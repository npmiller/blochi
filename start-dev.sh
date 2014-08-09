#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname blochi_dev \
    -s blochi \
    -s reloader
