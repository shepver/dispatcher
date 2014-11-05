#!/bin/bash

erl -sname dispatcher -pa ./ebin -s dispatcher start -s appmon start
