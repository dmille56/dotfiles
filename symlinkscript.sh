#!/bin/bash

for file in $(find . -not -path "./.git*" -not -path "./symlinkscript.sh" -not -path ".")
do
	trimmed_file=$(echo "$file" | cut -c 3-)
	if [ -f "$trimmed_file" ]; then
		#symlink the file
		ln -f $trimmed_file ~/$trimmed_file
	else
		#make a directory for the directory structure
		mkdir ~/$trimmed_file
	fi
done
