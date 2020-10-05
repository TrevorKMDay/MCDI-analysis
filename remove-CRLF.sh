#!/bin/bash

# Find everyone with CRLF, and remove the notes it generates
files=$(find . -not -type d -exec file "{}" ";" | \
			grep CRLF | \
			sed 's/:.*//')

# Loop over and fix everyone
for f in ${files} ; do
	dos2unix ${f}
done

# Print 'Done' so user knows nothing was found if returns nothing
echo "Done."