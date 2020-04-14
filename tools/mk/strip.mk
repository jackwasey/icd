#!/usr/bin/env make
#Enable using configure --enable-icd-strip
#With binary stripping, size is about 10% on Linux, 90% on Mac. CRAN Windows DLL is already small.
strippedLib: $(SHLIB)
	# so clumsy, but unambiguous, avoids weird multi level make vs bash escaping
	command -v strip >/dev/null && \
		command -v uname >/dev/null && \
		[ x$(uname -s) = xLinux ] && strip -S $(SHLIB) || true
	command -v strip >/dev/null && \
		command -v uname >/dev/null && \
		[ x$(uname -s) = xDarwin ] && strip -S $(SHLIB) || true
.phony: strippedLib
