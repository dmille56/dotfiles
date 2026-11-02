"""Open a review through the desktop's existing Tailscale Serve endpoint."""

import os
import subprocess
import sys
from urllib.parse import urlsplit, urlunsplit


def review_url(base, local):
    external, original = urlsplit(base), urlsplit(local)
    return urlunsplit((external.scheme, external.netloc, original.path,
                       original.query, original.fragment))


def main(args):
    url = review_url(*args)
    print(f"Plannotator Tailscale review: {url}", file=sys.stderr, flush=True)
    if os.environ.get("DISPLAY") or os.environ.get("WAYLAND_DISPLAY"):
        try:
            subprocess.Popen(["xdg-open", url], stdin=subprocess.DEVNULL,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                             start_new_session=True)
        except OSError as error:
            print(f"Could not open the browser: {error}. Open the HTTPS link above.", file=sys.stderr)
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
