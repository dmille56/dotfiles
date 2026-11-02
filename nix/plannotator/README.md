# Queued Plannotator reviews on castle

All desktop browser reviews started through `plannotator-agent` share one lock
and one URL: **https://castle.tailb6562d.ts.net:8443**. Tailscale Serve forwards
that URL to Plannotator on port **19432**. No additional ports or provisioning
services are needed.

When a review is active, other requests print a queue message and wait. Submitting
feedback or terminating the active review lets the next waiting request start.
Each request returns its own feedback to its originating agent. Keep commands
running while queued; canceling a waiting command removes that request.

The queue works across agents, repositories, and tmux panes for the same user.
Its persistent lock file lives beside the global mode setting at
`~/.config/plannotator/browser.lock`. The server inherits the lock so forcibly
killing its launcher does not allow another review to take over its port.
Do not delete the lock file while reviews are running or queued.

When the server is ready, it opens the HTTPS URL on castle if a desktop display
is available. Headless sessions print the link. `PLANNOTATOR_BROWSER=none` and
`PLANNOTATOR_SKIP_BROWSER_OPEN=1` suppress browser opening. A finished review tab
does not automatically become the next review; use the newly opened tab or
reload the fixed URL after the next review starts.

This covers plan hooks, review skills, annotation, last-response review, and
browser fallbacks. TUI reviews retain their existing tmux queue. Laptop behavior
and bare upstream `plannotator` are unchanged. Avoid running bare Plannotator
on port 19432 while using the managed queue, since it does not acquire the lock.

## Activation

Apply Home Manager through your normal manual desktop rebuild and restart agents
to load the new generated launcher paths. The hostname and ports are defined in
`tailscale-settings.nix`.

The existing Serve mapping is reused. If it ever needs to be established again,
run the existing `plannotator-serve` shell alias. Inspect it with
`tailscale serve status`. Review startup never runs privileged Tailscale commands.

The earlier multi-port implementation was not activated during development. If
you activated it separately, disable its old provisioning service and remove only
the extra HTTPS mappings (8444–8452) with `sudo tailscale serve --https=PORT off`.
Keep the 8443 mapping and unrelated services such as port 443.

## Checks

```sh
PYTHONDONTWRITEBYTECODE=1 python3 nix/plannotator/test-plannotator-browser.py
PYTHONDONTWRITEBYTECODE=1 python3 nix/plannotator/test-plannotator-agent.py
```

Set `PLANNOTATOR_BIN` to the installed Plannotator executable to include a real
server test. It uses temporary state and port 29432, checks sequential reviews
and feedback routing, and does not change Tailscale settings or open a browser.
