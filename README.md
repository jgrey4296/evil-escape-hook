# evil-escape

This is my experimental rewrite of evil-escape.
It adds a `post-command-hook` function, which calls `evil-escape-post-command-hook-plus`,
which runs any functions added to `evil-escape-hook`.

Also, I refactored `evil-escape-pre-command-hook` to be a lot simpler.
It doesn't have unordered sequences now, but also doesn't read events then add them back to the unprocessed list.

This would be ideal to rewrite (again) as an external c/rust module to make it faster.
As it is, I'm using a ring, which might not be the best, but its simple.
