% title: Swarm Markdown
% author: Brent Yorgey

The documents in this help system, entity descriptions, scenario
descriptions, *etc.* are all written in
[Markdown](https://daringfireball.net/projects/markdown/), with a few
Swarm-specific extensions:

- Things in backticks can be tagged by putting `{=tag}`{=snippet} after the
  closing backtick.  A few special tags are recognized:
    - `{=entity}`{=snippet} tags names of entities, like this: `tree`{=entity}.
    - `{=type}`{=snippet} tags types, like this: `Unit + Int`{=type}.
    - Anything in backticks with no tags is assumed to be Swarm
      language code, and will be parsed and type checked, like so: `(\x. x + 2) 3`.
    - The `{=snippet}`{=snippet} tag can be used to mark raw code or verbatim
      text that should not be parsed + type checked: `3 + "hi"`{=snippet}.
