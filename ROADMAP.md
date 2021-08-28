- Big projects
    - Modules
        - New form of terms: module(t)
        - New type form: Module(Δ, σ) where Δ is a context, σ a type
        - Special typing rules for modules that save the context from
          chained let expressions.
        - Load all files in standard lib directory and put them in a map
          of typechecked modules, make available to system robots
          e.g. cats etc.

- UI
    - Small
        - Add UI feature to look up robots by ID.
            - See their currently executing program?
        - Pause button and single-stepping
    - Big
        - Built-in module editor
        - Implement world zooming.
        - Improve handling of ticks.

- Language
    - Small
        - Add colors
            - New type of colors and color constants
            - Command to let a robot change its color
            - Command to let a robot change its appearance?
        - Add a version of turn that allows turning to any vector
        - Add pairs, use pair to reprsent vectors
        - Make a command to sense the ID of a nearby robot
        - Make a command to pick up another robot by ID
        - Add type annotations to the language.
    - Big
        - Fix pretty-printing
            - Print operators infix
            - Better indentation/layout etc.

- Game mechanics
    - Small
        - Allow smaller, finite worlds?

    - Big
        - Special seed robots to make harvested things regrow
        - Make world not writeable
        - Restrict programs based on installed devices etc.
        - Implement craftable items / devices
        - Give each robot its own inventory.  Add commands for giving/receiving.
        - Create world with biomes etc. using multiple noise sources

- Refactoring/technical debt
    - All V2 Int values should be x,y.  Only convert to row,column in
      the UI.
        - Start by making a newtype for V2 Int, then fix all the type
          errors and think carefully about it.
        - Fix Turn values, e.g. north, south etc.
    - Update world implementations with newtypes to represent indices.
    - Redo using a fast effects library?
