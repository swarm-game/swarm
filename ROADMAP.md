- Big projects

- UI
    - Small
        - Intro screen where you can choose a mode, start a new game,
          see the tutorial/help/about/contact, etc.
    - Big
        - Visualize robot's currently executing program
        - Built-in module editor
        - Implement world zooming.
        - Implement saving/loading.

- Language
    - Small
        - Bug: let forever = \c:cmd(). c; forever c in forever move
          says "unbound variable forever".  The problem is that we
          actually can't infer the type of actually recursive lets!
          Just need a better error message, require type annotation
          for recursive let.

        - Add colors
            - New type of colors and color constants
            - Command to let a robot change its color
        - Add a version of turn that allows turning to any vector
        - Make a command to sense the ID of a nearby robot
        - Make a command to pick up another robot by ID
        - Add type annotations to the language.
    - Big
        - Fix pretty-printing
            - Print operators infix
            - Better indentation/layout etc.

        - Load all files in a directory indicated as a command-line
          argument, make them available as modules with the same name
          as the files.
        - Eventually there may also be a way to edit modules in an
          editor directly inside the game, which could also be saved
          and reloaded along with the world.

- Game mechanics
    - Small
        - Allow smaller, finite worlds?
        - Base sequence of random values from the 'random' command
          deterministically on the world seed?  Then IO is not required
          and also it will pave the way for networked play (clients
          will stay in sync without having to communicate their
          choices of random values). =D

    - Big
        - Load seed program from a file in data dir!
        - Implement a challenge mode
            - A challenge consists of
                - A world
                - Initial inventory etc.
                - An unrestricted global program (of type cmd bool)
                  that runs once every tick to determine whether the
                  challenge has been completed.

- Refactoring/technical debt
    - Split up Game module into multiple modules
    - Update world implementations with newtypes to represent indices.
    - Redo using a fast effects library?
    - Make a test suite
