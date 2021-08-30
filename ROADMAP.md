- Big projects
    - Modules
        - New type form: Module(Δ, σ) where Δ is a context, σ a type
        - Special function inferModule (⊢im below) that takes a term and returns a
          Module type:

                       Γ ⊢im t₂ : Module(Δ ; σ)
            ———————————————————————————————————————————————–
            Γ ⊢im let x : τ = t₁ in t₂ : Module(Δ, x : τ ; σ)

                  Γ ⊢ t : σ
            ————————————————————–  (t is not a let expression)
            Γ ⊢im t : Module(∅; σ)

        - Then a new term form,  import m in t, with typing rule

              Γ ⊢ m : Module(Δ, σ)        Γ, Δ ⊢ t : τ
              ————————————————————————————————————————
                        Γ ⊢ import m in t : τ

          Typically m will be a variable referencing some module in
          the global context, not a literal module (at that point
          might as well just use a let expression).

        - Load all files in a directory indicated as a command-line
          argument, make them available as modules with the same name
          as the files.
        - Eventually there may also be a way to edit modules in an
          editor directly inside the game, which could also be saved
          and reloaded along with the world.

- UI
    - Small
        - Show information about the robot being currently viewed.
            - Inventory
            - Location, direction
            - See their currently executing program somehow?
        - Pause button and single-stepping
    - Big
        - Built-in module editor
        - Implement world zooming.
        - Improve handling of ticks.

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
        - Add pairs, use pair to reprsent vectors
        - Make a command to sense the ID of a nearby robot
        - Make a command to pick up another robot by ID
        - Add type annotations to the language.
    - Big
        - Fix pretty-printing
            - Print operators infix
            - Better indentation/layout etc.
        - Add polymorphism!
            - Makes typechecking certain primitives (e.g. if) simpler,
              can just infer them instead of having to resort to
              checking mode
            - This is a good example of why we might like
              polymorphism:

              let rep : int -> cmd () -> cmd () =
                \n.\c.
                  if (n == 0)
                    {}
                    {c ; rep (n-1) c}
              in
              rep 4 {
                rep 10 move;
                turn left;
                build "sq" (run("square.sw"))
              }

              The above actually does not type check, because the 'build'
              command returns a string, but 'rep' expects a cmd (),
              not a cmd string.  But the return type really doesn't
              matter. We'd like to say e.g.  rep : int -> cmd a -> cmd ().

- Game mechanics
    - Small
        - Allow smaller, finite worlds?
        - Base sequence of random values from the 'random' command
          deterministically on the world seed?  Then IO is not required
          and also it will pave the way for networked play (clients
          will stay in sync without having to communicate their
          choices of random values). =D

    - Big
        - Disallow harvesting while a seed robot is present
          (i.e. patch the "infinite tree glitch" =)
        - Load seed program from a file in data dir!
        - Make world not writeable
        - Restrict programs based on installed devices etc.
        - Implement craftable items / devices
        - Give each robot its own inventory.  Add commands for giving/receiving.
        - Create world with biomes etc. using multiple noise sources
        - Implement a challenge mode
            - A challenge consists of
                - A world
                - Initial inventory etc.
                - An unrestricted global program (of type cmd bool)
                  that runs once every tick to determine whether the
                  challenge has been completed.

- Refactoring/technical debt
    - All V2 Int values should be x,y.  Only convert to row,column in
      the UI.
        - Start by making a newtype for V2 Int, then fix all the type
          errors and think carefully about it.
        - Fix Turn values, e.g. north, south etc.
    - Update world implementations with newtypes to represent indices.
    - Redo using a fast effects library?
    - Make a test suite
    - Collect some example Swarm programs
