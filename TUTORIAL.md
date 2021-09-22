This is a brief tutorial that should hopefully get you up and running
with Swarm.  If you find any mistakes, or things that are confusing,
or ways that it no longer corresponds to the game, please [file a bug
report](https://github.com/byorgey/swarm/issues/new/choose)!
Eventually, this tutorial file should be [replaced by an in-game
tutorial](https://github.com/byorgey/swarm/issues/25).

The backstory
-------------

In a shockingly original turn of events, you have crash landed on an
alien planet!  ~~Your only hope is to~~ Scratch that, you have no hope
at the moment, but since you're here, you might as well explore a bit.
Your sensors indicate that the atmosphere is highly toxic, so you'll
have to stay inside your robotic base, with its built-in life support
system.  However, you are stocked with all the materials you need to
build a lot of robots to explore for you!  To start, you only have the
materials to make some very basic devices which give your robots
abilities like moving, turning, and grabbing things and interpreting
very simple imperative programs.  As you use your robots to gather
resources, you will be able to construct better devices which in turn
allow you to construct robots with upgraded abilities and programming
language features.

Getting started
---------------

When you first start up Swarm, you should be greeted by a screen that
looks like this:

![](images/initial.png)

The little white square in the middle represents your base.  Start by
using the Tab key to cycle through the three panels (the REPL, the
info panel, and the world panel), and read about the various devices
installed on your base.

Building your first robot
-------------------------

Pretty much the only thing you can do is build robots.  Let's build
one!  Tab back to the REPL and type
```
build "hello" {move; move; move; move}
```
then hit Enter.  You should see a robot appear and travel to the
east four steps before stopping.  It should look something like this:

![](images/firstrobot.png)

You can also see that on the next line after your input, the REPL printed out
```
"hello" : string
```
which is the result of your command, along with its type.  The `build` command
always returns a string which is the name of the robot that was built;
it may be different than the name you specified if there is already
another robot with that name.

Note that if you don't want a robot to hang around after completing
its job, you can add the `selfdestruct` command to the end of its
program.  Try building a robot that moves a few steps and then
self-destructs.

You can see that a semicolon is used to chain together commands, that
is, if `c1` and `c2` are both commands, then `c1 ; c2` is the command
which executes first `c1` and then `c2`.  The curly braces look fancy
but really they are the same as parentheses (parentheses would work
here too). Ultimately, the `build` command is just a function that takes two
arguments: a string, and a command. (Quiz: if we left out the curly
braces around the sequence of `move` commands, as in `build "hello"
move;move;move;move`, what do you think would happen?  Hint: function
application has higher precedence than semicolon.  Try it and see!)

We can actually see the type of the `build` command by just typing
`build` by itself at the prompt.  It should return something like
```
build : forall a0. string -> cmd a0 -> cmd string
```
which says that `build` takes two arguments---a `string`, and a command
that returns a value of any type---and results in a command which
returns a `string`.  Every command returns a value, though some might
return a value of the unit type, written `()`.

Something you can't do yet
--------------------------

Try entering the following at the REPL:
```
build "nope" {let m : cmd () = move in m;m;m}
```
You should get an error saying something like
```
base: build: this would require installing devices you don't have:
  dictionary
```
This is telling you that in order to `build` a robot which has the right
capabilities to run this program, you would need to
install a `dictionary` device on the robot, but you don't have a
`dictionary` in your inventory.  (You do have a `dictionary` device
*installed* in your base robot, but you can't rip it out and put it in
another robot.  You'll have to find a way to make more.)

Creating definitions
--------------------

We can already tell it's going to be tedious typing
`move;move;move;move;...`, so let's create some definitions to make
our lives a bit easier.  To start, type the following:
```
def m2 : cmd () = {move ; move} end
```

The `cmd ()` annotation is optional; in this situation the game could
have easily figured out the type of `m2` if we had just written `def
m2 = ...`.  The curly braces are actually optional as well.  The `end`
is required, and is needed to disambiguate where the end of the
command is.  It may not seem very ambiguous in this situation, but is
needed especially when several definitions are written in sequence
(such as in a file full of definitions).

Now try this:
```
def m4 = m2;m2 end; def m8 = m4;m4 end
```

Great, now we have commands that will execute `move` four and eight
times, respectively.  Finally, let's use them:
```
build "mover" {m8; m8; m2}
```
This should build a robot that moves eighteen steps to the east.

(You might wonder at this point if it is possible to create a function
that takes a number as input and moves that many steps forward.  It
certainly is possible, but right now your robots would not be capable
of executing it.  You'll have to figure out how to upgrade them!)

Getting some resources
----------------------

Let's get a tree!  Enter the following:
