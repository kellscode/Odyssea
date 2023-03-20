
![Odyssea](/img/logo.png "Odyssea")
=========

Odyssea is an arcade style survival game where you play as a ship navigating through a endless sea of dangerous obstacles. Dodge and weave through the icebergs, collect power-ups, and avoid the pesky shark to accumulate the highest score you can!

![Gameplay](/img/screenshot.png "Odyssea gameplay screenshot")

Odyssea was built by Kelly Lee (yl3267), Jeffrey Zhou (jlz45), Spencer Dunn (smd326) and Nicole Ding (nd347) in Spring 2022 for our CS 3110 final project. 

Installation
-----------
**First, if you have not already, follow CS 3110's ocaml and opam [installation instructions](https://cs3110.github.io/textbook/chapters/preface/install.html) for the OS you are running.**

### Mac

First, ensure that you are using Homebrew and not MacPorts. Then, clone the repository by running 
```
git clone https://github.coecis.cornell.edu/nd347/CS3110FinalProject/
```

Then run
```
opam depext raylib
opam install raylib raygui ansiterminal
```
to install all dependencies.

### Windows (CS3110 Ubuntu Virual Machine)

First clone the repository and install dependencies is the same way as for Mac. You will then need to install an XServer for graphics:
1. Download and install [VcXsrv](https://sourceforge.net/projects/vcxsrv/).
2. Run VcXsrv, and ensure that you disable access control.
3. Open your terminal and add the following line to your *~/.bashrc* file:
   ```
   export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
   ```

4. Restart your terminal.

Gameplay
------
The first time you run Odyssea, you will need to run `make play`, which will compile and launch the game. Anytime after, you can run `make start` to skip recompiling and just launch the game. You can find a tutorial in the main menu of the game.

The file *bin/scores.txt* contains all of our scores that we have logged while creating and playtesting Odyssea. If you wish to remove our scores, you can clear its contents.
