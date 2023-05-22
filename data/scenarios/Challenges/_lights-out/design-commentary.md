# Puzzle generation

Solvability for a puzzle of given dimensions can be determined by deriving the "quiet patterns" via linear algebra and ensuring that only an even number of "on" lights fall upon each quiet pattern.

For a 5x5 board, the pre-derived quiet patterns from here are used:
https://www.jaapsch.net/puzzles/lights.htm#quiet

If a randomly-generated light sequence is at first not solvable, it can be made so by toggling the appropriate lights to achieve even parity with the quiet patterns.

See also:
* https://www.jaapsch.net/puzzles/lomath.htm#solvtest
* https://www.xarg.org/2018/07/lightsout-solution-using-linear-algebra/
* https://web.archive.org/web/20100704161251/http://www.haar.clara.co.uk/Lights/solving.html
* https://en.wikipedia.org/wiki/Lights_Out_(game)#Light_chasing