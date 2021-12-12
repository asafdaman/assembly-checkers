# Assembly Checkers
This project is a checkers game, written entirely in 80x86 Assembly.
This is the final project I made in 10th grade Cyber class.
## How to run
Inside DOSBox (required), run the following commands:
1. Mount C: <path_to_your_project>
2. C:
3. cycles = max  # recommended
4. tasm /zi checkers.asm
5. tlink /v checkers.obj
6. checkers
7. (optional - for debug) td checkers