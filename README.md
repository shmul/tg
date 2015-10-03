# tg

A (personal) utility to assist in tagging audio files. It is personal in the sense that no attempt was made to make it general purpose - rather it was written to adhere to my peculiar liking. Moreover, it was written as a way to learn and experiment with OCaml, a process which was rather satisfying, though the result is surely not something to be particularly proud of (just an elementary level of OCaml know-how is displayed in the code).

It is released in the hope that it will be valuable to someone and for back up purposes. I do hope it will not greatly embarrass me in the future in horrible unforeseen ways.

## Usage
There are two commands, the more useful one is ```set``` which can be used to edit tags and the other one is ```guess``` which is used more for debugging purposes. Sparse documentation is available through the ```-h``` switch for each of the commands.

When using ```set``` the tool will try to guess as many of the fields as possible, and will present the choices to the user. The user choices for the first file will then be applied to all the rest of the files. This actually makes a lot of sense, since user choices can be *symbolic*. That is if the choice (when presented with the first file) is to use the track number as deduced from the file base name, the process will be separately applied to all the following files. Try it.

Patterns can be used to decompose a field, i.e. "Grateful Dead - Dave's Picks 22 - D1T3 - Sugaree" -> "%a - %b - D%dT%n - %t". The supported indicators are (use capitalize for non-greedy captures):
```
* %a - Artist
* %b - Album
* %d - Disc
* %i - ignored
* %n - Track
* %t - Title
```
Patterns in the input line are recognized by any ```%``` occurrence. They will operate on the default choice, or on a *symbolic* choice, i.e. ```*N<pattern>```, where N is from the list of presented options.

## Dependencies
The following were used through the wonders of ```OPAM```
* ```core```
* ```alcotest```
* ```taglib```
* ``ocamlnet``

## Tests
I only used these initially and then stopped enhancing them nor even making sure they properly run (which they don't!). Sorry. Someday I might fix it.

## License
This project is licensed under the terms of the MIT license.

## TODO
* automatically scan text files in a directory and try to identify track names.
* identify common prefix/suffix in track names and file names. Then use only the variable parts for guessing disc, track, title
