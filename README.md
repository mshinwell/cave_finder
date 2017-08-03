One-off setup:

1. Install OPAM (opam.ocaml.org)
2. opam init
3. eval $(opam config env)
4. opam switch 4.05.0
5. opam install jbuilder

Then install the normal GDAL command-line tools (www.gdal.org).

To build:

1. eval $(opam config env)
2. jbuilder build cave_finder.exe
3. ./_build/default/cave_finder.exe als_dgm_1m.txt ents.pos als

als_dgm_1m.txt is the data file from the Landesregierung for your
chosen area

ents.pos is the standard .pos file from fixedpts/ in the Loser dataset
(must be in UTM coordinates; if yours is out of date run the shell
script to update)

"als" is the prefix of the output Survex file to be created, in this
case als.svx.

als.svx can be included from all.svx or similar in the Loser dataset.
