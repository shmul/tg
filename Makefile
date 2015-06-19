test:
	corebuild -quiet -use-ocamlfind -pkg re2 -pkg alcotest -pkg netstring tests.byte

tg:
	corebuild -quiet -use-ocamlfind -pkg re2,netstring tg.byte
