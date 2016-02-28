PKG=re2,netstring,taglib
tg:
	corebuild -quiet -use-ocamlfind -pkg $(PKG) tg.byte

test:
	corebuild -quiet -use-ocamlfind -pkg $(PKG) -pkg alcotest tests.byte
