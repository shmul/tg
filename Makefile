PKG=re2,netstring,taglib
test:
	corebuild -quiet -use-ocamlfind -pkg $(PKG) -pkg alcotest tests.byte

tg:
	corebuild -quiet -use-ocamlfind -pkg $(PKG) tg.byte
