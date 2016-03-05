PKG=re2,netstring,taglib,alcotest

tg:
	corebuild -quiet -use-ocamlfind -pkg $(PKG) tg.byte
