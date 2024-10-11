
$(HOME)/.local/bin/cds: cds
	mv $< $(HOME)/.local/bin

cds: cds.hs
	ghc -no-keep-hi-files -no-keep-o-files $^

clean:
	$(RM) cds
