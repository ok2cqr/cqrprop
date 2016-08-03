CC=lazbuild
ST=strip
datadir  = $(DESTDIR)/usr/share/cqrprop
bindir   = $(DESTDIR)/usr/bin
sharedir = $(DESTDIR)/usr/share
tmpdir   = /tmp

cqrprop: src/cqrprop.lpi
	$(CC) --ws=gtk2 --pcp=$(tmpdir)/.lazarus src/cqrprop.lpi
	$(ST) src/cqrprop
	gzip tools/cqrprop.1 -c > tools/cqrprop.1.gz

clean:
	rm -f -v src/*.o src/*.ppu src/*.bak src/cqrprop src/*.compiled src/*.or src/*.lrs src/*.a src/*.rst src/*.rsj src/*.lrt src/*.lps src/*.res
	rm -rf src/synapse/backup
	rm -rf src/backup
	rm -rf src/lib
	rm -f -v tools/cqrprop.1.gz

install:
	install -d -v         $(bindir)
	install -d -v         $(sharedir)/pixmaps/cqrprop
	install -d -v         $(sharedir)/icons
	install -d -v         $(sharedir)/applications
	install -d -v         $(sharedir)/man/man1
	install    -v -m 0755 src/cqrprop $(bindir)
	install    -v -m 0644 tools/cqrprop.1.gz $(sharedir)/man/man1/cqrprop.1.gz
	install    -v -m 0644 tools/cqrprop.desktop $(sharedir)/applications/cqrprop.desktop
	install    -v -m 0644 images/cqrprop.png $(sharedir)/pixmaps/cqrprop/cqrprop.png
	install    -v -m 0644 images/cqrprop.png $(sharedir)/icons/cqrprop.png 

