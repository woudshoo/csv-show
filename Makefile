info-doc: csv-lens.info dir
tar: csv-lens-0.8.tar

csv-lens.info: doc/csv-lens.texi
	makeinfo doc/csv-lens.texi -o csv-lens.info


dir: csv-lens.info
	install-info csv-lens.info dir

csv-lens-0.8.tar: dir csv-lens.info *.el
	-mkdir csv-lens-0.8
	cp $^ csv-lens-0.8
	tar -cvf csv-lens-0.8.tar csv-lens-0.8
	rm csv-lens-0.8/*
	rmdir csv-lens-0.8

