CARGO = cargo

INSTALL = /usr/local/bin/install -c
INSTALL_PROGRAM = $(INSTALL)

prefix = /usr/local
exec_prefix = $(prefix)
bin_dir = $(exex_prefix)/bin

$(TARGET): mods
	$(CARGO) build -r

clean:
	$(CARGO) clean

run:
	$(CARGO) run

check:
	$(CARGO) check

install: $(TARGET)
	$(INSTALL) $(TARGET) $(DESTDIR)$(bin_dir)/$(TARGET)
